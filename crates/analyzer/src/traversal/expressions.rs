use super::borrowck;
use crate::builtins::{ContractTypeMethod, GlobalFunction, Intrinsic, ValueMethod};
use crate::context::{AnalyzerContext, CallType, Constant, ExpressionAttributes, NamedThing};
use crate::display::Displayable;
use crate::errors::{self, FatalError, IndexingError, TypeCoercionError};
use crate::namespace::items::{
    EnumVariantId, EnumVariantKind, FunctionId, FunctionSigId, ImplId, Item, StructId, TypeDef,
};
use crate::namespace::scopes::{check_visibility, BlockScopeType};
use crate::namespace::types::{
    self, Array, Base, FeString, Integer, TraitOrType, Tuple, Type, TypeDowncast, TypeId,
};
use crate::operations;
use crate::traversal::call_args::{validate_arg_count, validate_named_args};
use crate::traversal::const_expr::eval_expr;
use crate::traversal::types::{
    apply_generic_type_args, deref_type, try_cast_type, try_coerce_type,
};
use crate::traversal::utils::add_bin_operations_errors;

use fe_common::diagnostics::Label;
use fe_common::{numeric, Span};
use fe_parser::ast as fe;
use fe_parser::ast::GenericArg;
use fe_parser::node::Node;
use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero};
use smol_str::SmolStr;
use std::ops::RangeInclusive;
use std::str::FromStr;

// TODO: don't fail fatally if expected type is provided

pub fn expr_type(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
) -> Result<TypeId, FatalError> {
    expr(context, exp, None).map(|attr| attr.typ)
}

pub fn expect_expr_type(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected: TypeId,
    sink: bool,
) -> Result<ExpressionAttributes, FatalError> {
    let attr = expr(context, exp, Some(expected))?;

    match try_coerce_type(context, Some(exp), attr.typ, expected, sink) {
        Err(TypeCoercionError::RequiresToMem) => {
            context.add_diagnostic(errors::to_mem_error(exp.span));
        }
        Err(TypeCoercionError::Incompatible) => {
            context.type_error(
                "type mismatch",
                exp.span,
                expected.deref(context.db()),
                attr.typ.deref(context.db()),
            );
        }
        Err(TypeCoercionError::SelfContractType) => {
            context.add_diagnostic(errors::self_contract_type_error(
                exp.span,
                &attr.typ.display(context.db()),
            ));
        }
        Ok(_) => {}
    }
    Ok(attr)
}

pub fn value_expr_type(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected: Option<TypeId>,
) -> Result<TypeId, FatalError> {
    let attr = expr(context, exp, expected)?;
    Ok(deref_type(context, exp, attr.typ))
}

/// Gather context information for expressions and check for type errors.
pub fn expr(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let attr = match &exp.kind {
        fe::Expr::Name(_) => expr_name(context, exp, expected),
        fe::Expr::Path(_) => expr_path(context, exp, expected),
        fe::Expr::Num(_) => Ok(expr_num(context, exp, expected)),

        fe::Expr::Subscript { .. } => expr_subscript(context, exp, expected),
        fe::Expr::Attribute { .. } => expr_attribute(context, exp, expected),
        fe::Expr::Ternary { .. } => expr_ternary(context, exp, expected),
        fe::Expr::BoolOperation { .. } => expr_bool_operation(context, exp),
        fe::Expr::BinOperation { .. } => expr_bin_operation(context, exp, expected),
        fe::Expr::UnaryOperation { .. } => expr_unary_operation(context, exp, expected),
        fe::Expr::CompOperation { .. } => expr_comp_operation(context, exp),
        fe::Expr::Call {
            func,
            generic_args,
            args,
        } => expr_call(context, func, generic_args, args, expected),
        fe::Expr::List { elts } => expr_list(context, elts, expected),
        fe::Expr::Repeat { .. } => expr_repeat(context, exp, expected),
        fe::Expr::Tuple { .. } => expr_tuple(context, exp, expected),
        fe::Expr::Str(_) => expr_str(context, exp, expected),
        fe::Expr::Bool(_) => Ok(ExpressionAttributes::new(TypeId::bool(context.db()))),
        fe::Expr::Unit => Ok(ExpressionAttributes::new(TypeId::unit(context.db()))),
    }?;
    context.add_expression(exp, attr.clone());
    Ok(attr)
}

pub fn error_if_not_bool(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    msg: &str,
) -> Result<(), FatalError> {
    let bool_type = TypeId::bool(context.db());
    let attr = expr(context, exp, Some(bool_type))?;
    if try_coerce_type(context, Some(exp), attr.typ, bool_type, false).is_err() {
        context.type_error(msg, exp.span, bool_type, attr.typ);
    }
    Ok(())
}

fn expr_list(
    context: &mut dyn AnalyzerContext,
    elts: &[Node<fe::Expr>],
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let expected_inner = expected_type
        .and_then(|id| id.deref(context.db()).as_array(context.db()))
        .map(|arr| arr.inner);

    if elts.is_empty() {
        return Ok(ExpressionAttributes::new(context.db().intern_type(
            Type::Array(Array {
                size: 0,
                inner: expected_inner.unwrap_or_else(|| TypeId::unit(context.db())),
            }),
        )));
    }

    let inner_type = if let Some(inner) = expected_inner {
        for elt in elts {
            expect_expr_type(context, elt, inner, true)?;
        }
        inner
    } else {
        let first_attr = expr(context, &elts[0], None)?;

        // Assuming every element attribute should match the attribute of 0th element
        // of list.
        for elt in &elts[1..] {
            let element_attributes = expr(context, elt, Some(first_attr.typ))?;

            match try_coerce_type(
                context,
                Some(elt),
                element_attributes.typ,
                first_attr.typ,
                true,
            ) {
                Err(TypeCoercionError::RequiresToMem) => {
                    context.add_diagnostic(errors::to_mem_error(elt.span));
                }
                Err(TypeCoercionError::Incompatible) => {
                    context.fancy_error(
                        "array elements must have same type",
                        vec![
                            Label::primary(
                                elts[0].span,
                                format!("this has type `{}`", first_attr.typ.display(context.db())),
                            ),
                            Label::secondary(
                                elt.span,
                                format!(
                                    "this has type `{}`",
                                    element_attributes.typ.display(context.db())
                                ),
                            ),
                        ],
                        vec![],
                    );
                }
                Err(TypeCoercionError::SelfContractType) => {
                    context.add_diagnostic(errors::self_contract_type_error(
                        elt.span,
                        &first_attr.typ.display(context.db()),
                    ));
                }
                Ok(_) => {}
            }
        }
        first_attr.typ
    };

    Ok(ExpressionAttributes::new(
        Type::Array(Array {
            size: elts.len(),
            inner: inner_type,
        })
        .id(context.db()),
    ))
}

fn expr_repeat(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let (value, len) = match &exp.kind {
        fe::Expr::Repeat { value, len } => (value, len),
        _ => unreachable!(),
    };

    let expected_inner = expected_type
        .and_then(|id| id.deref(context.db()).as_array(context.db()))
        .map(|arr| arr.inner);

    let value = expr(context, value, expected_inner)?;

    let size = match &len.kind {
        GenericArg::Int(size) => Ok(size.kind),
        GenericArg::TypeDesc(_) => Err(context.fancy_error(
            "expected a constant u256 value",
            vec![Label::primary(len.span, "Array length")],
            vec!["Note: Array length must be a constant u256".to_string()],
        )),
        GenericArg::ConstExpr(exp) => {
            expr(context, exp, None)?;
            if let Constant::Int(len) = eval_expr(context, exp)? {
                Ok(len.to_usize().unwrap())
            } else {
                Err(context.fancy_error(
                    "expected a constant u256 value",
                    vec![Label::primary(len.span, "Array length")],
                    vec!["Note: Array length must be a constant u256".to_string()],
                ))
            }
        }
    };

    match size {
        Ok(size) => Ok(ExpressionAttributes::new(
            Type::Array(Array {
                size,
                inner: value.typ,
            })
            .id(context.db()),
        )),

        Err(diag) => {
            if let Some(expected) = expected_type {
                Ok(ExpressionAttributes::new(expected))
            } else {
                Err(FatalError::new(diag))
            }
        }
    }
}

fn expr_tuple(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let expected_items = expected_type.and_then(|id| {
        id.deref(context.db())
            .as_tuple(context.db())
            .map(|tup| tup.items)
    });

    if let fe::Expr::Tuple { elts } = &exp.kind {
        let types = elts
            .iter()
            .enumerate()
            .map(|(idx, elt)| {
                let exp_type = expected_items
                    .as_ref()
                    .and_then(|items| items.get(idx).copied());

                expr(context, elt, exp_type).map(|attributes| attributes.typ)
            })
            .collect::<Result<Vec<_>, _>>()?;

        if !&types.iter().all(|id| id.has_fixed_size(context.db())) {
            // TODO: doesn't need to be fatal if expected.is_some()
            return Err(FatalError::new(context.error(
                "variable size types can not be part of tuples",
                exp.span,
                "",
            )));
        }

        Ok(ExpressionAttributes::new(context.db().intern_type(
            Type::Tuple(Tuple {
                items: types.to_vec().into(),
            }),
        )))
    } else {
        unreachable!()
    }
}

fn expr_name(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let name = match &exp.kind {
        fe::Expr::Name(name) => name,
        _ => unreachable!(),
    };

    let name_thing = context.resolve_name(name, exp.span)?;
    expr_named_thing(context, exp, name_thing, expected_type)
}

fn expr_path(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let path = match &exp.kind {
        fe::Expr::Path(path) => path,
        _ => unreachable!(),
    };

    let named_thing = context.resolve_path(path, exp.span)?;
    expr_named_thing(context, exp, Some(named_thing), expected_type)
}

fn expr_named_thing(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    named_thing: Option<NamedThing>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let ty = match named_thing {
        Some(NamedThing::Variable { typ, .. }) => Ok(typ?),
        Some(NamedThing::SelfValue { decl, parent, .. }) => {
            if let Some(target) = parent {
                if decl.is_none() {
                    context.fancy_error(
                        "`self` is not defined",
                        vec![Label::primary(exp.span, "undefined value")],
                        if let Item::Function(func_id) = context.parent() {
                            vec![
                                "add `self` to the scope by including it in the function signature"
                                    .to_string(),
                                format!(
                                    "Example: `fn {}(self, foo: bool)`",
                                    func_id.name(context.db())
                                ),
                            ]
                        } else {
                            vec!["can't use `self` outside of function".to_string()]
                        },
                    );
                }

                let mut self_typ = match target {
                    Item::Type(TypeDef::Struct(s)) => Type::Struct(s).id(context.db()),
                    Item::Type(TypeDef::Enum(e)) => Type::Enum(e).id(context.db()),
                    Item::Impl(id) => id.receiver(context.db()),

                    // This can only happen when trait methods can implement a default body
                    Item::Trait(id) => {
                        return Err(FatalError::new(context.fancy_error(
                            &format!(
                                "`{}` is a trait, and can't be used as an expression",
                                exp.kind
                            ),
                            vec![
                                Label::primary(
                                    id.name_span(context.db()),
                                    format!("`{}` is defined here as a trait", exp.kind),
                                ),
                                Label::primary(
                                    exp.span,
                                    format!("`{}` is used here as a value", exp.kind),
                                ),
                            ],
                            vec![],
                        )))
                    }
                    Item::Type(TypeDef::Contract(c)) => Type::SelfContract(c).id(context.db()),
                    _ => unreachable!(),
                };
                // If there's no `self` param, let it be mut to avoid confusing errors
                if decl.map(|d| d.mut_.is_some()).unwrap_or(true) {
                    self_typ = Type::Mut(self_typ).id(context.db());
                }
                Ok(self_typ)
            } else {
                Err(context.fancy_error(
                    "`self` can only be used in contract, struct, trait or impl functions",
                    vec![Label::primary(
                        exp.span,
                        "not allowed in functions defined directly in a module",
                    )],
                    vec![],
                ))
            }
        }
        Some(NamedThing::Item(Item::Constant(id))) => {
            let typ = id.typ(context.db())?;

            if !typ.has_fixed_size(context.db()) {
                panic!("const type must be fixed size")
            }

            Ok(typ)
        }
        Some(NamedThing::EnumVariant(variant)) => {
            if let Ok(EnumVariantKind::Tuple(_)) = variant.kind(context.db()) {
                let name = variant.name_with_parent(context.db());
                context.fancy_error(
                    &format!(
                        "`{}` is not a unit variant",
                        variant.name_with_parent(context.db()),
                    ),
                    vec![
                        Label::primary(exp.span, format! {"`{name}` is not a unit variant"}),
                        Label::secondary(
                            variant.data(context.db()).ast.span,
                            format!("`{}` is defined here", variant.name(context.db())),
                        ),
                    ],
                    vec![],
                );
            }

            Ok(variant.parent(context.db()).as_type(context.db()))
        }

        Some(item) => {
            let item_kind = item.item_kind_display_name();
            let diag = if let Some(def_span) = item.name_span(context.db()) {
                context.fancy_error(
                    &format!(
                        "`{}` is a {} name, and can't be used as an expression",
                        exp.kind, item_kind
                    ),
                    vec![
                        Label::primary(
                            def_span,
                            format!("`{}` is defined here as a {}", exp.kind, item_kind),
                        ),
                        Label::primary(exp.span, format!("`{}` is used here as a value", exp.kind)),
                    ],
                    vec![],
                )
            } else {
                context.error(
                    &format!(
                        "`{}` is a built-in {} name, and can't be used as an expression",
                        exp.kind, item_kind
                    ),
                    exp.span,
                    &format!("`{}` is used here as a value", exp.kind),
                )
            };
            Err(diag)
        }
        None => Err(context.error(
            &format!("cannot find value `{}` in this scope", exp.kind),
            exp.span,
            "undefined",
        )),
    };
    match ty {
        Ok(ty) => Ok(ExpressionAttributes::new(ty)),
        Err(diag) => {
            if let Some(expected) = expected_type {
                Ok(ExpressionAttributes::new(expected))
            } else {
                Err(FatalError::new(diag))
            }
        }
    }
}

fn expr_str(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Str(string) = &exp.kind {
        if !is_valid_string(string) {
            context.error("String contains invalid byte sequence", exp.span, "");
        };

        if !context.is_in_function() {
            context.fancy_error(
                "string literal can't be used outside function",
                vec![Label::primary(exp.span, "string type is used here")],
                vec!["Note: string literal can be used only inside function".into()],
            );
        }

        let str_len = string.len();
        let expected_str_len = expected_type
            .and_then(|id| id.deref(context.db()).as_string(context.db()))
            .map(|s| s.max_size)
            .unwrap_or(str_len);
        // Use an expected string length if an expected length is larger than an actual
        // length.
        let max_size = if expected_str_len > str_len {
            expected_str_len
        } else {
            str_len
        };

        return Ok(ExpressionAttributes::new(
            Type::String(FeString { max_size }).id(context.db()),
        ));
    }

    unreachable!()
}

fn is_valid_string(val: &str) -> bool {
    const ALLOWED_SPECIAL_CHARS: [u8; 3] = [
        9_u8,  // Tab
        10_u8, // Newline
        13_u8, // Carriage return
    ];

    const PRINTABLE_ASCII: RangeInclusive<u8> = 32_u8..=126_u8;

    for x in val.as_bytes() {
        if ALLOWED_SPECIAL_CHARS.contains(x) || PRINTABLE_ASCII.contains(x) {
            continue;
        } else {
            return false;
        }
    }
    true
}

fn expr_num(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> ExpressionAttributes {
    let num = match &exp.kind {
        fe::Expr::Num(num) => num,
        _ => unreachable!(),
    };

    let literal = numeric::Literal::new(num);
    let num = literal
        .parse::<BigInt>()
        .expect("the numeric literal contains a invalid digit");

    if expected_type == Some(TypeId::address(context.db())) {
        if num < BigInt::zero() && num > types::address_max() {
            context.error(
                "literal out of range for `address` type",
                exp.span,
                "does not fit into type `address`",
            );
        }
        // TODO: error if literal.radix != Radix::Hexadecimal ?
        return ExpressionAttributes::new(TypeId::address(context.db()));
    }

    let int_typ = expected_type
        .and_then(|id| id.deref(context.db()).as_int(context.db()))
        .unwrap_or(Integer::U256);
    validate_numeric_literal_fits_type(context, num, exp.span, int_typ);
    return ExpressionAttributes::new(TypeId::int(context.db(), int_typ));
}

fn expr_subscript(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Subscript { value, index } = &exp.kind {
        let value_ty = expr_type(context, value)?;
        let expected_index_ty = operations::expected_index_type(context, value_ty);
        let index_ty = expr(context, index, expected_index_ty)?.typ;

        // performs type checking
        let typ = match operations::index(context, value_ty, index_ty, index) {
            Err(err) => {
                let diag = match err {
                    IndexingError::NotSubscriptable => context.fancy_error(
                        &format!(
                            "`{}` type is not subscriptable",
                            value_ty.display(context.db())
                        ),
                        vec![Label::primary(value.span, "unsubscriptable type")],
                        vec!["Note: Only arrays and maps are subscriptable".into()],
                    ),
                    IndexingError::WrongIndexType => context.fancy_error(
                        &format!(
                            "can not subscript {} with type {}",
                            value_ty.display(context.db()),
                            index_ty.display(context.db())
                        ),
                        vec![Label::primary(index.span, "wrong index type")],
                        vec![],
                    ),
                };
                if let Some(expected) = expected_type {
                    expected
                } else {
                    return Err(FatalError::new(diag));
                }
            }
            Ok(t) => t,
        };

        return Ok(ExpressionAttributes::new(typ));
    }

    unreachable!()
}

fn expr_attribute(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let (target, field) = match &exp.kind {
        fe::Expr::Attribute { value, attr } => (value, attr),
        _ => unreachable!(),
    };

    let attrs = expr(context, target, None)?;
    let typ = match field_type(context, attrs.typ, &field.kind, field.span) {
        Ok(t) => t,
        Err(err) => {
            if let Some(expected) = expected_type {
                expected
            } else {
                return Err(err);
            }
        }
    };
    Ok(ExpressionAttributes::new(typ))
}

fn field_type(
    context: &mut dyn AnalyzerContext,
    obj: TypeId,
    field_name: &str,
    field_span: Span,
) -> Result<TypeId, FatalError> {
    match obj.typ(context.db()) {
        Type::Mut(inner) => {
            Ok(Type::Mut(field_type(context, inner, field_name, field_span)?).id(context.db()))
        }
        Type::SPtr(inner) => {
            Ok(Type::SPtr(field_type(context, inner, field_name, field_span)?).id(context.db()))
        }
        Type::SelfType(TraitOrType::TypeId(inner)) => Ok(Type::SelfType(TraitOrType::TypeId(
            field_type(context, inner, field_name, field_span)?,
        ))
        .id(context.db())),
        Type::SelfContract(id) => match id.field_type(context.db(), field_name) {
            Some(typ) => Ok(typ?.make_sptr(context.db())),
            None => Err(FatalError::new(context.fancy_error(
                &format!("No field `{field_name}` exists on this contract"),
                vec![Label::primary(field_span, "undefined field")],
                vec![],
            ))),
        },

        Type::Struct(struct_) => {
            if let Some(struct_field) = struct_.field(context.db(), field_name) {
                if !context.root_item().is_struct(&struct_) && !struct_field.is_public(context.db())
                {
                    context.fancy_error(
                        &format!(
                            "Can not access private field `{}` on struct `{}`",
                            field_name,
                            struct_.name(context.db())
                        ),
                        vec![Label::primary(field_span, "private field")],
                        vec![],
                    );
                }
                Ok(struct_field.typ(context.db())?)
            } else {
                Err(FatalError::new(context.fancy_error(
                    &format!(
                        "No field `{}` exists on struct `{}`",
                        field_name,
                        struct_.name(context.db())
                    ),
                    vec![Label::primary(field_span, "undefined field")],
                    vec![],
                )))
            }
        }
        Type::Tuple(tuple) => {
            let item_index = tuple_item_index(field_name).ok_or_else(||
                    FatalError::new(context.fancy_error(
                        &format!("No field `{field_name}` exists on this tuple"),
                        vec![
                            Label::primary(
                                field_span,
                                "undefined field",
                            )
                        ],
                        vec!["Note: Tuple values are accessed via `itemN` properties such as `item0` or `item1`".into()],
                    )))?;

            tuple.items.get(item_index).copied().ok_or_else(|| {
                FatalError::new(context.fancy_error(
                    &format!("No field `item{item_index}` exists on this tuple"),
                    vec![Label::primary(field_span, "unknown field")],
                    vec![format!(
                        "Note: The highest possible field for this tuple is `item{}`",
                        tuple.items.len() - 1
                    )],
                ))
            })
        }
        _ => Err(FatalError::new(context.fancy_error(
            &format!(
                "No field `{}` exists on type {}",
                field_name,
                obj.display(context.db())
            ),
            vec![Label::primary(field_span, "unknown field")],
            vec![],
        ))),
    }
}

/// Pull the item index from the attribute string (e.g. "item4" -> "4").
fn tuple_item_index(item: &str) -> Option<usize> {
    if item.len() < 5 || &item[..4] != "item" || (item.len() > 5 && &item[4..5] == "0") {
        None
    } else {
        item[4..].parse::<usize>().ok()
    }
}

fn expr_bin_operation(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let (left, op, right) = match &exp.kind {
        fe::Expr::BinOperation { left, op, right } => (left, op, right),
        _ => unreachable!(),
    };

    let (left_expected, right_expected) = match &op.kind {
        // In shift operations, the right hand side may have a different type than the left hand
        // side because the right hand side needs to be unsigned. The type of the
        // entire expression is determined by the left hand side anyway so we don't
        // try to coerce the right hand side in this case.
        fe::BinOperator::LShift | fe::BinOperator::RShift => (expected_type, None),
        _ => (expected_type, expected_type),
    };

    let left_attributes = expr(context, left, left_expected)?;
    let right_attributes = expr(context, right, right_expected)?;

    match operations::bin(
        context,
        left_attributes.typ,
        left,
        op.kind,
        right_attributes.typ,
        right,
    ) {
        Err(err) => {
            let diag = add_bin_operations_errors(
                context,
                &op.kind,
                left.span,
                left_attributes.typ,
                right.span,
                right_attributes.typ,
                err,
            );
            if let Some(expected) = expected_type {
                Ok(ExpressionAttributes::new(expected))
            } else {
                Err(FatalError::new(diag))
            }
        }
        Ok(typ) => Ok(ExpressionAttributes::new(typ)),
    }
}

fn expr_unary_operation(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let (op, operand) = match &exp.kind {
        fe::Expr::UnaryOperation { op, operand } => (op, operand),
        _ => unreachable!(),
    };

    let operand_ty = value_expr_type(context, operand, None)?;

    let emit_err = |context: &mut dyn AnalyzerContext, expected| {
        context.error(
            &format!(
                "cannot apply unary operator `{}` to type `{}`",
                op.kind,
                operand_ty.display(context.db())
            ),
            operand.span,
            &format!(
                "this has type `{}`; expected {}",
                operand_ty.display(context.db()),
                expected
            ),
        );
    };

    return match op.kind {
        fe::UnaryOperator::USub => {
            let expected_int_type = expected_type
                .and_then(|id| id.as_int(context.db()))
                .unwrap_or(Integer::I256);

            if operand_ty.is_integer(context.db()) {
                if let fe::Expr::Num(num_str) = &operand.kind {
                    let num = -to_bigint(num_str);
                    validate_numeric_literal_fits_type(context, num, exp.span, expected_int_type);
                }
                if !expected_int_type.is_signed() {
                    context.error(
                        "Can not apply unary operator",
                        op.span + operand.span,
                        &format!(
                            "Expected unsigned type `{expected_int_type}`. Unary operator `-` can not be used here.",
                        ),
                    );
                }
            } else {
                emit_err(context, "a numeric type")
            }
            Ok(ExpressionAttributes::new(TypeId::int(
                context.db(),
                expected_int_type,
            )))
        }
        fe::UnaryOperator::Not => {
            if !operand_ty.is_bool(context.db()) {
                emit_err(context, "type `bool`");
            }
            Ok(ExpressionAttributes::new(TypeId::bool(context.db())))
        }
        fe::UnaryOperator::Invert => {
            if !operand_ty.is_integer(context.db()) {
                emit_err(context, "a numeric type")
            }

            Ok(ExpressionAttributes::new(operand_ty))
        }
    };
}

fn expr_call(
    context: &mut dyn AnalyzerContext,
    func: &Node<fe::Expr>,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    let (attributes, call_type) = match &func.kind {
        fe::Expr::Name(name) => expr_call_name(context, name, func, generic_args, args)?,
        fe::Expr::Path(path) => expr_call_path(context, path, func, generic_args, args)?,
        fe::Expr::Attribute { value, attr } => {
            // TODO: err if there are generic args
            expr_call_method(context, value, attr, generic_args, args)?
        }
        _ => {
            let expression = expr(context, func, None)?;
            let diag = context.fancy_error(
                &format!(
                    "`{}` type is not callable",
                    expression.typ.display(context.db())
                ),
                vec![Label::primary(
                    func.span,
                    format!("this has type `{}`", expression.typ.display(context.db())),
                )],
                vec![],
            );
            return if let Some(expected) = expected_type {
                Ok(ExpressionAttributes::new(expected))
            } else {
                Err(FatalError::new(diag))
            };
        }
    };

    if !context.inherits_type(BlockScopeType::Unsafe) && call_type.is_unsafe(context.db()) {
        let mut labels = vec![Label::primary(func.span, "call to unsafe function")];
        let fn_name = call_type.function_name(context.db());
        if let Some(function) = call_type.function() {
            let def_name_span = function.name_span(context.db());
            let unsafe_span = function.unsafe_span(context.db());
            labels.push(Label::secondary(
                def_name_span + unsafe_span,
                format!("`{}` is defined here as unsafe", &fn_name),
            ))
        }
        context.fancy_error(
            &format!("unsafe function `{}` can only be called in an unsafe function or block",
                     &fn_name),
            labels,
            vec!["Hint: put this call in an `unsafe` block if you're confident that it's safe to use here".into()],
        );
    }

    if context.is_in_function() {
        context.add_call(func, call_type);
    } else {
        context.error(
            "calling function outside function",
            func.span,
            "function can only be called inside function",
        );
    }

    Ok(attributes)
}

fn expr_call_name<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    name: &str,
    func: &Node<T>,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    check_for_call_to_special_fns(context, name, func.span)?;

    let named_thing = context.resolve_name(name, func.span)?.ok_or_else(|| {
        // Check for call to a fn in the current class that takes self.
        if context.is_in_function() {
            let func_id = context.parent_function();
            if let Some(function) = func_id
                .sig(context.db())
                .self_item(context.db())
                .and_then(|target| target.function_sig(context.db(), name))
                .filter(|fun| fun.takes_self(context.db()))
            {
                // TODO: this doesn't have to be fatal
                FatalError::new(context.fancy_error(
                    &format!("`{name}` must be called via `self`"),
                    vec![
                        Label::primary(
                            function.name_span(context.db()),
                            format!("`{name}` is defined here as a function that takes `self`"),
                        ),
                        Label::primary(
                            func.span,
                            format!("`{name}` is called here as a standalone function"),
                        ),
                    ],
                    vec![format!(
                        "Suggestion: use `self.{name}(...)` instead of `{name}(...)`"
                    )],
                ))
            } else {
                FatalError::new(context.error(
                    &format!("`{name}` is not defined"),
                    func.span,
                    &format!("`{name}` has not been defined in this context"),
                ))
            }
        } else {
            FatalError::new(context.error(
                "calling function outside function",
                func.span,
                "function can only be called inside function",
            ))
        }
    })?;

    expr_call_named_thing(context, named_thing, func, generic_args, args)
}

fn expr_call_path<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    path: &fe::Path,
    func: &Node<T>,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    match context.resolve_visible_path(path) {
        Some(named_thing) => {
            check_visibility(context, &named_thing, func.span);
            validate_has_no_conflicting_trait_in_scope(context, &named_thing, path, func)?;
            expr_call_named_thing(context, named_thing, func, generic_args, args)
        }
        // If we we can't resolve a call to a path e.g. `foo::Bar::do_thing()` there is a chance that `do_thing`
        // still exists as as a trait associated function for `foo::Bar`.
        None => expr_call_trait_associated_function(context, path, func, generic_args, args),
    }
}

fn validate_has_no_conflicting_trait_in_scope<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    named_thing: &NamedThing,
    path: &fe::Path,
    func: &Node<T>,
) -> Result<(), FatalError> {
    let fn_name = &path.segments.last().unwrap().kind;
    let parent_path = path.remove_last();
    let parent_thing = context.resolve_path(&parent_path, func.span)?;

    if let NamedThing::Item(Item::Type(val)) = parent_thing {
        let type_id = val.type_id(context.db())?;
        let (_, in_scope_candidates) = type_id.trait_function_candidates(context, fn_name);

        if !in_scope_candidates.is_empty() {
            let labels = vec![Label::primary(
                named_thing.name_span(context.db()).unwrap(),
                format!(
                    "candidate 1 is defined here on the {}",
                    parent_thing.item_kind_display_name(),
                ),
            )]
            .into_iter()
            .chain(
                in_scope_candidates
                    .iter()
                    .enumerate()
                    .map(|(idx, (fun, _impl))| {
                        Label::primary(
                            fun.name_span(context.db()),
                            format!(
                                "candidate #{} is defined here on trait `{}`",
                                idx + 2,
                                _impl.trait_id(context.db()).name(context.db())
                            ),
                        )
                    }),
            )
            .collect::<Vec<_>>();

            return Err(FatalError::new(context.fancy_error(
                "multiple applicable items in scope",
                labels,
                vec![
                    "Hint: Rename one of the methods or make sure only one of them is in scope"
                        .into(),
                ],
            )));
        }
    }

    Ok(())
}

fn expr_call_trait_associated_function<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    path: &fe::Path,
    func: &Node<T>,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    let fn_name = &path.segments.last().unwrap().kind;
    let parent_path = path.remove_last();
    let parent_thing = context.resolve_path(&parent_path, func.span)?;

    if let NamedThing::Item(Item::Type(val)) = parent_thing {
        let type_id = val.type_id(context.db())?;

        let (candidates, in_scope_candidates) = type_id.trait_function_candidates(context, fn_name);

        if in_scope_candidates.len() > 1 {
            context.fancy_error(
                "multiple applicable items in scope",
                in_scope_candidates
                    .iter()
                    .enumerate()
                    .map(|(idx, (fun, _impl))| {
                        Label::primary(
                            fun.name_span(context.db()),
                            format!(
                                "candidate #{} is defined here on trait `{}`",
                                idx + 1,
                                _impl.trait_id(context.db()).name(context.db())
                            ),
                        )
                    })
                    .collect(),
                vec![
                    "Hint: Rename one of the methods or make sure only one of them is in scope"
                        .into(),
                ],
            );
            // We arbitrarily carry on with the first candidate since the error doesn't need to be fatal
            let (fun, _) = in_scope_candidates[0];
            return expr_call_pure(context, fun, func.span, generic_args, args);
        } else if in_scope_candidates.is_empty() && !candidates.is_empty() {
            context.fancy_error(
                "Applicable items exist but are not in scope",
                candidates.iter().enumerate().map(|(idx, (fun, _impl ))| {
                    Label::primary(fun.name_span(context.db()), format!(
                        "candidate #{} is defined here on trait `{}`",
                        idx + 1,
                        _impl.trait_id(context.db()).name(context.db())
                    ))
                }).collect(),
                vec!["Hint: Bring one of these candidates in scope via `use module_name::trait_name`".into()],
            );
            // We arbitrarily carry on with an applicable candidate since the error doesn't need to be fatal
            let (fun, _) = candidates[0];
            return expr_call_pure(context, fun, func.span, generic_args, args);
        } else if in_scope_candidates.len() == 1 {
            let (fun, _) = in_scope_candidates[0];
            return expr_call_pure(context, fun, func.span, generic_args, args);
        }
    }

    // At this point, we will have an error so we run `resolve_path` to register any errors that we
    // did not report yet
    context.resolve_path(path, func.span)?;

    Err(FatalError::new(context.error(
        "unresolved path item",
        func.span,
        "not found",
    )))
}

fn expr_call_named_thing<T: std::fmt::Display>(
    context: &mut dyn AnalyzerContext,
    named_thing: NamedThing,
    func: &Node<T>,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    match named_thing {
        NamedThing::Item(Item::BuiltinFunction(function)) => {
            expr_call_builtin_function(context, function, func.span, generic_args, args)
        }
        NamedThing::Item(Item::Intrinsic(function)) => {
            expr_call_intrinsic(context, function, func.span, generic_args, args)
        }
        NamedThing::Item(Item::Function(function)) => {
            expr_call_pure(context, function, func.span, generic_args, args)
        }
        NamedThing::Item(Item::Type(def)) => {
            if let Some(args) = generic_args {
                context.fancy_error(
                    &format!("`{}` type is not generic", func.kind),
                    vec![Label::primary(
                        args.span,
                        "unexpected generic argument list",
                    )],
                    vec![],
                );
            }
            let typ = def.type_id(context.db())?;
            expr_call_type_constructor(context, typ, func.span, args)
        }
        NamedThing::Item(Item::GenericType(generic)) => {
            let concrete_type =
                apply_generic_type_args(context, generic, func.span, generic_args.as_ref())?;
            expr_call_type_constructor(context, concrete_type, func.span, args)
        }
        NamedThing::Item(Item::Constant(id)) => Err(FatalError::new(context.error(
            &format!("`{}` is not callable", func.kind),
            func.span,
            &format!(
                "`{}` is a constant of type `{}`, and can't be used as a function",
                func.kind,
                id.typ(context.db())?.display(context.db()),
            ),
        ))),
        NamedThing::Item(Item::Trait(_)) => Err(FatalError::new(context.error(
            &format!("`{}` is not callable", func.kind),
            func.span,
            &format!(
                "`{}` is a trait, and can't be used as a function",
                func.kind
            ),
        ))),
        NamedThing::Item(Item::Impl(_)) => unreachable!(),
        NamedThing::Item(Item::Attribute(_)) => unreachable!(),
        NamedThing::Item(Item::Ingot(_)) => Err(FatalError::new(context.error(
            &format!("`{}` is not callable", func.kind),
            func.span,
            &format!(
                "`{}` is an ingot, and can't be used as a function",
                func.kind
            ),
        ))),
        NamedThing::Item(Item::Module(_)) => Err(FatalError::new(context.error(
            &format!("`{}` is not callable", func.kind),
            func.span,
            &format!(
                "`{}` is a module, and can't be used as a function",
                func.kind
            ),
        ))),

        NamedThing::EnumVariant(variant) => {
            expr_call_enum_constructor(context, func.span, variant, args)
        }

        // Nothing else is callable (for now at least)
        NamedThing::SelfValue { .. } => Err(FatalError::new(context.error(
            "`self` is not callable",
            func.span,
            "can't be used as a function",
        ))),

        NamedThing::Variable { typ, span, .. } => Err(FatalError::new(context.fancy_error(
            &format!("`{}` is not callable", func.kind),
            vec![
                Label::secondary(
                    span,
                    format!("`{}` has type `{}`", func.kind, typ?.display(context.db())),
                ),
                Label::primary(
                    func.span,
                    format!("`{}` can't be used as a function", func.kind),
                ),
            ],
            vec![],
        ))),
    }
}

fn expr_call_builtin_function(
    context: &mut dyn AnalyzerContext,
    function: GlobalFunction,
    name_span: Span,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    if let Some(args) = generic_args {
        context.error(
            &format!(
                "`{}` function does not expect generic arguments",
                function.as_ref()
            ),
            args.span,
            "unexpected generic argument list",
        );
    }

    let argument_attributes = expr_call_args(context, args)?;

    let attrs = match function {
        GlobalFunction::Keccak256 => {
            validate_arg_count(context, function.as_ref(), name_span, args, 1, "argument");
            expect_no_label_on_arg(context, args, 0);

            if let Some(arg_typ) = argument_attributes.first().map(|attr| &attr.typ) {
                match arg_typ.typ(context.db()) {
                    Type::Array(Array { inner, .. }) if inner.typ(context.db()) == Type::u8() => {}
                    _ => {
                        context.fancy_error(
                            &format!(
                                "`{}` can not be used as an argument to `{}`",
                                arg_typ.display(context.db()),
                                function.as_ref(),
                            ),
                            vec![Label::primary(args.span, "wrong type")],
                            vec![format!(
                                "Note: `{}` expects a byte array argument",
                                function.as_ref()
                            )],
                        );
                    }
                }
            };
            ExpressionAttributes::new(TypeId::int(context.db(), Integer::U256))
        }
    };
    Ok((attrs, CallType::BuiltinFunction(function)))
}

fn expr_call_intrinsic(
    context: &mut dyn AnalyzerContext,
    function: Intrinsic,
    name_span: Span,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    if let Some(args) = generic_args {
        context.error(
            &format!(
                "`{}` function does not expect generic arguments",
                function.as_ref()
            ),
            args.span,
            "unexpected generic argument list",
        );
    }

    let argument_attributes = expr_call_args(context, args)?;

    validate_arg_count(
        context,
        function.as_ref(),
        name_span,
        args,
        function.arg_count(),
        "arguments",
    );
    for (idx, arg_attr) in argument_attributes.iter().enumerate() {
        if arg_attr.typ.typ(context.db()) != Type::Base(Base::Numeric(Integer::U256)) {
            context.type_error(
                "arguments to intrinsic functions must be u256",
                args.kind[idx].kind.value.span,
                TypeId::int(context.db(), Integer::U256),
                arg_attr.typ,
            );
        }
    }

    Ok((
        ExpressionAttributes::new(TypeId::base(context.db(), function.return_type())),
        CallType::Intrinsic(function),
    ))
}

fn validate_visibility_of_called_fn(
    context: &mut dyn AnalyzerContext,
    call_span: Span,
    called_fn: FunctionSigId,
) {
    let fn_parent = called_fn.parent(context.db());

    // We don't need to validate `pub` if the called function is a module function
    if let Item::Module(_) = fn_parent {
        return;
    }

    let name = called_fn.name(context.db());
    let parent_name = fn_parent.name(context.db());

    let is_called_from_same_item = fn_parent == context.parent_function().parent(context.db());
    if !called_fn.is_public(context.db()) && !is_called_from_same_item {
        context.fancy_error(
            &format!(
                "the function `{}` on `{} {}` is private",
                name,
                fn_parent.item_kind_display_name(),
                fn_parent.name(context.db())
            ),
            vec![
                Label::primary(call_span, "this function is not `pub`"),
                Label::secondary(
                    called_fn.name_span(context.db()),
                    format!("`{name}` is defined here"),
                ),
            ],
            vec![
                format!(
                    "`{name}` can only be called from other functions within `{parent_name}`"
                ),
                format!(
                    "Hint: use `pub fn {name}(..)` to make `{name}` callable from outside of `{parent_name}`"
                ),
            ],
        );
    }
}

fn expr_call_pure(
    context: &mut dyn AnalyzerContext,
    function: FunctionId,
    call_span: Span,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    let sig = function.sig(context.db());
    validate_visibility_of_called_fn(context, call_span, sig);

    let fn_name = function.name(context.db());
    if let Some(args) = generic_args {
        context.fancy_error(
            &format!("`{fn_name}` function is not generic"),
            vec![Label::primary(
                args.span,
                "unexpected generic argument list",
            )],
            vec![],
        );
    }

    if function.is_test(context.db()) {
        context.fancy_error(
            &format!("`{fn_name}` is a test function"),
            vec![Label::primary(call_span, "test functions are not callable")],
            vec![],
        );
    }

    let sig = function.signature(context.db());
    let name_span = function.name_span(context.db());
    validate_named_args(context, &fn_name, name_span, args, &sig.params)?;
    borrowck::check_fn_call_arg_borrows(context, &fn_name, None, &args.kind, &sig.params);

    let return_type = sig.return_type.clone()?;
    Ok((
        ExpressionAttributes::new(return_type),
        CallType::Pure(function),
    ))
}

fn expr_call_type_constructor(
    context: &mut dyn AnalyzerContext,
    into_type: TypeId,
    into_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    let typ = into_type.typ(context.db());
    match typ {
        Type::Struct(struct_type) => {
            return expr_call_struct_constructor(context, into_span, struct_type, args)
        }
        Type::Base(Base::Bool)
        | Type::Enum(_)
        | Type::Array(_)
        | Type::Map(_)
        | Type::Generic(_) => {
            return Err(FatalError::new(context.error(
                &format!("`{}` type is not callable", typ.display(context.db())),
                into_span,
                "",
            )))
        }
        Type::SPtr(_) => unreachable!(), // unnameable
        _ => {}
    }

    // These all expect 1 arg, for now.
    let type_name = typ.name(context.db());
    validate_arg_count(context, &type_name, into_span, args, 1, "argument");
    expect_no_label_on_arg(context, args, 0);

    if let Some(arg) = args.kind.first() {
        let expected = into_type.is_integer(context.db()).then_some(into_type);
        let from_type = expr(context, &arg.kind.value, expected)?.typ;
        try_cast_type(context, from_type, &arg.kind.value, into_type, into_span);
    }
    Ok((
        ExpressionAttributes::new(into_type),
        CallType::TypeConstructor(into_type),
    ))
}

fn expr_call_struct_constructor(
    context: &mut dyn AnalyzerContext,
    name_span: Span,
    struct_: StructId,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    let name = &struct_.name(context.db());
    // Check visibility of struct.

    if struct_.has_private_field(context.db()) && !context.root_item().is_struct(&struct_) {
        let labels = struct_
            .fields(context.db())
            .iter()
            .filter_map(|(name, field)| {
                if !field.is_public(context.db()) {
                    Some(Label::primary(
                        field.span(context.db()),
                        format!("Field `{name}` is private"),
                    ))
                } else {
                    None
                }
            })
            .collect();

        context.fancy_error(
            &format!(
                "Can not call private constructor of struct `{name}` "
            ),
            labels,
            vec![format!(
                "Suggestion: implement a method `new(...)` on struct `{name}` to call the constructor and return the struct"
            )],
        );
    }

    let db = context.db();
    let fields = struct_
        .fields(db)
        .iter()
        .map(|(name, field)| (name.clone(), field.typ(db), true))
        .collect::<Vec<_>>();

    validate_named_args(context, name, name_span, args, &fields)?;

    let struct_type = struct_.as_type(context.db());
    Ok((
        ExpressionAttributes::new(struct_type),
        CallType::TypeConstructor(struct_type),
    ))
}

fn expr_call_enum_constructor(
    context: &mut dyn AnalyzerContext,
    name_span: Span,
    variant: EnumVariantId,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    let name = &variant.name_with_parent(context.db());
    match variant.kind(context.db())? {
        EnumVariantKind::Unit => {
            let name = variant.name_with_parent(context.db());
            let label = Label::primary(name_span, format! {"`{name}` is a unit variant"});
            context.fancy_error(
                &format!("Can not call a unit variant `{name}`",),
                vec![label],
                vec![format!(
                    "Suggestion: remove the parentheses to construct the unit variant `{name}`",
                )],
            );
        }
        EnumVariantKind::Tuple(elts) => {
            let params: Vec<_> = elts.iter().map(|ty| (None, Ok(*ty), true)).collect();
            validate_named_args(context, name, name_span, args, &params)?;
        }
    }

    Ok((
        ExpressionAttributes::new(variant.parent(context.db()).as_type(context.db())),
        CallType::EnumConstructor(variant),
    ))
}

fn expr_call_method(
    context: &mut dyn AnalyzerContext,
    target: &Node<fe::Expr>,
    field: &Node<SmolStr>,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    // We need to check if the target is a type or a global object before calling
    // `expr()`. When the type method call syntax is changed to `MyType::foo()`
    // and the global objects are replaced by `Context`, we can remove this.
    // All other `NamedThing`s will be handled correctly by `expr()`.
    if let fe::Expr::Name(name) = &target.kind {
        if let Ok(Some(NamedThing::Item(Item::Type(def)))) = context.resolve_name(name, target.span)
        {
            let typ = def.typ(context.db())?;
            return expr_call_type_attribute(context, typ, target.span, field, generic_args, args);
        }
    }

    let target_attributes = expr(context, target, None)?;

    // Check built-in methods.
    if let Ok(method) = ValueMethod::from_str(&field.kind) {
        return expr_call_builtin_value_method(
            context,
            target_attributes,
            target,
            method,
            field,
            args,
        );
    }

    let obj_type = target_attributes.typ.deref(context.db());
    if obj_type.is_contract(context.db()) {
        check_for_call_to_special_fns(context, &field.kind, field.span)?;
    }

    match obj_type.function_sigs(context.db(), &field.kind).as_ref() {
        [] => Err(FatalError::new(context.fancy_error(
            &format!(
                "No function `{}` exists on type `{}`",
                &field.kind,
                obj_type.display(context.db())
            ),
            vec![Label::primary(field.span, "undefined function")],
            vec![],
        ))),
        [method] => {
            validate_visibility_of_called_fn(context, field.span, *method);

            if !method.takes_self(context.db()) {
                let prefix = if method.is_module_fn(context.db())
                    && context.module() == method.module(context.db())
                {
                    "".into()
                } else {
                    format!("{}::", method.parent(context.db()).name(context.db()))
                };

                context.fancy_error(
                    &format!("`{}` must be called without `self`", &field.kind),
                    vec![Label::primary(field.span, "function does not take self")],
                    vec![format!(
                        "Suggestion: try `{}{}(...)` instead of `self.{}(...)`",
                        prefix, &field.kind, &field.kind
                    )],
                );
            }

            let sig = method.signature(context.db());
            let mut_self = matches!(sig.self_decl.map(|d| d.is_mut()), Some(true));
            if mut_self && !target_attributes.typ.is_mut(context.db()) {
                context.error(
                    &format!("`{}` takes `mut self`", &field.kind),
                    target.span,
                    "this is not mutable",
                );
            }

            validate_named_args(context, &field.kind, field.span, args, &sig.params)?;
            borrowck::check_fn_call_arg_borrows(
                context,
                &field.kind,
                Some((target, target_attributes.typ)),
                &args.kind,
                &sig.params,
            );

            let calltype = match obj_type.typ(context.db()) {
                Type::Contract(contract) | Type::SelfContract(contract) => {
                    let method = contract
                        .function(context.db(), &method.name(context.db()))
                        .unwrap();

                    if is_self_value(target) {
                        CallType::ValueMethod {
                            typ: obj_type,
                            method,
                        }
                    } else {
                        // Contract address needs to be on the stack
                        deref_type(context, target, target_attributes.typ);
                        CallType::External {
                            contract,
                            function: method,
                        }
                    }
                }
                Type::Generic(inner) => CallType::TraitValueMethod {
                    trait_id: *inner.bounds.first().expect("expected trait bound"),
                    method: *method,
                    generic_type: inner,
                },
                _ => {
                    let method = method.function(context.db()).unwrap();
                    if let Type::SPtr(inner) = target_attributes.typ.typ(context.db()) {
                        if matches!(
                            inner.typ(context.db()),
                            Type::Struct(_) | Type::Tuple(_) | Type::Array(_)
                        ) {
                            let kind = obj_type.kind_display_name(context.db());
                            context.fancy_error(
                                &format!("{kind} functions can only be called on {kind} in memory"),
                                vec![
                                    Label::primary(target.span, "this value is in storage"),
                                    Label::secondary(
                                        field.span,
                                        format!("hint: copy the {kind} to memory with `.to_mem()`"),
                                    ),
                                ],
                                vec![],
                            );
                        }
                    }

                    if let Item::Impl(id) = method.parent(context.db()) {
                        validate_trait_in_scope(context, field.span, method, id);
                    }

                    CallType::ValueMethod {
                        typ: obj_type,
                        method,
                    }
                }
            };

            let return_type = sig.return_type.clone()?;
            Ok((ExpressionAttributes::new(return_type), calltype))
        }
        [first, second, ..] => {
            context.fancy_error(
                "multiple applicable items in scope",
                vec![
                    Label::primary(
                        first.name_span(context.db()),
                        format!(
                            "candidate #1 is defined here on the `{}`",
                            first.parent(context.db()).item_kind_display_name()
                        ),
                    ),
                    Label::primary(
                        second.name_span(context.db()),
                        format!(
                            "candidate #2 is defined here on the `{}`",
                            second.parent(context.db()).item_kind_display_name()
                        ),
                    ),
                ],
                vec!["Hint: rename one of the methods to disambiguate".into()],
            );
            let return_type = first.signature(context.db()).return_type.clone()?;
            return Ok((
                ExpressionAttributes::new(return_type),
                CallType::ValueMethod {
                    typ: obj_type,
                    method: first.function(context.db()).unwrap(),
                },
            ));
        }
    }
}

fn validate_trait_in_scope(
    context: &mut dyn AnalyzerContext,
    name_span: Span,
    called_fn: FunctionId,
    impl_: ImplId,
) {
    let treit = impl_.trait_id(context.db());
    let type_name = impl_.receiver(context.db()).name(context.db());

    if !context
        .module()
        .is_in_scope(context.db(), Item::Trait(treit))
    {
        context.fancy_error(
            &format!(
                "No method named `{}` found for type `{}` in the current scope",
                called_fn.name(context.db()),
                type_name
            ),
            vec![
                Label::primary(name_span, format!("method not found in `{type_name}`")),
                Label::secondary(called_fn.name_span(context.db()), format!("the method is available for `{type_name}` here"))
            ],
            vec![
                "Hint: items from traits can only be used if the trait is in scope".into(),
                format!("Hint: the following trait is implemented but not in scope; perhaps add a `use` for it: `trait {}`", treit.name(context.db()))
            ],
        );
    }
}

fn expr_call_builtin_value_method(
    context: &mut dyn AnalyzerContext,
    mut value_attrs: ExpressionAttributes,
    value: &Node<fe::Expr>,
    method: ValueMethod,
    method_name: &Node<SmolStr>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    // for now all of these functions expect 0 arguments
    validate_arg_count(
        context,
        &method_name.kind,
        method_name.span,
        args,
        0,
        "argument",
    );

    let ty = value_attrs.typ;
    let calltype = CallType::BuiltinValueMethod {
        method,
        typ: value_attrs.typ,
    };
    match method {
        ValueMethod::ToMem => {
            if ty.is_sptr(context.db()) {
                let inner = ty.deref(context.db());
                if inner.is_primitive(context.db()) {
                    context.fancy_error(
                        "`to_mem()` called on primitive type",
                        vec![
                            Label::primary(
                                value.span,
                                "this value does not need to be explicitly copied to memory",
                            ),
                            Label::secondary(method_name.span, "hint: remove `.to_mem()`"),
                        ],
                        vec![],
                    );
                } else if inner.is_map(context.db()) {
                    context.fancy_error(
                        "`to_mem()` called on a Map",
                        vec![
                            Label::primary(value.span, "Maps can not be copied to memory"),
                            Label::secondary(method_name.span, "hint: remove `.to_mem()`"),
                        ],
                        vec![],
                    );

                    // TODO: this restriction should be removed
                } else if ty.is_generic(context.db()) {
                    context.fancy_error(
                        "`to_mem()` called on generic type",
                        vec![
                            Label::primary(value.span, "this value can not be copied to memory"),
                            Label::secondary(method_name.span, "hint: remove `.to_mem()`"),
                        ],
                        vec![],
                    );
                }

                value_attrs.typ = inner;
                return Ok((value_attrs, calltype));
            } else {
                context.fancy_error(
                    "`to_mem()` called on value in memory",
                    vec![
                        Label::primary(value.span, "this value is already in memory"),
                        Label::secondary(
                            method_name.span,
                            "hint: to make a copy, use `.clone()` here",
                        ),
                    ],
                    vec![],
                );
            }
            Ok((value_attrs, calltype))
        }
        ValueMethod::AbiEncode => {
            abi_encoded_type(context, value_attrs.typ, value.span).map(|attr| (attr, calltype))
        }
    }
}

fn abi_encoded_type(
    context: &mut dyn AnalyzerContext,
    ty: TypeId,
    span: Span,
) -> Result<ExpressionAttributes, FatalError> {
    match ty.typ(context.db()) {
        Type::SPtr(inner) => {
            let res = abi_encoded_type(context, inner, span);
            if res.is_ok() {
                context.add_diagnostic(errors::to_mem_error(span));
            }
            res
        }
        Type::Struct(struct_) => Ok(ExpressionAttributes::new(
            Type::Array(Array {
                inner: Type::u8().id(context.db()),
                size: struct_.fields(context.db()).len() * 32,
            })
            .id(context.db()),
        )),
        Type::Tuple(tuple) => Ok(ExpressionAttributes::new(
            Type::Array(Array {
                inner: context.db().intern_type(Type::u8()),
                size: tuple.items.len() * 32,
            })
            .id(context.db()),
        )),
        _ => Err(FatalError::new(context.fancy_error(
            &format!(
                "value of type `{}` does not support `abi_encode()`",
                ty.display(context.db())
            ),
            vec![Label::primary(
                span,
                "this value cannot be encoded using `abi_encode()`",
            )],
            vec![
                "Hint: struct and tuple values can be encoded.".into(),
                "Example: `(42,).abi_encode()`".into(),
            ],
        ))),
    }
}

fn expr_call_type_attribute(
    context: &mut dyn AnalyzerContext,
    typ: Type,
    target_span: Span,
    field: &Node<SmolStr>,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    if let Some(generic_args) = generic_args {
        context.error(
            "unexpected generic argument list",
            generic_args.span,
            "unexpected",
        );
    }

    let arg_attributes = expr_call_args(context, args)?;

    let target_type = typ.id(context.db());
    let target_name = typ.name(context.db());

    if let Type::Contract(contract) = typ {
        // Check for Foo.create/create2 (this will go away when the context object is
        // ready)
        if let Ok(function) = ContractTypeMethod::from_str(&field.kind) {
            if context.root_item() == Item::Type(TypeDef::Contract(contract)) {
                context.fancy_error(
                        &format!("`{contract}.{}(...)` called within `{contract}` creates an illegal circular dependency", function.as_ref(), contract=&target_name),
                        vec![Label::primary(field.span, "Contract creation")],
                        vec![format!("Note: Consider using a dedicated factory contract to create instances of `{}`", &target_name)]);
            }
            let arg_count = function.arg_count();
            validate_arg_count(
                context,
                &field.kind,
                field.span,
                args,
                arg_count,
                "argument",
            );

            for i in 0..arg_count {
                if let Some(attrs) = arg_attributes.get(i) {
                    if i == 0 {
                        if let Some(ctx_type) = context.get_context_type() {
                            if attrs.typ != Type::Mut(ctx_type).id(context.db()) {
                                context.fancy_error(
                                    &format!(
                                        "incorrect type for argument to `{}.{}`",
                                        &target_name,
                                        function.as_ref()
                                    ),
                                    vec![Label::primary(
                                        args.kind[i].span,
                                        format!(
                                            "this has type `{}`; expected `mut Context`",
                                            &attrs.typ.display(context.db())
                                        ),
                                    )],
                                    vec![],
                                );
                            }
                        } else {
                            context.fancy_error(
                                "`Context` is not defined",
                                vec![
                                    Label::primary(
                                        args.span,
                                        "`ctx` must be passed into the function",
                                    ),
                                    Label::secondary(
                                        context.parent_function().name_span(context.db()),
                                        "Note: declare `ctx` in this function signature",
                                    ),
                                    Label::secondary(
                                        context.parent_function().name_span(context.db()),
                                        "Example: `pub fn foo(ctx: Context, ...)`",
                                    ),
                                ],
                                vec!["Example: `MyContract.create(ctx, 0)`".into()],
                            );
                        }
                    } else if !attrs.typ.is_integer(context.db()) {
                        context.fancy_error(
                            &format!(
                                "incorrect type for argument to `{}.{}`",
                                &target_name,
                                function.as_ref()
                            ),
                            vec![Label::primary(
                                args.kind[i].span,
                                format!(
                                    "this has type `{}`; expected a number",
                                    &attrs.typ.display(context.db())
                                ),
                            )],
                            vec![],
                        );
                    }
                }
            }
            return Ok((
                ExpressionAttributes::new(context.db().intern_type(typ)),
                CallType::BuiltinAssociatedFunction { contract, function },
            ));
        }
    }

    if let Some(sig) = target_type.function_sig(context.db(), &field.kind) {
        if sig.takes_self(context.db()) {
            return Err(FatalError::new(context.fancy_error(
                &format!(
                    "`{}` function `{}` must be called on an instance of `{}`",
                    &target_name, &field.kind, &target_name,
                ),
                vec![Label::primary(
                    target_span,
                    format!(
                        "expected a value of type `{}`, found type name",
                        &target_name
                    ),
                )],
                vec![],
            )));
        } else {
            context.fancy_error(
                "Static functions need to be called with `::` not `.`",
                vec![Label::primary(
                    field.span,
                    "This is a static function (doesn't take a `self` parameter)",
                )],
                vec![format!(
                    "Try `{}::{}(...)` instead",
                    &target_name, &field.kind
                )],
            );
        }

        validate_visibility_of_called_fn(context, field.span, sig);

        let ret_type = sig.signature(context.db()).return_type.clone()?;

        return Ok((
            ExpressionAttributes::new(ret_type),
            CallType::AssociatedFunction {
                typ: target_type,
                function: sig
                    .function(context.db())
                    .expect("expected function signature to have body"),
            },
        ));
    }

    Err(FatalError::new(context.fancy_error(
        &format!(
            "No function `{}` exists on type `{}`",
            &field.kind,
            typ.display(context.db())
        ),
        vec![Label::primary(field.span, "undefined function")],
        vec![],
    )))
}

fn expr_call_args(
    context: &mut dyn AnalyzerContext,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<Vec<ExpressionAttributes>, FatalError> {
    args.kind
        .iter()
        .map(|arg| expr(context, &arg.kind.value, None))
        .collect::<Result<Vec<_>, _>>()
}

fn expect_no_label_on_arg(
    context: &mut dyn AnalyzerContext,
    args: &Node<Vec<Node<fe::CallArg>>>,
    arg_index: usize,
) {
    if let Some(label) = args
        .kind
        .get(arg_index)
        .and_then(|arg| arg.kind.label.as_ref())
    {
        context.error(
            "argument should not be labeled",
            label.span,
            "remove this label",
        );
    }
}

fn check_for_call_to_special_fns(
    context: &mut dyn AnalyzerContext,
    name: &str,
    span: Span,
) -> Result<(), FatalError> {
    if name == "__init__" || name == "__call__" {
        let label = if name == "__init__" {
            "Note: `__init__` is the constructor function, and can't be called at runtime."
        } else {
            // TODO: add a hint label explaining how to call contracts directly
            // with `Context` (not yet supported).
            "Note: `__call__` is not part of the contract's interface, and can't be called."
        };
        Err(FatalError::new(context.fancy_error(
            &format!("`{name}()` is not directly callable"),
            vec![Label::primary(span, "")],
            vec![label.into()],
        )))
    } else {
        Ok(())
    }
}

fn validate_numeric_literal_fits_type(
    context: &mut dyn AnalyzerContext,
    num: BigInt,
    span: Span,
    int_type: Integer,
) {
    if !int_type.fits(num) {
        context.error(
            &format!("literal out of range for `{int_type}`"),
            span,
            &format!("does not fit into type `{int_type}`"),
        );
    }
}

fn expr_comp_operation(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::CompOperation { left, op, right } = &exp.kind {
        // comparison operands should be moved to the stack
        let left_ty = value_expr_type(context, left, None)?;
        if left_ty.is_primitive(context.db()) {
            expect_expr_type(context, right, left_ty, false)?;
        } else {
            context.error(
                &format!(
                    "`{}` type can't be compared with the `{}` operator",
                    left_ty.display(context.db()),
                    op.kind
                ),
                exp.span,
                "invalid comparison",
            );
        }
        return Ok(ExpressionAttributes::new(TypeId::bool(context.db())));
    }

    unreachable!()
}

fn expr_ternary(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<TypeId>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Ternary {
        if_expr,
        test,
        else_expr,
    } = &exp.kind
    {
        error_if_not_bool(context, test, "`if` test expression must be a `bool`")?;
        let if_attr = expr(context, if_expr, expected_type)?;
        let else_attr = expr(context, else_expr, expected_type.or(Some(if_attr.typ)))?;

        // Should have the same return Type
        if if_attr.typ != else_attr.typ {
            let if_expr_ty = deref_type(context, exp, if_attr.typ);

            if try_coerce_type(context, Some(else_expr), else_attr.typ, if_expr_ty, false).is_err()
            {
                context.fancy_error(
                    "`if` and `else` values must have same type",
                    vec![
                        Label::primary(
                            if_expr.span,
                            format!("this has type `{}`", if_attr.typ.display(context.db())),
                        ),
                        Label::secondary(
                            else_expr.span,
                            format!("this has type `{}`", else_attr.typ.display(context.db())),
                        ),
                    ],
                    vec![],
                );
            }
        }

        return Ok(ExpressionAttributes::new(if_attr.typ));
    }
    unreachable!()
}

fn expr_bool_operation(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::BoolOperation { left, op: _, right } = &exp.kind {
        let bool_ty = TypeId::bool(context.db());
        expect_expr_type(context, left, bool_ty, false)?;
        expect_expr_type(context, right, bool_ty, false)?;
        return Ok(ExpressionAttributes::new(bool_ty));
    }

    unreachable!()
}

/// Converts a input string to `BigInt`.
///
/// # Panics
/// Panics if `num` contains invalid digit.
fn to_bigint(num: &str) -> BigInt {
    numeric::Literal::new(num)
        .parse::<BigInt>()
        .expect("the numeric literal contains a invalid digit")
}

fn is_self_value(expr: &Node<fe::Expr>) -> bool {
    if let fe::Expr::Name(name) = &expr.kind {
        name == "self"
    } else {
        false
    }
}
