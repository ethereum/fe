use crate::builtins::{ContractTypeMethod, GlobalFunction, Intrinsic, ValueMethod};
use crate::context::{AnalyzerContext, CallType, ExpressionAttributes, Location, NamedThing};
use crate::errors::{FatalError, IndexingError, NotFixedSize};
use crate::namespace::items::{Class, FunctionId, Item, TypeDef};
use crate::namespace::scopes::BlockScopeType;
use crate::namespace::types::{
    Array, Base, Contract, FeString, Integer, Struct, Tuple, Type, TypeDowncast, U256,
};
use crate::operations;
use crate::traversal::call_args::{validate_arg_count, validate_named_args};
use crate::traversal::types::apply_generic_type_args;
use crate::traversal::utils::{add_bin_operations_errors, types_to_fixed_sizes};
use fe_common::diagnostics::Label;
use fe_common::{numeric, Span};
use fe_parser::ast as fe;
use fe_parser::ast::UnaryOperator;
use fe_parser::node::Node;
use num_bigint::BigInt;
use smol_str::SmolStr;
use std::ops::RangeInclusive;
use std::str::FromStr;
use vec1::Vec1;

/// Gather context information for expressions and check for type errors.
pub fn expr(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    let attributes = match &exp.kind {
        fe::Expr::Name(_) => expr_name(context, exp, expected_type),
        fe::Expr::Path(_) => expr_path(context, exp, expected_type),
        fe::Expr::Num(_) => Ok(expr_num(context, exp, expected_type.as_int())),
        fe::Expr::Bool(_) => expr_bool(exp),
        fe::Expr::Subscript { .. } => expr_subscript(context, exp),
        fe::Expr::Attribute { .. } => expr_attribute(context, exp),
        fe::Expr::Ternary { .. } => expr_ternary(context, exp),
        fe::Expr::BoolOperation { .. } => expr_bool_operation(context, exp),
        fe::Expr::BinOperation { .. } => expr_bin_operation(context, exp, expected_type.as_int()),
        fe::Expr::UnaryOperation { .. } => expr_unary_operation(context, exp, expected_type),
        fe::Expr::CompOperation { .. } => expr_comp_operation(context, exp),
        fe::Expr::Call {
            func,
            generic_args,
            args,
        } => expr_call(context, func, generic_args, args),
        fe::Expr::List { elts } => expr_list(context, elts, expected_type.as_array()),
        fe::Expr::Tuple { .. } => expr_tuple(context, exp, expected_type.as_tuple()),
        fe::Expr::Str(_) => expr_str(context, exp, expected_type.as_string()),
        fe::Expr::Unit => Ok(ExpressionAttributes::new(Type::unit(), Location::Value)),
    }?;

    context.add_expression(exp, attributes.clone());
    Ok(attributes)
}

pub fn expr_list(
    context: &mut dyn AnalyzerContext,
    elts: &[Node<fe::Expr>],
    expected_type: Option<&Array>,
) -> Result<ExpressionAttributes, FatalError> {
    if elts.is_empty() {
        return Ok(ExpressionAttributes {
            typ: Type::Array(Array {
                size: 0,
                inner: expected_type.map_or(Base::Unit, |arr| arr.inner),
            }),
            location: Location::Memory,
            move_location: None,
            const_value: None,
        });
    }

    let inner_type = if let Some(expected) = expected_type {
        for elt in elts {
            let element_attributes =
                assignable_expr(context, elt, Some(&Type::Base(expected.inner)))?;
            if element_attributes.typ != Type::Base(expected.inner) {
                context.type_error(
                    "type mismatch",
                    elt.span,
                    &expected.inner,
                    &element_attributes.typ,
                );
            }
        }
        expected.inner
    } else {
        let first_attr = assignable_expr(context, &elts[0], None)?;
        let inner = match first_attr.typ {
            Type::Base(base) => base,
            _ => {
                return Err(FatalError::new(context.error(
                    "arrays can only hold primitive types",
                    elts[0].span,
                    &format!(
                        "this has type `{}`; expected a primitive type",
                        first_attr.typ
                    ),
                )));
            }
        };

        // Assuming every element attribute should match the attribute of 0th element
        // of list.
        for elt in &elts[1..] {
            let element_attributes = assignable_expr(context, elt, Some(&first_attr.typ))?;
            if element_attributes.typ != first_attr.typ {
                context.fancy_error(
                    "array elements must have same type",
                    vec![
                        Label::primary(elts[0].span, format!("this has type `{}`", first_attr.typ)),
                        Label::secondary(
                            elt.span,
                            format!("this has type `{}`", element_attributes.typ),
                        ),
                    ],
                    vec![],
                );
            }
        }
        inner
    };

    // TODO: Right now we are only supporting Base type arrays
    // Potentially we can support tuples as well.
    let array_typ = Array {
        size: elts.len(),
        inner: inner_type,
    };

    Ok(ExpressionAttributes {
        typ: Type::Array(array_typ),
        location: Location::Memory,
        move_location: None,
        const_value: None,
    })
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is on the stack.
pub fn value_expr(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    let original_attributes = expr(context, exp, expected_type)?;
    let attributes = original_attributes.clone().into_loaded().map_err(|_| {
        FatalError::new(context.fancy_error(
            "can't move value onto stack",
            vec![Label::primary(exp.span, "Value to be moved")],
            vec![format!(
                "Note: Can't move `{}` types on the stack",
                original_attributes.typ
            )],
        ))
    })?;

    context.update_expression(exp, attributes.clone());

    Ok(attributes)
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is in the type's assignment location.
pub fn assignable_expr(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    use Type::*;

    let mut attributes = expr(context, exp, expected_type)?;
    match &attributes.typ {
        Base(_) | Contract(_) => {
            if attributes.location != Location::Value {
                attributes.move_location = Some(Location::Value);
            }
        }
        Array(_) | Tuple(_) | String(_) | Struct(_) => {
            if attributes.final_location() != Location::Memory {
                context.fancy_error(
                    "value must be copied to memory",
                    vec![Label::primary(exp.span, "this value is in storage")],
                    vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                         "Example: `self.my_array.to_mem()`".into(),
                    ],
                );
                attributes.move_location = Some(Location::Memory);
            }
        }
        SelfContract(_) => {
            // We can't tell from here how `self is being misused; it might be
            // `x = self` or `f(self)` or `for x in self` or ...
            return Err(FatalError::new(context.error(
                "invalid use of contract `self`",
                exp.span,
                "`self` can't be used here",
            )));
        }
        Map(_) => {
            return Err(FatalError::new(context.error(
                "`Map` type cannot reside in memory",
                exp.span,
                "this type can only be used in a contract field",
            )));
        }
    };
    context.update_expression(exp, attributes.clone());

    Ok(attributes)
}

fn expr_tuple(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Tuple>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Tuple { elts } = &exp.kind {
        let types = elts
            .iter()
            .enumerate()
            .map(|(idx, elt)| {
                let exp_type = expected_type
                    .and_then(|tup| tup.items.get(idx))
                    .map(|fixed| fixed.clone().into());
                assignable_expr(context, elt, exp_type.as_ref()).map(|attributes| attributes.typ)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let tuple_types = match types_to_fixed_sizes(&types) {
            Err(NotFixedSize) => {
                return Err(FatalError::new(context.error(
                    "variable size types can not be part of tuples",
                    exp.span,
                    "",
                )));
            }
            Ok(val) => val,
        };

        let tuple = Tuple {
            items: Vec1::try_from_vec(tuple_types).expect("tuple is empty"),
        };

        Ok(ExpressionAttributes::new(
            Type::Tuple(tuple),
            Location::Memory,
        ))
    } else {
        unreachable!()
    }
}

fn expr_name(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
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
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    let path = match &exp.kind {
        fe::Expr::Path(path) => path,
        _ => unreachable!(),
    };

    let named_thing = context.resolve_path(path, exp.span);
    expr_named_thing(context, exp, named_thing, expected_type)
}

fn expr_named_thing(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    named_thing: Option<NamedThing>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    match named_thing {
        Some(NamedThing::Variable { typ, .. }) => {
            let typ = typ?;
            let location = Location::assign_location(&typ);
            Ok(ExpressionAttributes::new(typ.into(), location))
        }
        Some(NamedThing::SelfValue { decl, class, .. }) => {
            if let Some(class) = class {
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
                match class {
                    Class::Struct(id) => Ok(ExpressionAttributes::new(
                        Type::Struct(Struct::from_id(id, context.db())),
                        Location::Memory,
                    )),
                    Class::Contract(id) => Ok(ExpressionAttributes::new(
                        Type::SelfContract(Contract::from_id(id, context.db())),
                        Location::Value,
                    )),
                }
            } else {
                Err(FatalError::new(context.fancy_error(
                    "`self` can only be used in contract or struct functions",
                    vec![Label::primary(
                        exp.span,
                        "not allowed in functions defined outside of a contract or struct",
                    )],
                    vec![],
                )))
            }
        }
        Some(NamedThing::Item(Item::Constant(id))) => {
            let typ = id
                .typ(context.db())?
                .try_into()
                .expect("const type must be fixed size");

            // Check visibility of constant.
            if !id.is_public(context.db()) && id.module(context.db()) != context.module() {
                let module_name = id.module(context.db()).name(context.db());
                let name = id.name(context.db());
                context.fancy_error(
                    &format!(
                        "the constant `{}` is private",
                        exp.kind,
                    ),
                    vec![
                        Label::primary(exp.span, "this constant is not `pub`"),
                        Label::secondary(
                            id.data(context.db()).ast.span,
                            format!("`{}` is defined here", name)
                        ),
                    ],
                    vec![
                        format!("`{}` can only be used within `{}`", name, module_name),
                        format!("Hint: use `pub const {constant}` to make `{constant}` visible from outside of `{module}`", constant=name, module=module_name),
                    ],
                );
            }

            let location = Location::assign_location(&typ);
            Ok(ExpressionAttributes::new(typ.into(), location))
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
                            &format!("`{}` is defined here as a {}", exp.kind, item_kind),
                        ),
                        Label::primary(
                            exp.span,
                            &format!("`{}` is used here as a value", exp.kind),
                        ),
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
            Err(FatalError::new(diag))
        }
        None => {
            let diag = context.error(
                &format!("cannot find value `{}` in this scope", exp.kind),
                exp.span,
                "undefined",
            );
            match expected_type {
                Some(typ) => Ok(ExpressionAttributes::new(
                    typ.clone(),
                    Location::assign_location(
                        &typ.clone().try_into().map_err(|_| FatalError::new(diag))?,
                    ),
                )),
                None => Err(FatalError::new(diag)),
            }
        }
    }
}

fn expr_str(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<&FeString>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Str(string) = &exp.kind {
        if !is_valid_string(string) {
            context.error("String contains invalid byte sequence", exp.span, "");
        };

        if context.is_in_function() {
            context.add_string(string.clone());
        } else {
            context.fancy_error(
                "string literal can't be used outside function",
                vec![Label::primary(exp.span, "string type is used here")],
                vec!["Note: string literal can be used only inside function".into()],
            );
        }

        let str_len = string.len();
        let expected_str_len = expected_type.map(|typ| typ.max_size).unwrap_or(str_len);
        // Use an expected string length if an expected length is larger than an actual
        // length.
        let max_size = if expected_str_len > str_len {
            expected_str_len
        } else {
            str_len
        };

        return Ok(ExpressionAttributes::new(
            Type::String(FeString { max_size }),
            Location::Memory,
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

fn expr_bool(exp: &Node<fe::Expr>) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Bool(_) = &exp.kind {
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Bool),
            Location::Value,
        ));
    }

    unreachable!()
}

fn expr_num(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<Integer>,
) -> ExpressionAttributes {
    if let fe::Expr::Num(num) = &exp.kind {
        let int_typ = expected_type.unwrap_or(Integer::U256);
        let num = to_bigint(num);
        validate_numeric_literal_fits_type(context, num, exp.span, int_typ);
        return ExpressionAttributes::new(Type::int(int_typ), Location::Value);
    }

    unreachable!()
}

fn expr_subscript(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Subscript { value, index } = &exp.kind {
        let value_attributes = expr(context, value, None)?;
        let index_attributes = value_expr(context, index, None)?;

        // performs type checking
        let typ =
            match operations::index(value_attributes.typ.clone(), index_attributes.typ.clone()) {
                Err(IndexingError::NotSubscriptable) => {
                    return Err(FatalError::new(context.fancy_error(
                        &format!("`{}` type is not subscriptable", value_attributes.typ),
                        vec![Label::primary(value.span, "unsubscriptable type")],
                        vec!["Note: Only arrays and maps are subscriptable".into()],
                    )));
                }
                Err(IndexingError::WrongIndexType) => {
                    return Err(FatalError::new(context.fancy_error(
                        &format!(
                            "can not subscript {} with type {}",
                            value_attributes.typ, index_attributes.typ
                        ),
                        vec![Label::primary(index.span, "wrong index type")],
                        vec![],
                    )));
                }
                Ok(val) => val,
            };

        let location = match value_attributes.location {
            Location::Storage { .. } => Location::Storage { nonce: None },
            Location::Memory => Location::Memory,
            // neither maps or arrays can be stored as values, so this is unreachable
            Location::Value => unreachable!(),
        };

        return Ok(ExpressionAttributes::new(typ, location));
    }

    unreachable!()
}

fn expr_attribute(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    let (target, field) = match &exp.kind {
        fe::Expr::Attribute { value, attr } => (value, attr),
        _ => unreachable!(),
    };

    let attrs = expr(context, target, None)?;
    return match attrs.typ {
        Type::SelfContract(contract) => match contract.id.field_type(context.db(), &field.kind) {
            Some((typ, nonce)) => Ok(ExpressionAttributes::new(
                typ?,
                Location::Storage { nonce: Some(nonce) },
            )),
            None => Err(FatalError::new(context.fancy_error(
                &format!("No field `{}` exists on this contract", &field.kind),
                vec![Label::primary(field.span, "undefined field")],
                vec![],
            ))),
        },
        // If the value is a struct, we return the type of the struct field. The location stays the
        // same and can be memory or storage.
        Type::Struct(struct_) => {
            if let Some(struct_field) = struct_.id.field(context.db(), &field.kind) {
                if !context.root_item().is_struct(&struct_.id)
                    && !struct_field.is_public(context.db())
                {
                    context.fancy_error(
                        &format!(
                            "Can not access private field `{}` on struct `{}`",
                            &field.kind, struct_.name
                        ),
                        vec![Label::primary(field.span, "private field")],
                        vec![],
                    );
                }
                Ok(ExpressionAttributes::new(
                    struct_field.typ(context.db())?.into(),
                    attrs.location,
                ))
            } else {
                Err(FatalError::new(context.fancy_error(
                    &format!(
                        "No field `{}` exists on struct `{}`",
                        &field.kind, struct_.name
                    ),
                    vec![Label::primary(field.span, "undefined field")],
                    vec![],
                )))
            }
        }
        Type::Tuple(tuple) => {
            let item_index = match tuple_item_index(&field.kind) {
                Some(index) => index,
                None => {
                    return Err(FatalError::new(context.fancy_error(
                        &format!("No field `{}` exists on this tuple", &field.kind),
                        vec![
                            Label::primary(
                                field.span,
                                "undefined field",
                            )
                        ],
                        vec!["Note: Tuple values are accessed via `itemN` properties such as `item0` or `item1`".into()],
                    )));
                }
            };

            if let Some(typ) = tuple.items.get(item_index) {
                Ok(ExpressionAttributes::new(
                    typ.clone().into(),
                    attrs.location,
                ))
            } else {
                Err(FatalError::new(context.fancy_error(
                    &format!("No field `item{}` exists on this tuple", item_index),
                    vec![Label::primary(field.span, "unknown field")],
                    vec![format!(
                        "Note: The highest possible field for this tuple is `item{}`",
                        tuple.items.len() - 1
                    )],
                )))
            }
        }
        _ => Err(FatalError::new(context.fancy_error(
            &format!("No field `{}` exists on type {}", &field.kind, attrs.typ),
            vec![Label::primary(field.span, "unknown field")],
            vec![],
        ))),
    };
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
    expected_type: Option<Integer>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::BinOperation { left, op, right } = &exp.kind {
        let (left_expected, right_expected) = match &op.kind {
            // In shift operations, the right hand side may have a different type than the left hand
            // side because the right hand side needs to be unsigned. The type of the
            // entire expression is determined by the left hand side anyway so we don't
            // try to coerce the right hand side in this case.
            fe::BinOperator::LShift | fe::BinOperator::RShift => {
                (expected_type.map(Type::int), None)
            }
            _ => (expected_type.map(Type::int), expected_type.map(Type::int)),
        };

        let left_attributes = value_expr(context, left, left_expected.as_ref())?;
        let right_attributes = value_expr(context, right, right_expected.as_ref())?;

        let typ = match operations::bin(&left_attributes.typ, &op.kind, &right_attributes.typ) {
            Err(err) => {
                return Err(FatalError::new(add_bin_operations_errors(
                    context,
                    &op.kind,
                    left.span,
                    &left_attributes.typ,
                    right.span,
                    &right_attributes.typ,
                    err,
                )));
            }
            Ok(val) => val,
        };

        return Ok(ExpressionAttributes::new(typ, Location::Value));
    }

    unreachable!()
}

fn expr_unary_operation(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::UnaryOperation { op, operand } = &exp.kind {
        let operand_attributes = value_expr(context, operand, None)?;

        let emit_err = |context: &mut dyn AnalyzerContext, expected| {
            context.error(
                &format!(
                    "cannot apply unary operator `{}` to type `{}`",
                    op.kind, operand_attributes.typ
                ),
                operand.span,
                &format!(
                    "this has type `{}`; expected {}",
                    operand_attributes.typ, expected
                ),
            );
        };

        return match op.kind {
            fe::UnaryOperator::USub => {
                let int_type = expected_type.as_int().unwrap_or(Integer::I256);
                match operand_attributes.typ {
                    Type::Base(Base::Numeric(_)) => {
                        if let fe::Expr::Num(num_str) = &operand.kind {
                            let num = -to_bigint(num_str);
                            validate_numeric_literal_fits_type(context, num, exp.span, int_type);
                        }
                    }
                    _ => emit_err(context, "a numeric type"),
                }
                Ok(ExpressionAttributes::new(
                    Type::int(int_type),
                    Location::Value,
                ))
            }
            fe::UnaryOperator::Not => {
                if !matches!(operand_attributes.typ, Type::Base(Base::Bool)) {
                    emit_err(context, "type `bool`");
                }
                Ok(ExpressionAttributes::new(
                    Type::Base(Base::Bool),
                    Location::Value,
                ))
            }
            UnaryOperator::Invert => {
                if !matches!(operand_attributes.typ, Type::Base(Base::Numeric(_))) {
                    emit_err(context, "a numeric type")
                }

                Ok(ExpressionAttributes::new(
                    operand_attributes.typ,
                    Location::Value,
                ))
            }
        };
    }

    unreachable!()
}

fn expr_call(
    context: &mut dyn AnalyzerContext,
    func: &Node<fe::Expr>,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
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
            return Err(FatalError::new(context.fancy_error(
                &format!("`{}` type is not callable", expression.typ),
                vec![Label::primary(
                    func.span,
                    format!("this has type `{}`", expression.typ),
                )],
                vec![],
            )));
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
        context.fancy_error(&format!("unsafe function `{}` can only be called in an unsafe function or block",
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
                .class(context.db())
                .and_then(|class| class.self_function(context.db(), name))
            {
                // TODO: this doesn't have to be fatal
                FatalError::new(context.fancy_error(
                    &format!("`{}` must be called via `self`", name),
                    vec![
                        Label::primary(
                            function.name_span(context.db()),
                            &format!("`{}` is defined here as a function that takes `self`", name),
                        ),
                        Label::primary(
                            func.span,
                            format!("`{}` is called here as a standalone function", name),
                        ),
                    ],
                    vec![format!(
                        "Suggestion: use `self.{}(...)` instead of `{}(...)`",
                        name, name
                    )],
                ))
            } else {
                FatalError::new(context.error(
                    &format!("`{}` is not defined", name),
                    func.span,
                    &format!("`{}` has not been defined in this context", name),
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
    let named_thing = context.resolve_path(path, func.span).ok_or_else(|| {
        FatalError::new(context.error(
            &format!("`{}` is not defined", func.kind),
            func.span,
            &format!("`{}` has not been defined in this context", func.kind),
        ))
    })?;

    expr_call_named_thing(context, named_thing, func, generic_args, args)
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
            expr_call_pure(context, function, generic_args, args)
        }
        NamedThing::Item(Item::Type(id)) => {
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
            let typ = id.typ(context.db())?;
            expr_call_type_constructor(context, typ, func.span, args)
        }
        NamedThing::Item(Item::GenericType(generic)) => {
            let concrete_type =
                apply_generic_type_args(context, generic, func.span, generic_args.as_ref())?;
            expr_call_type_constructor(context, concrete_type, func.span, args)
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
                Label::secondary(span, format!("`{}` has type `{}`", func.kind, typ?)),
                Label::primary(
                    func.span,
                    format!("`{}` can't be used as a function", func.kind),
                ),
            ],
            vec![],
        ))),
        NamedThing::Item(Item::Constant(id)) => Err(FatalError::new(context.error(
            &format!("`{}` is not callable", func.kind),
            func.span,
            &format!(
                "`{}` is a constant of type `{}`, and can't be used as a function",
                func.kind,
                id.typ(context.db())?,
            ),
        ))),
        NamedThing::Item(Item::Event(_)) => Err(FatalError::new(context.fancy_error(
            &format!("`{}` is not callable", func.kind),
            vec![Label::primary(
                func.span,
                &format!(
                    "`{}` is an event, and can't be constructed in this context",
                    func.kind
                ),
            )],
            vec![format!(
                "Hint: to emit an event, use `emit {}(..)`",
                func.kind
            )],
        ))),
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
                if !matches!(
                    arg_typ,
                    Type::Array(Array {
                        inner: Base::Numeric(Integer::U8),
                        ..
                    })
                ) {
                    context.fancy_error(
                        &format!(
                            "`{}` can not be used as an argument to `{}`",
                            arg_typ,
                            function.as_ref(),
                        ),
                        vec![Label::primary(args.span, "wrong type")],
                        vec![format!(
                            "Note: `{}` expects a byte array argument",
                            function.as_ref()
                        )],
                    );
                }
            };
            ExpressionAttributes::new(Type::Base(U256), Location::Value)
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
        if arg_attr.typ != Type::Base(Base::Numeric(Integer::U256)) {
            context.type_error(
                "arguments to intrinsic functions must be u256",
                args.kind[idx].kind.value.span,
                &Integer::U256,
                &arg_attr.typ,
            );
        }
    }

    Ok((
        ExpressionAttributes::new(Type::Base(function.return_type()), Location::Value),
        CallType::Intrinsic(function),
    ))
}

fn expr_call_pure(
    context: &mut dyn AnalyzerContext,
    function: FunctionId,
    generic_args: &Option<Node<Vec<fe::GenericArg>>>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    let fn_name = function.name(context.db());
    if let Some(args) = generic_args {
        context.fancy_error(
            &format!("`{}` function is not generic", fn_name),
            vec![Label::primary(
                args.span,
                "unexpected generic argument list",
            )],
            vec![],
        );
    }

    let sig = function.signature(context.db());
    let name_span = function.name_span(context.db());
    validate_named_args(context, &fn_name, name_span, args, &sig.params)?;

    let return_type = sig.return_type.clone()?;
    let return_location = Location::assign_location(&return_type);
    Ok((
        ExpressionAttributes::new(return_type.into(), return_location),
        CallType::Pure(function),
    ))
}

fn expr_call_type_constructor(
    context: &mut dyn AnalyzerContext,
    typ: Type,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    match typ {
        Type::Struct(struct_type) => {
            return expr_call_struct_constructor(context, name_span, struct_type, args)
        }
        Type::Base(Base::Bool) => {
            return Err(FatalError::new(context.error(
                "`bool` type is not callable",
                name_span,
                "",
            )))
        }
        Type::Map(_) => {
            return Err(FatalError::new(context.error(
                "`Map` type is not callable",
                name_span,
                "",
            )))
        }
        Type::Array(_) => {
            return Err(FatalError::new(context.error(
                "`Array` type is not callable",
                name_span,
                "",
            )))
        }
        _ => {}
    }

    if matches!(typ, Type::Contract(_)) {
        validate_arg_count(context, &format!("{}", typ), name_span, args, 2, "argument");
        expect_no_label_on_arg(context, args, 0);
        expect_no_label_on_arg(context, args, 1);
    } else {
        // These all expect 1 arg, for now.
        validate_arg_count(context, &format!("{}", typ), name_span, args, 1, "argument");
        expect_no_label_on_arg(context, args, 0);
    }

    let expr_attrs = match &typ {
        Type::String(string_type) => {
            if let Some(arg) = args.kind.first() {
                assignable_expr(context, &arg.kind.value, None)?;
                validate_str_literal_fits_type(context, &arg.kind.value, string_type);
            }
            ExpressionAttributes::new(typ.clone(), Location::Memory)
        }
        Type::Contract(_) => {
            if let Some(first_arg) = &args.kind.get(0) {
                let first_arg_attr = assignable_expr(context, &first_arg.kind.value, None)?;
                if let Some(context_type) = context.get_context_type() {
                    if first_arg_attr.typ != Type::Struct(context_type.clone()) {
                        context.type_error(
                            "type mismatch",
                            first_arg.span,
                            &context_type,
                            &first_arg_attr.typ,
                        );
                    }
                } else {
                    context.fancy_error(
                        "`Context` is not defined",
                        vec![
                            Label::primary(
                                args.span,
                                "`ctx` must be defined and passed into the contract constructor",
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
                        vec![
                            "Note: import context with `use std::context::Context`".into(),
                            "Example: MyContract(ctx, contract_address)".into(),
                        ],
                    );
                }
            }
            if let Some(second_arg) = &args.kind.get(1) {
                let second_arg_attr = assignable_expr(context, &second_arg.kind.value, None)?;
                if second_arg_attr.typ != Type::Base(Base::Address) {
                    context.type_error(
                        "type mismatch",
                        second_arg.span,
                        &Base::Address,
                        &second_arg_attr.typ,
                    );
                }
            }
            ExpressionAttributes::new(typ.clone(), Location::Value)
        }
        Type::Base(Base::Numeric(integer)) => {
            if let Some(arg) = args.kind.first() {
                // This will check if the literal fits inside int_type.
                let arg_exp = assignable_expr(context, &arg.kind.value, Some(&typ))?;
                match arg_exp.typ {
                    Type::Base(Base::Numeric(new_type)) => {
                        let sign_differs = integer.is_signed() != new_type.is_signed();
                        let size_differs = integer.size() != new_type.size();

                        if sign_differs && size_differs {
                            context.error("Casting between numeric values can change the sign or size but not both at once", arg.span, &format!("can not cast from `{}` to `{}` in a single step", arg_exp.typ, typ));
                        }
                    }
                    Type::Base(Base::Address) => {
                        if *integer != Integer::U256 {
                            context.error(
                                &format!("can't cast `address` to `{}`", integer),
                                name_span,
                                "try `u256` here",
                            );
                        }
                    }
                    _ => {
                        context.error(
                            "type mismatch",
                            arg.span,
                            &format!("expected a numeric type but was `{}`", arg_exp.typ),
                        );
                    }
                }
            }
            ExpressionAttributes::new(typ.clone(), Location::Value)
        }
        Type::Base(Base::Address) => {
            if let Some(arg) = args.kind.first() {
                let arg_attr = assignable_expr(context, &arg.kind.value, None)?;
                match arg_attr.typ {
                    Type::Contract(_) | Type::Base(Base::Numeric(_) | Base::Address) => {}
                    _ => {
                        context.fancy_error(
                            &format!("`{}` can not be used as a parameter to `address(..)`", arg_attr.typ),
                            vec![Label::primary(arg.span, "wrong type")],
                            vec!["Note: address(..) expects a parameter of a contract type, numeric or address".into()],
                        );
                    }
                }
            };
            ExpressionAttributes::new(Type::Base(Base::Address), Location::Value)
        }
        Type::Base(Base::Unit) => unreachable!(), // rejected in expr_call_type
        Type::Base(Base::Bool) => unreachable!(), // handled above
        Type::Tuple(_) => unreachable!(),         // rejected in expr_call_type
        Type::Struct(_) => unreachable!(),        // handled above
        Type::Map(_) => unreachable!(),           // handled above
        Type::Array(_) => unreachable!(),         // handled above
        Type::SelfContract(_) => unreachable!(),  /* unnameable; contract names all become
                                                    * Type::Contract */
    };
    Ok((expr_attrs, CallType::TypeConstructor(typ)))
}

fn expr_call_struct_constructor(
    context: &mut dyn AnalyzerContext,
    name_span: Span,
    struct_: Struct,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<(ExpressionAttributes, CallType), FatalError> {
    let id = struct_.id;
    let name = &struct_.name;
    // Check visibility of struct.

    if id.has_private_field(context.db()) && !context.root_item().is_struct(&struct_.id) {
        let labels = struct_
            .id
            .private_fields(context.db())
            .iter()
            .map(|(name, field)| {
                Label::primary(
                    field.span(context.db()),
                    format!("Field `{}` is private", name),
                )
            })
            .collect();

        context.fancy_error(
            &format!(
                "Can not call private constructor of struct `{}` ",
                name
            ),
            labels,
            vec![format!(
                "Suggestion: implement a method `new(...)` on struct `{}` to call the constructor and return the struct",
                name
            )],
        );
    }

    let db = context.db();
    let fields = struct_
        .id
        .fields(db)
        .iter()
        .map(|(name, field)| (name.clone(), field.typ(db)))
        .collect::<Vec<_>>();
    validate_named_args(context, name, name_span, args, &fields)?;

    Ok((
        ExpressionAttributes::new(Type::Struct(struct_.clone()), Location::Memory),
        CallType::TypeConstructor(Type::Struct(struct_)),
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
        if let Ok(Some(NamedThing::Item(Item::Type(id)))) = context.resolve_name(name, target.span)
        {
            let typ = id.typ(context.db())?;
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

    // If the target is a "class" type (contract or struct), check for a member
    // function
    if let Some(class) = target_attributes.typ.as_class() {
        if matches!(class, Class::Contract(_)) {
            check_for_call_to_special_fns(context, &field.kind, field.span)?;
        }
        if let Some(method) = class.function(context.db(), &field.kind) {
            let is_self = is_self_value(target);

            if is_self && !method.takes_self(context.db()) {
                context.fancy_error(
                    &format!("`{}` must be called without `self`", &field.kind),
                    vec![Label::primary(field.span, "function does not take self")],
                    vec![format!(
                        "Suggestion: try `{}(...)` instead of `self.{}(...)`",
                        &field.kind, &field.kind
                    )],
                );
            } else if !is_self && !method.is_public(context.db()) {
                context.fancy_error(
                    &format!(
                        "the function `{}` on `{} {}` is private",
                        &field.kind,
                        class.kind(),
                        class.name(context.db())
                    ),
                    vec![
                        Label::primary(field.span, "this function is not `pub`"),
                        Label::secondary(
                            method.data(context.db()).ast.span,
                            format!("`{}` is defined here", &field.kind),
                        ),
                    ],
                    vec![],
                );
            }

            let sig = method.signature(context.db());

            let params = if is_self {
                &sig.params
            } else {
                sig.external_params()
            };
            validate_named_args(context, &field.kind, field.span, args, params)?;

            let calltype = match class {
                Class::Contract(contract) => {
                    if is_self {
                        CallType::ValueMethod {
                            is_self,
                            class,
                            method,
                        }
                    } else {
                        // External contract address must be loaded onto the stack.
                        context.update_expression(
                            target,
                            target_attributes
                                .into_loaded()
                                .expect("should be able to move contract type to stack"),
                        );

                        CallType::External {
                            contract,
                            function: method,
                        }
                    }
                }
                Class::Struct(_) => {
                    if matches!(target_attributes.final_location(), Location::Storage { .. }) {
                        context.fancy_error(
                            "struct functions can only be called on structs in memory",
                            vec![
                                Label::primary(target.span, "this value is in storage"),
                                Label::secondary(
                                    field.span,
                                    "hint: copy the struct to memory with `.to_mem()`",
                                ),
                            ],
                            vec![],
                        );
                    }
                    CallType::ValueMethod {
                        is_self,
                        class,
                        method,
                    }
                }
            };

            let return_type = sig.return_type.clone()?;
            let location = Location::assign_location(&return_type);
            return Ok((
                ExpressionAttributes::new(return_type.into(), location),
                calltype,
            ));
        }
    }
    Err(FatalError::new(context.fancy_error(
        &format!(
            "No function `{}` exists on type `{}`",
            &field.kind, &target_attributes.typ
        ),
        vec![Label::primary(field.span, "undefined function")],
        vec![],
    )))
}

fn expr_call_builtin_value_method(
    context: &mut dyn AnalyzerContext,
    value_attrs: ExpressionAttributes,
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

    let calltype = CallType::BuiltinValueMethod {
        method,
        typ: value_attrs.typ.clone(),
    };
    match method {
        ValueMethod::Clone => {
            match value_attrs.location {
                Location::Storage { .. } => {
                    context.fancy_error(
                        "`clone()` called on value in storage",
                        vec![
                            Label::primary(value.span, "this value is in storage"),
                            Label::secondary(method_name.span, "hint: try `to_mem()` here"),
                        ],
                        vec![],
                    );
                }
                Location::Value => {
                    context.fancy_error(
                        "`clone()` called on primitive type",
                        vec![
                            Label::primary(value.span, "this value does not need to be cloned"),
                            Label::secondary(method_name.span, "hint: remove `.clone()`"),
                        ],
                        vec![],
                    );
                }
                Location::Memory => {}
            }
            Ok((value_attrs.into_cloned(), calltype))
        }
        ValueMethod::ToMem => {
            match value_attrs.location {
                Location::Storage { .. } => {}
                Location::Value => {
                    context.fancy_error(
                        "`to_mem()` called on primitive type",
                        vec![
                            Label::primary(
                                value.span,
                                "this value does not need to be copied to memory",
                            ),
                            Label::secondary(method_name.span, "hint: remove `.to_mem()`"),
                        ],
                        vec![],
                    );
                }
                Location::Memory => {
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
            }
            Ok((value_attrs.into_cloned(), calltype))
        }
        ValueMethod::AbiEncode => match &value_attrs.typ {
            Type::Struct(struct_) => {
                if value_attrs.final_location() != Location::Memory {
                    context.fancy_error(
                        "value must be copied to memory",
                        vec![Label::primary(value.span, "this value is in storage")],
                        vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                             "Example: `self.my_array.to_mem().abi_encode()`".into(),
                        ],
                    );
                }

                Ok((
                    ExpressionAttributes::new(
                        Type::Array(Array {
                            inner: Base::Numeric(Integer::U8),
                            size: struct_.id.fields(context.db()).len() * 32,
                        }),
                        Location::Memory,
                    ),
                    calltype,
                ))
            }
            Type::Tuple(tuple) => {
                if value_attrs.final_location() != Location::Memory {
                    context.fancy_error(
                        "value must be copied to memory",
                        vec![Label::primary(value.span, "this value is in storage")],
                        vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                             "Example: `self.my_array.to_mem().abi_encode()`".into(),
                        ],
                    );
                }

                Ok((
                    ExpressionAttributes::new(
                        Type::Array(Array {
                            inner: Base::Numeric(Integer::U8),
                            size: tuple.items.len() * 32,
                        }),
                        Location::Memory,
                    ),
                    calltype,
                ))
            }
            _ => Err(FatalError::new(context.fancy_error(
                &format!(
                    "value of type `{}` does not support `abi_encode()`",
                    value_attrs.typ
                ),
                vec![Label::primary(
                    value.span,
                    "this value cannot be encoded using `abi_encode()`",
                )],
                vec![
                    "Hint: struct and tuple values can be encoded.".into(),
                    "Example: `(42,).abi_encode()`".into(),
                ],
            ))),
        },
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

    if let Some(class) = typ.as_class() {
        let class_name = class.name(context.db());

        if let Class::Contract(contract) = class {
            // Check for Foo.create/create2 (this will go away when the context object is
            // ready)
            if let Ok(function) = ContractTypeMethod::from_str(&field.kind) {
                if context.root_item() == Item::Type(TypeDef::Contract(contract)) {
                    context.fancy_error(
                        &format!("`{contract}.{}(...)` called within `{contract}` creates an illegal circular dependency", function.as_ref(), contract=&class_name),
                        vec![Label::primary(field.span, "Contract creation")],
                        vec![format!("Note: Consider using a dedicated factory contract to create instances of `{}`", &class_name)]);
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
                            if let Some(context_type) = context.get_context_type() {
                                if attrs.typ != Type::Struct(context_type) {
                                    context.fancy_error(
                                        &format!(
                                            "incorrect type for argument to `{}.{}`",
                                            &class_name,
                                            function.as_ref()
                                        ),
                                        vec![Label::primary(
                                            args.kind[i].span,
                                            format!(
                                                "this has type `{}`; expected `Context`",
                                                &attrs.typ
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
                                            "`ctx` must be defined and passed into the function",
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
                                    vec![
                                        "Note: import context with `use std::context::Context`"
                                            .into(),
                                        "Example: `MyContract.create(ctx, 0)`".into(),
                                    ],
                                );
                            }
                        } else if !matches!(&attrs.typ, Type::Base(Base::Numeric(_))) {
                            context.fancy_error(
                                &format!(
                                    "incorrect type for argument to `{}.{}`",
                                    &class_name,
                                    function.as_ref()
                                ),
                                vec![Label::primary(
                                    args.kind[i].span,
                                    format!("this has type `{}`; expected a number", &attrs.typ),
                                )],
                                vec![],
                            );
                        }
                    }
                }
                return Ok((
                    ExpressionAttributes::new(typ, Location::Value),
                    CallType::BuiltinAssociatedFunction { contract, function },
                ));
            }
        }

        if let Some(function) = class.function(context.db(), &field.kind) {
            if function.takes_self(context.db()) {
                return Err(FatalError::new(context.fancy_error(
                    &format!(
                        "`{}` function `{}` must be called on an instance of `{}`",
                        &class_name, &field.kind, &class_name,
                    ),
                    vec![Label::primary(
                        target_span,
                        format!(
                            "expected a value of type `{}`, found type name",
                            &class_name
                        ),
                    )],
                    vec![],
                )));
            }

            // Returns `true` if the current contract belongs to the same class as an input
            // `callee_class`.
            let is_same_class = |callee_class| match (context.root_item(), callee_class) {
                (Item::Type(TypeDef::Contract(caller)), Class::Contract(callee)) => {
                    caller == callee
                }
                (Item::Type(TypeDef::Struct(caller)), Class::Struct(callee)) => caller == callee,
                _ => false,
            };

            // Check for visibility of callee.
            // Emits an error if callee is not public and caller doesn't belong to the same
            // class as callee.
            if !function.is_public(context.db()) && is_same_class(class) {
                context.fancy_error(
                    &format!(
                        "the function `{}.{}` is private",
                        &class_name,
                        &field.kind,
                    ),
                    vec![
                        Label::primary(field.span, "this function is not `pub`"),
                        Label::secondary(
                            function.data(context.db()).ast.span,
                            format!("`{}` is defined here", &field.kind),
                        ),
                    ],
                    vec![
                        format!("`{}.{}` can only be called from other functions within `{}`", &class_name, &field.kind, &class_name),
                        format!("Hint: use `pub fn {fun}(..` to make `{cls}.{fun}` callable from outside of `{cls}`", fun=&field.kind, cls=&class_name),
                    ],
                );
            }

            if matches!(class, Class::Contract(_)) {
                // TODO: `MathLibContract::square(x)` can't be called yet, because yulgen
                // doesn't compile-in pure functions defined on external
                // contracts. We should also discuss how/if this will work when
                // the external contract is deployed and called via STATICCALL
                // or whatever.
                context.not_yet_implemented(
                    &format!("calling contract-associated pure functions. Consider moving `{}` outside of `{}`",
                             &field.kind, class_name),
                    target_span + field.span,
                );
            }

            let ret_type = function.signature(context.db()).return_type.clone()?;
            let location = Location::assign_location(&ret_type);
            return Ok((
                ExpressionAttributes::new(ret_type.into(), location),
                CallType::AssociatedFunction { class, function },
            ));
        }
    }

    Err(FatalError::new(context.fancy_error(
        &format!("No function `{}` exists on type `{}`", &field.kind, &typ),
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
        .map(|arg| assignable_expr(context, &arg.kind.value, None))
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
            &format!("`{}()` is not directly callable", name),
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
            &format!("literal out of range for `{}`", int_type),
            span,
            &format!("does not fit into type `{}`", int_type),
        );
    }
}

fn validate_str_literal_fits_type(
    context: &mut dyn AnalyzerContext,
    arg_val: &Node<fe::Expr>,
    typ: &FeString,
) {
    if let fe::Expr::Str(string) = &arg_val.kind {
        if string.len() > typ.max_size {
            context.error(
                "string capacity exceeded",
                arg_val.span,
                &format!(
                    "this string has length {}; expected length <= {}",
                    string.len(),
                    typ.max_size
                ),
            );
        }
    } else {
        context.error("type mismatch", arg_val.span, "expected a string literal");
    }
}

fn expr_comp_operation(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::CompOperation { left, op, right } = &exp.kind {
        // comparison operands should be moved to the stack
        let left_attr = value_expr(context, left, None)?;
        let right_attr = value_expr(context, right, Some(&left_attr.typ))?;

        if left_attr.typ != right_attr.typ {
            context.fancy_error(
                &format!("`{}` operands must have the same type", op.kind),
                vec![
                    Label::primary(left.span, format!("this has type `{}`", left_attr.typ)),
                    Label::secondary(
                        right.span,
                        format!("this has incompatible type `{}`", right_attr.typ),
                    ),
                ],
                vec![],
            );
        }

        // for now we assume these are the only possible attributes
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Bool),
            Location::Value,
        ));
    }

    unreachable!()
}

fn expr_ternary(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Ternary {
        if_expr,
        test,
        else_expr,
    } = &exp.kind
    {
        // test attributes should be stored as a value
        let test_attributes = value_expr(context, test, None)?;
        // the return expressions should be stored in their default locations
        //
        // If, for example, one of the expressions is stored in memory and the other is
        // stored in storage, it's necessary that we move them to the same location.
        // This could be memory or the stack, depending on the type.
        let if_expr_attributes = assignable_expr(context, if_expr, None)?;
        let else_expr_attributes =
            assignable_expr(context, else_expr, Some(&if_expr_attributes.typ))?;

        // Make sure the `test_attributes` is a boolean type.
        if Type::Base(Base::Bool) != test_attributes.typ {
            context.error(
                "`if` test expression must be a `bool`",
                test.span,
                &format!("this has type `{}`; expected `bool`", test_attributes.typ),
            );
        }
        // Should have the same return Type
        if if_expr_attributes.typ != else_expr_attributes.typ {
            context.fancy_error(
                "`if` and `else` values must have same type",
                vec![
                    Label::primary(
                        if_expr.span,
                        format!("this has type `{}`", if_expr_attributes.typ),
                    ),
                    Label::secondary(
                        else_expr.span,
                        format!("this has type `{}`", else_expr_attributes.typ),
                    ),
                ],
                vec![],
            );
        }

        let loc = if_expr_attributes.final_location();
        return Ok(ExpressionAttributes::new(if_expr_attributes.typ, loc));
    }
    unreachable!()
}

fn expr_bool_operation(
    context: &mut dyn AnalyzerContext,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::BoolOperation { left, op, right } = &exp.kind {
        for operand in &[left, right] {
            let attributes = value_expr(context, operand, None)?;
            if attributes.typ != Type::Base(Base::Bool) {
                context.error(
                    &format!("binary op `{}` operands must have type `bool`", op.kind),
                    operand.span,
                    &format!("this has type `{}`; expected `bool`", attributes.typ),
                );
            }
        }
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Bool),
            Location::Value,
        ));
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
