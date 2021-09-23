use crate::builtins::{
    BlockField, ChainField, ContractTypeMethod, GlobalMethod, MsgField, Object, SelfField, TxField,
    ValueMethod,
};
use crate::context::{AnalyzerContext, CallType, ExpressionAttributes, Location};
use crate::errors::{FatalError, IndexingError, NotFixedSize};
use crate::namespace::scopes::BlockScope;
use crate::namespace::types::{
    Array, Base, Contract, FeString, Integer, SelfDecl, Struct, Tuple, Type, TypeDowncast, U256,
};
use crate::operations;
use crate::traversal::call_args::{validate_arg_count, validate_named_args, LabelPolicy};
use crate::traversal::types::resolve_type_name;
use crate::traversal::utils::{add_bin_operations_errors, types_to_fixed_sizes};
use fe_common::diagnostics::Label;
use fe_common::numeric;
use fe_common::Span;
use fe_parser::ast as fe;
use fe_parser::ast::UnaryOperator;
use fe_parser::node::Node;
use num_bigint::BigInt;
use std::convert::TryInto;
use std::ops::RangeInclusive;
use std::str::FromStr;
use vec1::Vec1;

/// Gather context information for expressions and check for type errors.
pub fn expr(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    let attributes = match &exp.kind {
        fe::Expr::Name(_) => expr_name(scope, exp, expected_type),
        fe::Expr::Num(_) => Ok(expr_num(scope, exp, expected_type.as_int())),
        fe::Expr::Bool(_) => expr_bool(exp),
        fe::Expr::Subscript { .. } => expr_subscript(scope, exp),
        fe::Expr::Attribute { .. } => expr_attribute(scope, exp),
        fe::Expr::Ternary { .. } => expr_ternary(scope, exp),
        fe::Expr::BoolOperation { .. } => expr_bool_operation(scope, exp),
        fe::Expr::BinOperation { .. } => expr_bin_operation(scope, exp, expected_type.as_int()),
        fe::Expr::UnaryOperation { .. } => expr_unary_operation(scope, exp, expected_type),
        fe::Expr::CompOperation { .. } => expr_comp_operation(scope, exp),
        fe::Expr::Call { .. } => expr_call(scope, exp),
        fe::Expr::List { elts } => expr_list(scope, elts, expected_type.as_array()),
        fe::Expr::Tuple { .. } => expr_tuple(scope, exp, expected_type.as_tuple()),
        fe::Expr::Str(_) => expr_str(scope, exp),
        fe::Expr::Unit => Ok(ExpressionAttributes::new(Type::unit(), Location::Value)),
    }?;

    scope.root.add_expression(exp, attributes.clone());
    Ok(attributes)
}

pub fn expr_list(
    scope: &mut BlockScope,
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
        });
    }

    let inner_type = if let Some(expected) = expected_type {
        for elt in elts {
            let element_attributes =
                assignable_expr(scope, elt, Some(&Type::Base(expected.inner)))?;
            if element_attributes.typ != Type::Base(expected.inner) {
                scope.type_error(
                    "type mismatch",
                    elt.span,
                    &expected.inner,
                    &element_attributes.typ,
                );
            }
        }
        expected.inner
    } else {
        let first_attr = assignable_expr(scope, &elts[0], None)?;
        let inner = match first_attr.typ {
            Type::Base(base) => base,
            _ => {
                return Err(FatalError::new(scope.error(
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
            let element_attributes = assignable_expr(scope, elt, Some(&first_attr.typ))?;
            if element_attributes.typ != first_attr.typ {
                scope.fancy_error(
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
    })
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is on the stack.
pub fn value_expr(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    let original_attributes = expr(scope, exp, expected_type)?;
    let attributes = original_attributes.clone().into_loaded().map_err(|_| {
        FatalError::new(scope.fancy_error(
            "can't move value onto stack",
            vec![Label::primary(exp.span, "Value to be moved")],
            vec![format!(
                "Note: Can't move `{}` types on the stack",
                original_attributes.typ
            )],
        ))
    })?;

    scope.root.update_expression(exp, attributes.clone());

    Ok(attributes)
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is in the type's assigment location.
pub fn assignable_expr(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    use Type::*;

    let mut attributes = expr(scope, exp, expected_type)?;
    match &attributes.typ {
        Base(_) | Contract(_) => {
            if attributes.location != Location::Value {
                attributes.move_location = Some(Location::Value);
            }
        }
        Array(_) | Tuple(_) | String(_) | Struct(_) => {
            if attributes.final_location() != Location::Memory {
                scope.fancy_error(
                    "value must be copied to memory",
                    vec![Label::primary(exp.span, "this value is in storage")],
                    vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                         "Example: `self.my_array.to_mem()`".into(),
                    ],
                );
                attributes.move_location = Some(Location::Memory);
            }
        }
        Map(_) => {
            return Err(FatalError::new(scope.error(
                "`Map` type cannot reside in memory",
                exp.span,
                "this type can only be used in a contract field",
            )));
        }
    };
    scope.root.update_expression(exp, attributes.clone());

    Ok(attributes)
}

fn expr_tuple(
    scope: &mut BlockScope,
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
                assignable_expr(scope, elt, exp_type.as_ref()).map(|attributes| attributes.typ)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let tuple_types = match types_to_fixed_sizes(&types) {
            Err(NotFixedSize) => {
                return Err(FatalError::new(scope.error(
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
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    let name = match &exp.kind {
        fe::Expr::Name(name) => name,
        _ => unreachable!(),
    };
    match scope.var_type(name) {
        Some(Ok(typ)) => {
            let location = Location::assign_location(&typ);
            Ok(ExpressionAttributes::new(typ.into(), location))
        }
        Some(Err(err)) => Err(err.into()),
        None => {
            let voucher = scope.error(
                &format!("cannot find value `{}` in this scope", name),
                exp.span,
                "undefined",
            );
            match expected_type {
                Some(typ) => Ok(ExpressionAttributes::new(
                    typ.clone(),
                    Location::assign_location(
                        &typ.clone()
                            .try_into()
                            .map_err(|_| FatalError::new(voucher))?,
                    ),
                )),
                None => Err(FatalError::new(voucher)),
            }
        }
    }
}

fn expr_str(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Str(string) = &exp.kind {
        if !is_valid_string(string) {
            scope.error("String contains invalid byte sequence", exp.span, "");
        };

        return Ok(ExpressionAttributes::new(
            Type::String(FeString {
                max_size: string.len(),
            }),
            Location::Memory,
        ));
    }

    unreachable!()
}

fn is_valid_string(val: &str) -> bool {
    const ALLOWED_SPECIAL_CHARS: [u8; 3] = [
        9u8,  // Tab
        10u8, // Newline
        13u8, // Carriage return
    ];

    const PRINTABLE_ASCII: RangeInclusive<u8> = 31u8..=126u8;

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
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<Integer>,
) -> ExpressionAttributes {
    if let fe::Expr::Num(num) = &exp.kind {
        let int_typ = expected_type.unwrap_or(Integer::U256);
        let num = to_bigint(num);
        validate_numeric_literal_fits_type(scope, num, exp.span, int_typ);
        return ExpressionAttributes::new(Type::int(int_typ), Location::Value);
    }

    unreachable!()
}

fn expr_subscript(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Subscript { value, index } = &exp.kind {
        let value_attributes = expr(scope, value, None)?;
        let index_attributes = value_expr(scope, index, None)?;

        // performs type checking
        let typ =
            match operations::index(value_attributes.typ.clone(), index_attributes.typ.clone()) {
                Err(IndexingError::NotSubscriptable) => {
                    return Err(FatalError::new(scope.fancy_error(
                        &format!("`{}` type is not subscriptable", value_attributes.typ),
                        vec![Label::primary(value.span, "unsubscriptable type")],
                        vec!["Note: Only arrays and maps are subscriptable".into()],
                    )));
                }
                Err(IndexingError::WrongIndexType) => {
                    return Err(FatalError::new(scope.fancy_error(
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
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Attribute { value, attr } = &exp.kind {
        let base_type = |typ| Ok(ExpressionAttributes::new(Type::Base(typ), Location::Value));

        // If the value is a name, check if it is a builtin object and attribute.
        if let fe::Expr::Name(name) = &value.kind {
            match Object::from_str(name) {
                Ok(Object::Self_) => return expr_attribute_self(scope, value, attr),
                Ok(Object::Block) => {
                    return match BlockField::from_str(&attr.kind) {
                        Ok(BlockField::Coinbase) => base_type(Base::Address),
                        Ok(BlockField::Difficulty) => base_type(U256),
                        Ok(BlockField::Number) => base_type(U256),
                        Ok(BlockField::Timestamp) => base_type(U256),
                        Err(_) => {
                            Err(FatalError::new(scope.fancy_error(
                                "Not a block field",
                                vec![
                                    Label::primary(
                                        attr.span,
                                        "",
                                    ),
                                ],
                                vec!["Note: Only `coinbase`, `difficulty`, `number` and `timestamp` can be accessed on `block`.".into()],
                            )))
                        }
                    }
                }
                Ok(Object::Chain) => {
                    return match ChainField::from_str(&attr.kind) {
                        Ok(ChainField::Id) => base_type(U256),
                        Err(_) => {
                            Err(FatalError::new(scope.fancy_error(
                                "Not a chain field",
                                vec![Label::primary(attr.span, "")],
                                vec!["Note: Only `id` can be accessed on `chain`.".into()],
                            )))
                        }
                    }
                }
                Ok(Object::Msg) => {
                    return match MsgField::from_str(&attr.kind) {
                        Ok(MsgField::Sender) => base_type(Base::Address),
                        Ok(MsgField::Sig) => base_type(U256),
                        Ok(MsgField::Value) => base_type(U256),
                        Err(_) => {
                            Err(FatalError::new(scope.fancy_error(
                                "Not a `msg` field",
                                vec![
                                    Label::primary(
                                        attr.span,
                                        "",
                                    ),
                                ],
                                vec!["Note: Only `sender`, `sig` and `value` can be accessed on `msg`.".into()],
                            )))
                        }
                    }
                }
                Ok(Object::Tx) => {
                    return match TxField::from_str(&attr.kind) {
                        Ok(TxField::GasPrice) => base_type(U256),
                        Ok(TxField::Origin) => base_type(Base::Address),
                        Err(_) => {
                            Err(FatalError::new(scope.fancy_error(
                                "Not a `tx` field",
                                vec![Label::primary(attr.span, "")],
                                vec![
                                    "Note: Only `gas_price` and `origin` can be accessed on `tx`."
                                        .into(),
                                ],
                            )))
                        }
                    }
                }
                Err(_) => {}
            }
        }

        // We attempt to analyze the value as an expression. If this is successful, we
        // build a new set of attributes from the value attributes.
        let expression_attributes = expr(scope, value, None)?;
        return match expression_attributes {
            // If the value is a struct, we return the type of the attribute. The location stays the
            // same and can be memory or storage.
            ExpressionAttributes {
                typ: Type::Struct(struct_),
                location,
                ..
            } => {
                if let Some(field) = struct_.id.field(scope.db(), &attr.kind) {
                    Ok(ExpressionAttributes::new(
                        field.typ(scope.db())?.into(),
                        location,
                    ))
                } else {
                    Err(FatalError::new(scope.fancy_error(
                        &format!(
                            "No field `{}` exists on struct `{}`",
                            &attr.kind, struct_.name
                        ),
                        vec![Label::primary(attr.span, "undefined field")],
                        vec![],
                    )))
                }
            }
            ExpressionAttributes {
                typ: Type::Tuple(tuple),
                location,
                ..
            } => {
                let item_index = match tuple_item_index(&attr.kind) {
                    Some(index) => index,
                    None => {
                        return Err(FatalError::new(scope.fancy_error(
                            &format!("No field `{}` exists on this tuple", &attr.kind),
                            vec![
                                Label::primary(
                                    attr.span,
                                    "undefined field",
                                )
                            ],
                            vec!["Note: Tuple values are accessed via `itemN` properties such as `item0` or `item1`".into()],
                        )));
                    }
                };

                if let Some(typ) = tuple.items.get(item_index) {
                    Ok(ExpressionAttributes::new(typ.to_owned().into(), location))
                } else {
                    Err(FatalError::new(scope.fancy_error(
                        &format!("No field `item{}` exists on this tuple", item_index),
                        vec![Label::primary(attr.span, "unknown field")],
                        vec![format!(
                            "Note: The highest possible field for this tuple is `item{}`",
                            tuple.items.len() - 1
                        )],
                    )))
                }
            }
            _ => Err(FatalError::new(scope.fancy_error(
                &format!(
                    "No field `{}` exists on type {}",
                    &attr.kind, expression_attributes.typ
                ),
                vec![Label::primary(attr.span, "unknown field")],
                vec![],
            ))),
        };
    }

    unreachable!()
}

/// Pull the item index from the attribute string (e.g. "item4" -> "4").
fn tuple_item_index(item: &str) -> Option<usize> {
    if item.len() < 5 || &item[..4] != "item" || (item.len() > 5 && &item[4..5] == "0") {
        None
    } else {
        item[4..].parse::<usize>().ok()
    }
}

fn expr_attribute_self(
    scope: &mut BlockScope,
    value: &Node<fe::Expr>,
    attr: &Node<String>,
) -> Result<ExpressionAttributes, FatalError> {
    check_for_self_param(scope, value.span);

    if let Ok(SelfField::Address) = SelfField::from_str(&attr.kind) {
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Address),
            Location::Value,
        ));
    }

    match scope.root.contract_field(&attr.kind) {
        Some((typ, nonce)) => Ok(ExpressionAttributes::new(
            typ?,
            Location::Storage { nonce: Some(nonce) },
        )),
        None => Err(FatalError::new(scope.fancy_error(
            &format!("No field `{}` exists on this contract", &attr.kind),
            vec![Label::primary(attr.span, "undefined field")],
            vec![],
        ))),
    }
}

fn expr_bin_operation(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<Integer>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::BinOperation { left, op, right } = &exp.kind {
        let expected = expected_type.map(Type::int);
        let left_attributes = value_expr(scope, left, expected.as_ref())?;
        let right_attributes = value_expr(scope, right, expected.as_ref())?;

        let typ = match operations::bin(&left_attributes.typ, &op.kind, &right_attributes.typ) {
            Err(err) => {
                return Err(FatalError::new(add_bin_operations_errors(
                    scope,
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
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::UnaryOperation { op, operand } = &exp.kind {
        let operand_attributes = value_expr(scope, operand, None)?;

        let emit_err = |scope: &mut BlockScope, expected| {
            scope.error(
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
                            validate_numeric_literal_fits_type(scope, num, exp.span, int_type);
                        }
                    }
                    _ => emit_err(scope, "a numeric type"),
                }
                Ok(ExpressionAttributes::new(
                    Type::int(int_type),
                    Location::Value,
                ))
            }
            fe::UnaryOperator::Not => {
                if !matches!(operand_attributes.typ, Type::Base(Base::Bool)) {
                    emit_err(scope, "type `bool`");
                }
                Ok(ExpressionAttributes::new(
                    Type::Base(Base::Bool),
                    Location::Value,
                ))
            }
            UnaryOperator::Invert => {
                if !matches!(operand_attributes.typ, Type::Base(Base::Numeric(_))) {
                    emit_err(scope, "a numeric type")
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
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Call {
        func,
        generic_args,
        args,
    } = &exp.kind
    {
        return match expr_call_type(scope, func, generic_args.as_ref())? {
            CallType::BuiltinFunction { func: builtin } => {
                expr_call_builtin_function(scope, builtin, func.span, args)
            }
            CallType::TypeConstructor { typ } => {
                expr_call_type_constructor(scope, func.span, typ, args)
            }
            CallType::SelfAttribute {
                func_name,
                self_span,
            } => expr_call_self_attribute(scope, &func_name, func.span, self_span, args),
            CallType::Pure { func_name } => expr_call_pure(scope, &func_name, func.span, args),
            CallType::ValueAttribute => expr_call_value_attribute(scope, func, args),
            CallType::TypeAttribute { typ, func_name } => {
                expr_call_type_attribute(scope, typ, &func_name, func.span, args)
            }
        };
    }

    unreachable!()
}

fn expr_call_builtin_function(
    scope: &mut BlockScope,
    function: GlobalMethod,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    let argument_attributes = expr_call_args(scope, args)?;
    match function {
        GlobalMethod::Keccak256 => {
            validate_arg_count(scope, function.into(), name_span, args, 1);
            expect_no_label_on_first_arg(scope, args);

            if let Some(arg_typ) = argument_attributes.first().map(|attr| &attr.typ) {
                if !matches!(
                    arg_typ,
                    Type::Array(Array {
                        inner: Base::Numeric(Integer::U8),
                        ..
                    })
                ) {
                    let fn_name: &str = function.into();
                    scope.fancy_error(
                        &format!(
                            "`{}` can not be used as an argument to `{}`",
                            arg_typ, fn_name,
                        ),
                        vec![Label::primary(args.span, "wrong type")],
                        vec![format!("Note: `{}` expects a byte array argument", fn_name)],
                    );
                }
            };

            Ok(ExpressionAttributes::new(Type::Base(U256), Location::Value))
        }
    }
}

fn expr_call_struct_constructor(
    scope: &mut BlockScope,
    name_span: Span,
    struct_: Struct,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    let db = scope.root.db;
    let fields = struct_
        .id
        .fields(db)
        .iter()
        .map(|(name, field)| (name.clone(), field.typ(db)))
        .collect::<Vec<_>>();
    validate_named_args(
        scope,
        &struct_.name,
        name_span,
        args,
        &fields,
        LabelPolicy::AllowUnlabledIfNameEqual,
    )?;

    Ok(ExpressionAttributes::new(
        Type::Struct(struct_),
        Location::Memory,
    ))
}

fn expect_no_label_on_first_arg(scope: &mut BlockScope, args: &Node<Vec<Node<fe::CallArg>>>) {
    if let Some(label) = args.kind.first().and_then(|arg| arg.kind.label.as_ref()) {
        scope.error(
            "argument should not be labeled",
            label.span,
            "remove this label",
        );
    }
}

fn expr_call_type_constructor(
    scope: &mut BlockScope,
    name_span: Span,
    typ: Type,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    match typ {
        Type::Struct(struct_type) => {
            return expr_call_struct_constructor(scope, name_span, struct_type, args)
        }
        Type::Base(Base::Bool) => {
            return Err(FatalError::new(scope.error(
                "`bool` type is not callable",
                name_span,
                "",
            )))
        }
        Type::Map(_) => {
            return Err(FatalError::new(scope.error(
                "`Map` type is not callable",
                name_span,
                "",
            )))
        }
        _ => {}
    }

    // These all expect 1 arg, for now.
    validate_arg_count(scope, &format!("{}", typ), name_span, args, 1);
    expect_no_label_on_first_arg(scope, args);

    match &typ {
        Type::String(string_type) => {
            if let Some(arg) = args.kind.first() {
                assignable_expr(scope, &arg.kind.value, None)?;
                validate_str_literal_fits_type(scope, &arg.kind.value, string_type);
            }
            Ok(ExpressionAttributes::new(typ, Location::Memory))
        }
        Type::Contract(_) => {
            if let Some(arg) = args.kind.first() {
                let arg_attr = assignable_expr(scope, &arg.kind.value, None)?;
                if arg_attr.typ != Type::Base(Base::Address) {
                    scope.type_error("type mismatch", arg.span, &Base::Address, &typ);
                }
            }
            Ok(ExpressionAttributes::new(typ, Location::Value))
        }
        Type::Base(Base::Numeric(_)) => {
            if let Some(arg) = args.kind.first() {
                // This will check if the literal fits inside int_type.
                assignable_expr(scope, &arg.kind.value, Some(&typ))?;
                validate_is_numeric_literal(scope, &arg.kind.value);
            }
            Ok(ExpressionAttributes::new(typ, Location::Value))
        }
        Type::Base(Base::Address) => {
            if let Some(arg) = args.kind.first() {
                let arg_attr = assignable_expr(scope, &arg.kind.value, None)?;
                match arg_attr.typ {
                    Type::Contract(_)
                    | Type::Base(Base::Numeric(_))
                    | Type::Base(Base::Address) => {}
                    _ => {
                        scope.fancy_error(
                            &format!("`{}` can not be used as a parameter to `address(..)`", arg_attr.typ),
                            vec![Label::primary(arg.span, "wrong type")],
                            vec!["Note: address(..) expects a parameter of a contract type, numeric or address".into()],
                        );
                    }
                }
            };
            Ok(ExpressionAttributes::new(
                Type::Base(Base::Address),
                Location::Value,
            ))
        }
        Type::Base(Base::Unit) => unreachable!(), // rejected in expr_call_type
        Type::Base(Base::Bool) => unreachable!(), // handled above
        Type::Tuple(_) => unreachable!(),         // rejected in expr_call_type
        Type::Struct(_) => unreachable!(),        // handled above
        Type::Map(_) => unreachable!(),           // handled above
        // The current array type syntax is parsed as an index operation in this case (eg `u8[10](5)`),
        // and won't end up here.
        Type::Array(_) => unreachable!(),
    }
}

fn expr_call_args(
    scope: &mut BlockScope,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<Vec<ExpressionAttributes>, FatalError> {
    args.kind
        .iter()
        .map(|arg| assignable_expr(scope, &arg.kind.value, None))
        .collect::<Result<Vec<_>, _>>()
}

fn check_for_call_to_init_fn(
    scope: &mut BlockScope,
    name: &str,
    span: Span,
) -> Result<(), FatalError> {
    if name == "__init__" {
        Err(FatalError::new(scope.fancy_error(
            "`__init__()` is not directly callable",
            vec![Label::primary(span, "")],
            vec![
                "Note: `__init__` is the constructor function, and can't be called at runtime."
                    .into(),
            ],
        )))
    } else {
        Ok(())
    }
}

fn check_for_self_param(scope: &mut BlockScope, use_span: Span) {
    if scope.root.function.signature(scope.db()).self_decl == SelfDecl::None {
        scope.fancy_error(
            "`self` is not defined",
            vec![Label::primary(use_span, "undefined value")],
            vec![
                "add `self` to the scope by including it in the function signature".to_string(),
                "Example: `fn foo(self, bar: bool)`".to_string(),
            ],
        );
    }
}

fn expr_call_self_attribute(
    scope: &mut BlockScope,
    func_name: &str,
    name_span: Span,
    self_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    check_for_call_to_init_fn(scope, func_name, name_span)?;
    check_for_self_param(scope, self_span);

    if let Some(func) = scope.root.self_contract_function(func_name) {
        let sig = func.signature(scope.root.db);
        validate_named_args(
            scope,
            func_name,
            name_span,
            args,
            &sig.params,
            LabelPolicy::AllowAnyUnlabeled,
        )?;

        let return_type = sig.return_type.clone()?;
        let return_location = Location::assign_location(&return_type);
        Ok(ExpressionAttributes::new(
            return_type.into(),
            return_location,
        ))
    } else {
        let voucher = if scope.root.pure_contract_function(func_name).is_none() {
            scope.fancy_error(
                &format!("no function named `{}` exists in this contract", func_name),
                vec![Label::primary(name_span, "undefined function")],
                vec![],
            )
        } else {
            scope.fancy_error(
                &format!("`{}` must be called without `self`", func_name),
                vec![Label::primary(name_span, "function does not take self")],
                vec![format!(
                    "Suggestion: try `{}(...)` instead of `self.{}(...)`",
                    func_name, func_name
                )],
            )
        };
        Err(FatalError::new(voucher))
    }
}

fn expr_call_pure(
    scope: &mut BlockScope,
    func_name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    check_for_call_to_init_fn(scope, func_name, name_span)?;

    if let Some(func) = scope.root.pure_contract_function(func_name) {
        let sig = func.signature(scope.root.db);
        validate_named_args(
            scope,
            func_name,
            name_span,
            args,
            &sig.params,
            LabelPolicy::AllowAnyUnlabeled,
        )?;

        let return_type = sig.return_type.clone()?;
        let return_location = Location::assign_location(&return_type);
        Ok(ExpressionAttributes::new(
            return_type.into(),
            return_location,
        ))
    } else {
        let voucher = if scope.root.self_contract_function(func_name).is_none() {
            scope.fancy_error(
                &format!("no function named `{}` exists in this contract", func_name),
                vec![Label::primary(name_span, "undefined function")],
                vec![],
            )
        } else {
            scope.fancy_error(
                &format!("`{}` must be called using `self`", func_name),
                vec![Label::primary(name_span, "function takes self")],
                vec![format!(
                    "Suggestion: try `self.{}(...)` instead of `{}(...)`",
                    func_name, func_name
                )],
            )
        };
        Err(FatalError::new(voucher))
    }
}

fn expr_call_value_attribute(
    scope: &mut BlockScope,
    func: &Node<fe::Expr>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Attribute { value, attr } = &func.kind {
        let value_attributes = expr(scope, value, None)?;

        if let Type::Contract(contract) = &value_attributes.typ {
            // We must ensure the expression is loaded onto the stack.
            let expression = value_attributes.clone().into_loaded().map_err(|_| {
                // TODO: Add test code that triggers this
                FatalError::new(scope.fancy_error(
                    "can't move value onto stack",
                    vec![Label::primary(value.span, "Value to be moved")],
                    vec![format!(
                        "Note: Can't move `{}` types on the stack",
                        value_attributes.typ
                    )],
                ))
            })?;

            scope.root.update_expression(value, expression);
            return expr_call_contract_attribute(
                scope,
                contract.to_owned(),
                &attr.kind,
                attr.span,
                args,
            );
        }

        // for now all of these function expect 0 arguments
        validate_arg_count(scope, &attr.kind, attr.span, args, 0);

        return match ValueMethod::from_str(&attr.kind) {
            Err(_) => {
                return Err(FatalError::new(scope.fancy_error(
                    &format!(
                        "No function `{}` exists on type `{}`",
                        &attr.kind, &value_attributes.typ
                    ),
                    vec![Label::primary(attr.span, "undefined function")],
                    vec![],
                )));
            }
            Ok(ValueMethod::Clone) => {
                match value_attributes.location {
                    Location::Storage { .. } => {
                        scope.fancy_error(
                            "`clone()` called on value in storage",
                            vec![
                                Label::primary(value.span, "this value is in storage"),
                                Label::secondary(attr.span, "hint: try `to_mem()` here"),
                            ],
                            vec![],
                        );
                    }
                    Location::Value => {
                        scope.fancy_error(
                            "`clone()` called on primitive type",
                            vec![
                                Label::primary(value.span, "this value does not need to be cloned"),
                                Label::secondary(attr.span, "hint: remove `.clone()`"),
                            ],
                            vec![],
                        );
                    }
                    Location::Memory => {}
                }
                Ok(value_attributes.into_cloned())
            }
            Ok(ValueMethod::ToMem) => {
                match value_attributes.location {
                    Location::Storage { .. } => {}
                    Location::Value => {
                        scope.fancy_error(
                            "`to_mem()` called on primitive type",
                            vec![
                                Label::primary(
                                    value.span,
                                    "this value does not need to be copied to memory",
                                ),
                                Label::secondary(attr.span, "hint: remove `.to_mem()`"),
                            ],
                            vec![],
                        );
                    }
                    Location::Memory => {
                        scope.fancy_error(
                            "`to_mem()` called on value in memory",
                            vec![
                                Label::primary(value.span, "this value is in storage"),
                                Label::secondary(
                                    attr.span,
                                    "hint: to make a copy, use `.to_mem()` here",
                                ),
                            ],
                            vec![],
                        );
                    }
                }
                Ok(value_attributes.into_cloned())
            }
            Ok(ValueMethod::AbiEncode) => match &value_attributes.typ {
                Type::Struct(struct_) => {
                    if value_attributes.final_location() != Location::Memory {
                        scope.fancy_error(
                            "value must be copied to memory",
                            vec![Label::primary(value.span, "this value is in storage")],
                            vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                                 "Example: `self.my_array.to_mem().abi_encode()`".into(),
                            ],
                        );
                    }

                    Ok(ExpressionAttributes::new(
                        Type::Array(Array {
                            inner: Base::Numeric(Integer::U8),
                            size: struct_.id.fields(scope.db()).len() * 32,
                        }),
                        Location::Memory,
                    ))
                }
                Type::Tuple(tuple) => {
                    if value_attributes.final_location() != Location::Memory {
                        scope.fancy_error(
                            "value must be copied to memory",
                            vec![Label::primary(value.span, "this value is in storage")],
                            vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                                 "Example: `self.my_array.to_mem().abi_encode()`".into(),
                            ],
                        );
                    }

                    Ok(ExpressionAttributes::new(
                        Type::Array(Array {
                            inner: Base::Numeric(Integer::U8),
                            size: tuple.items.len() * 32,
                        }),
                        Location::Memory,
                    ))
                }
                _ => {
                    scope.fancy_error(
                        &format!(
                            "value of type {} does not support `abi_encode()`",
                            value_attributes.typ
                        ),
                        vec![Label::primary(
                            value.span,
                            "this value cannot be encoded using `abi_encode()`",
                        )],
                        vec![
                            "Hint: struct and tuple values can be encoded.".into(),
                            "Example: `(42,).abi_encode()`".into(),
                        ],
                    );

                    Ok(ExpressionAttributes::new(Type::unit(), Location::Value))
                }
            },
        };
    }

    unreachable!()
}

fn expr_call_type_attribute(
    scope: &mut BlockScope,
    typ: Type,
    func_name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    let arg_attributes = expr_call_args(scope, args)?;
    let contract_name = scope.contract_name();

    let report_circular_dependency = |scope: &mut BlockScope, method: String| {
        scope.fancy_error(
            &format!("`{contract}.{}(...)` called within `{contract}` creates an illegal circular dependency", method, contract=contract_name),
            vec![Label::primary(name_span, "Contract creation")],
            vec![format!("Note: Consider using a dedicated factory contract to create instances of `{}`", contract_name)]);
    };

    // TODO: we should check for SomeContract.__init__() here and suggest create/create2

    match (typ.clone(), ContractTypeMethod::from_str(func_name)) {
        (Type::Contract(contract), Ok(ContractTypeMethod::Create2)) => {
            validate_arg_count(scope, func_name, name_span, args, 2);

            if contract_name == contract.name {
                report_circular_dependency(scope, ContractTypeMethod::Create2.to_string());
            }

            if !matches!(
                (&arg_attributes[0].typ, &arg_attributes[1].typ),
                (Type::Base(Base::Numeric(_)), Type::Base(Base::Numeric(_)))
            ) {
                scope.fancy_error(
                    "function `create2` expects numeric parameters",
                    vec![Label::primary(args.span, "invalid argument")],
                    vec![],
                );
            }
            Ok(ExpressionAttributes::new(
                Type::Contract(contract),
                Location::Value,
            ))
        }
        (Type::Contract(contract), Ok(ContractTypeMethod::Create)) => {
            validate_arg_count(scope, func_name, name_span, args, 1);

            if contract_name == contract.name {
                report_circular_dependency(scope, ContractTypeMethod::Create.to_string());
            }

            if !matches!(&arg_attributes[0].typ, Type::Base(Base::Numeric(_))) {
                scope.fancy_error(
                    "function `create` expects numeric parameter",
                    vec![Label::primary(args.span, "invalid argument")],
                    vec![],
                );
            }

            Ok(ExpressionAttributes::new(
                Type::Contract(contract),
                Location::Value,
            ))
        }
        _ => Err(FatalError::new(scope.fancy_error(
            &format!("No function `{}` exists on type `{}`", func_name, &typ),
            vec![Label::primary(name_span, "undefined function")],
            vec![],
        ))),
    }
}

fn expr_call_contract_attribute(
    scope: &mut BlockScope,
    contract: Contract,
    func_name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    check_for_call_to_init_fn(scope, func_name, name_span)?;

    if let Some(function) = contract.id.public_function(scope.db(), func_name) {
        let sig = function.signature(scope.db());
        let return_type = sig.return_type.clone()?;

        validate_named_args(
            scope,
            func_name,
            name_span,
            args,
            &sig.params,
            LabelPolicy::AllowAnyUnlabeled,
        )?;

        let location = Location::assign_location(&return_type);
        Ok(ExpressionAttributes::new(return_type.into(), location))
    } else if let Some(function) = contract.id.function(scope.db(), func_name) {
        Err(FatalError::new(scope.fancy_error(
            &format!(
                "The function `{}` on `contract {}` is private",
                func_name, contract.name
            ),
            vec![
                Label::primary(name_span, "this function is not `pub`"),
                Label::secondary(
                    function.data(scope.db()).ast.span,
                    format!("`{}` is defined here", func_name),
                ),
            ],
            vec![],
        )))
    } else {
        Err(FatalError::new(scope.fancy_error(
            &format!(
                "No function `{}` exists on contract `{}`",
                func_name, contract.name
            ),
            vec![Label::primary(name_span, "undefined function")],
            vec![],
        )))
    }
}

fn expr_call_type(
    scope: &mut BlockScope,
    func: &Node<fe::Expr>,
    generic_args: Option<&Node<Vec<fe::GenericArg>>>,
) -> Result<CallType, FatalError> {
    let call_type = match &func.kind {
        fe::Expr::Name(name) => expr_name_call_type(scope, name, func.span, generic_args),
        fe::Expr::Attribute { .. } => expr_attribute_call_type(scope, func),
        _ => {
            let expression = expr(scope, func, None)?;
            Err(FatalError::new(scope.fancy_error(
                &format!("`{}` type is not callable", expression.typ),
                vec![Label::primary(
                    func.span,
                    format!("this has type `{}`", expression.typ),
                )],
                vec![],
            )))
        }
    }?;

    scope.root.add_call(func, call_type.clone());
    Ok(call_type)
}

fn expr_name_call_type(
    scope: &mut BlockScope,
    name: &str,
    name_span: Span,
    generic_args: Option<&Node<Vec<fe::GenericArg>>>,
) -> Result<CallType, FatalError> {
    if let Some(typ) = resolve_type_name(scope, name, name_span, generic_args) {
        Ok(CallType::TypeConstructor { typ: typ? })
    } else if let Ok(func) = GlobalMethod::from_str(name) {
        if let Some(args) = generic_args {
            scope.error(
                &format!("`{}` function does not expect generic arguments", name),
                args.span,
                "unexpected generic argument list",
            );
        }
        Ok(CallType::BuiltinFunction { func })
    } else {
        Ok(CallType::Pure {
            func_name: name.to_string(),
        })
    }
}

fn expr_attribute_call_type(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<CallType, FatalError> {
    if let fe::Expr::Attribute { value, attr } = &exp.kind {
        if let fe::Expr::Name(name) = &value.kind {
            match Object::from_str(name) {
                Ok(Object::Block) | Ok(Object::Chain) | Ok(Object::Msg) | Ok(Object::Tx) => {
                    return Err(FatalError::new(scope.fancy_error(
                        &format!("no function `{}` exists on builtin `{}`", &attr.kind, &name),
                        vec![Label::primary(exp.span, "undefined function")],
                        vec![],
                    )));
                }
                Ok(Object::Self_) => {
                    return Ok(CallType::SelfAttribute {
                        func_name: attr.kind.to_string(),
                        self_span: value.span,
                    })
                }
                Err(_) => {}
            }

            if let Some(typ) = scope.resolve_type(name) {
                return Ok(CallType::TypeAttribute {
                    typ: typ?,
                    func_name: attr.kind.to_string(),
                });
            }
        };

        return Ok(CallType::ValueAttribute);
    }

    unreachable!()
}

fn validate_is_numeric_literal(scope: &mut BlockScope, value: &Node<fe::Expr>) -> Option<BigInt> {
    if let fe::Expr::UnaryOperation { operand, op: _ } = &value.kind {
        if let fe::Expr::Num(num) = &operand.kind {
            return Some(-to_bigint(num));
        }
    } else if let fe::Expr::Num(num) = &value.kind {
        return Some(to_bigint(num));
    }
    scope.error("type mismatch", value.span, "expected a number literal");
    None
}

fn validate_numeric_literal_fits_type(
    scope: &mut BlockScope,
    num: BigInt,
    span: Span,
    int_type: Integer,
) {
    if !int_type.fits(num) {
        scope.error(
            &format!("literal out of range for `{}`", int_type),
            span,
            &format!("does not fit into type `{}`", int_type),
        );
    }
}

fn validate_str_literal_fits_type(
    scope: &mut BlockScope,
    arg_val: &Node<fe::Expr>,
    typ: &FeString,
) {
    if let fe::Expr::Str(string) = &arg_val.kind {
        if string.len() > typ.max_size {
            scope.error(
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
        scope.error("type mismatch", arg_val.span, "expected a string literal");
    }
}

fn expr_comp_operation(
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::CompOperation { left, op, right } = &exp.kind {
        // comparison operands should be moved to the stack
        let left_attr = value_expr(scope, left, None)?;
        let right_attr = value_expr(scope, right, Some(&left_attr.typ))?;

        if left_attr.typ != right_attr.typ {
            scope.fancy_error(
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
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Ternary {
        if_expr,
        test,
        else_expr,
    } = &exp.kind
    {
        // test attributes should be stored as a value
        let test_attributes = value_expr(scope, test, None)?;
        // the return expressions should be stored in their default locations
        //
        // If, for example, one of the expressions is stored in memory and the other is
        // stored in storage, it's necessary that we move them to the same location.
        // This could be memory or the stack, depending on the type.
        let if_expr_attributes = assignable_expr(scope, if_expr, None)?;
        let else_expr_attributes =
            assignable_expr(scope, else_expr, Some(&if_expr_attributes.typ))?;

        // Make sure the `test_attributes` is a boolean type.
        if Type::Base(Base::Bool) != test_attributes.typ {
            scope.error(
                "`if` test expression must be a `bool`",
                test.span,
                &format!("this has type `{}`; expected `bool`", test_attributes.typ),
            );
        }
        // Should have the same return Type
        if if_expr_attributes.typ != else_expr_attributes.typ {
            scope.fancy_error(
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
    scope: &mut BlockScope,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::BoolOperation { left, op, right } = &exp.kind {
        for operand in &[left, right] {
            let attributes = value_expr(scope, operand, None)?;
            if attributes.typ != Type::Base(Base::Bool) {
                scope.error(
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
