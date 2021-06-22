use crate::context::{CallType, Context, ExpressionAttributes, Location};
use crate::errors::{FatalError, IndexingError, TypeError};
use crate::namespace::scopes::{BlockScope, ContractFunctionDef, Shared};
use crate::namespace::types::{
    Array, Base, Contract, FeString, FixedSize, Integer, Struct, Tuple, Type, TypeDowncast, U256,
};
use crate::operations;
use crate::traversal::utils::{add_bin_operations_errors, types_to_fixed_sizes};

use crate::builtins::{
    BlockField, ChainField, ContractTypeMethod, GlobalMethod, MsgField, Object, SelfField, TxField,
    ValueMethod,
};
use fe_common::numeric;
use fe_common::{diagnostics::Label, utils::humanize::pluralize_conditionally};
use fe_common::{Span, Spanned};
use fe_parser::ast as fe;
use fe_parser::ast::UnaryOperator;
use fe_parser::node::Node;
use num_bigint::BigInt;
use std::convert::TryFrom;
use std::rc::Rc;
use std::str::FromStr;
use vec1::Vec1;

/// Gather context information for expressions and check for type errors.
pub fn expr(
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    let attributes = match &exp.kind {
        fe::Expr::Name(_) => expr_name(scope, context, exp, expected_type),
        fe::Expr::Num(_) => Ok(expr_num(context, exp, expected_type.as_int())),
        fe::Expr::Bool(_) => expr_bool(exp),
        fe::Expr::Subscript { .. } => expr_subscript(scope, context, exp),
        fe::Expr::Attribute { .. } => expr_attribute(scope, context, exp),
        fe::Expr::Ternary { .. } => expr_ternary(scope, context, exp),
        fe::Expr::BoolOperation { .. } => expr_bool_operation(scope, context, exp),
        fe::Expr::BinOperation { .. } => {
            expr_bin_operation(scope, context, exp, expected_type.as_int())
        }
        fe::Expr::UnaryOperation { .. } => expr_unary_operation(scope, context, exp, expected_type),
        fe::Expr::CompOperation { .. } => expr_comp_operation(scope, context, exp),
        fe::Expr::Call { .. } => expr_call(scope, context, exp),
        fe::Expr::List { elts } => expr_list(scope, context, elts, expected_type.as_array()),
        fe::Expr::Tuple { .. } => expr_tuple(scope, context, exp, expected_type.as_tuple()),
        fe::Expr::Str(_) => expr_str(scope, exp),
        fe::Expr::Unit => Ok(ExpressionAttributes::new(Type::unit(), Location::Value)),
    }?;

    context.add_expression(exp, attributes.clone());

    Ok(attributes)
}

pub fn expr_list(
    scope: Shared<BlockScope>,
    context: &mut Context,
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
            let attr = expr(
                Rc::clone(&scope),
                context,
                elt,
                Some(&Type::Base(expected.inner)),
            )?;
            if attr.typ != Type::Base(expected.inner) {
                context.type_error("type mismatch", elt.span, &expected.inner, attr.typ);
            }
        }
        expected.inner
    } else {
        let first_attr = expr(Rc::clone(&scope), context, &elts[0], None)?;
        let inner = match first_attr.typ {
            Type::Base(base) => base,
            _ => {
                context.error(
                    "arrays can only hold primitive types",
                    elts[0].span,
                    format!(
                        "this has type `{}`; expected a primitive type",
                        first_attr.typ
                    ),
                );
                return Err(FatalError);
            }
        };

        // Assuming every element attribute should match the attribute of 0th element
        // of list.
        for elt in &elts[1..] {
            let attr = expr(Rc::clone(&scope), context, elt, Some(&first_attr.typ))?;
            if attr.typ != first_attr.typ {
                context.fancy_error(
                    "array elements must have same type",
                    vec![
                        Label::primary(elts[0].span, format!("this has type `{}`", first_attr.typ)),
                        Label::secondary(elt.span, format!("this has type `{}`", attr.typ)),
                    ],
                    vec![],
                );
            }
        }
        inner
    };

    // TODO: Right now we are only supporting Base type arrays
    // Potential we can support the tuples as well.
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
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    let original_attributes = expr(Rc::clone(&scope), context, exp, expected_type)?;
    let attributes = original_attributes.clone().into_loaded().map_err(|_| {
        context.fancy_error(
            "can't move value onto stack",
            vec![Label::primary(exp.span, "Value to be moved")],
            vec![format!(
                "Note: Can't move `{}` types on the stack",
                original_attributes.typ
            )],
        );
        FatalError
    })?;

    context.update_expression(exp, attributes.clone());

    Ok(attributes)
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is in the type's assigment location.
pub fn assignable_expr(
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    use Type::*;

    let mut attributes = expr(Rc::clone(&scope), context, exp, expected_type)?;
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
        Map(_) => {
            context.error(
                "Maps cannot reside in memory",
                exp.span,
                "this type can only be used in a contract field",
            );
            return Err(FatalError);
        }
    };
    context.update_expression(exp, attributes.clone());

    Ok(attributes)
}

fn expr_tuple(
    scope: Shared<BlockScope>,
    context: &mut Context,
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
                assignable_expr(Rc::clone(&scope), context, elt, exp_type.as_ref())
                    .map(|attributes| attributes.typ)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let tuple_types = match types_to_fixed_sizes(&types) {
            Err(TypeError) => {
                context.error(
                    "variable size types can not be part of tuples",
                    exp.span,
                    "",
                );
                return Err(FatalError);
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
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Name(name) = &exp.kind {
        return match scope.borrow().get_variable_def(name) {
            Some(FixedSize::Base(base)) => {
                Ok(ExpressionAttributes::new(Type::Base(base), Location::Value))
            }
            Some(FixedSize::Contract(contract)) => Ok(ExpressionAttributes::new(
                Type::Contract(contract),
                Location::Value,
            )),
            Some(FixedSize::Array(array)) => Ok(ExpressionAttributes::new(
                Type::Array(array),
                Location::Memory,
            )),
            Some(FixedSize::String(string)) => Ok(ExpressionAttributes::new(
                Type::String(string),
                Location::Memory,
            )),
            Some(FixedSize::Tuple(tuple)) => Ok(ExpressionAttributes::new(
                Type::Tuple(tuple),
                Location::Memory,
            )),
            Some(FixedSize::Struct(val)) => Ok(ExpressionAttributes::new(
                Type::Struct(val),
                Location::Memory,
            )),
            None => {
                context.error(
                    format!("cannot find value `{}` in this scope", name),
                    exp.span,
                    "undefined",
                );
                match expected_type {
                    Some(typ) => {
                        let fixed_size = match FixedSize::try_from(typ.clone()) {
                            Err(TypeError) => {
                                context.error(
                                    format!("Expected type with fixed size but got `{}`", typ),
                                    exp.span,
                                    "wrong type",
                                );
                                return Err(FatalError);
                            }
                            Ok(val) => val,
                        };

                        Ok(ExpressionAttributes::new(
                            typ.clone(),
                            Location::assign_location(&fixed_size),
                        ))
                    }
                    None => Err(FatalError),
                }
            }
        };
    }

    unreachable!()
}

fn expr_str(
    scope: Shared<BlockScope>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Str(string) = &exp.kind {
        scope
            .borrow()
            .contract_scope()
            .borrow_mut()
            .add_string(&string);

        return Ok(ExpressionAttributes::new(
            Type::String(FeString {
                max_size: string.len(),
            }),
            Location::Memory,
        ));
    }

    unreachable!()
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
    context: &mut Context,
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
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Subscript { value, index } = &exp.kind {
        let value_attributes = expr(Rc::clone(&scope), context, value, None)?;
        let index_attributes = value_expr(scope, context, index, None)?;

        // performs type checking
        let typ =
            match operations::index(value_attributes.typ.clone(), index_attributes.typ.clone()) {
                Err(IndexingError::NotSubscriptable) => {
                    context.fancy_error(
                        format!("`{}` type is not subscriptable", value_attributes.typ),
                        vec![Label::primary(value.span, "unsubscriptable type")],
                        vec!["Note: Only arrays and maps are subscriptable".into()],
                    );
                    return Err(FatalError);
                }
                Err(IndexingError::WrongIndexType) => {
                    context.fancy_error(
                        format!(
                            "can not subscript {} with type {}",
                            value_attributes.typ, index_attributes.typ
                        ),
                        vec![Label::primary(index.span, "wrong index type")],
                        vec![],
                    );
                    return Err(FatalError);
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
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Attribute { value, attr } = &exp.kind {
        let base_type = |typ| Ok(ExpressionAttributes::new(Type::Base(typ), Location::Value));
        let array_type = |array| {
            Ok(ExpressionAttributes::new(
                Type::Array(array),
                Location::Value,
            ))
        };
        let fatal_err = Err(FatalError);

        // If the value is a name, check if it is a builtin object and attribute.
        if let fe::Expr::Name(name) = &value.kind {
            match Object::from_str(name) {
                Ok(Object::Self_) => return expr_attribute_self(scope, context, attr),
                Ok(Object::Block) => {
                    return match BlockField::from_str(&attr.kind) {
                        Ok(BlockField::Coinbase) => base_type(Base::Address),
                        Ok(BlockField::Difficulty) => base_type(U256),
                        Ok(BlockField::Number) => base_type(U256),
                        Ok(BlockField::Timestamp) => base_type(U256),
                        Err(_) => {
                            context.fancy_error(
                                "Not a block field",
                                vec![
                                    Label::primary(
                                        attr.span,
                                        "",
                                    ),
                                ],
                                vec!["Note: Only `coinbase`, `difficulty`, `number` and `timestamp` can be accessed on `block`.".into()],
                            );
                            fatal_err
                        }
                    }
                }
                Ok(Object::Chain) => {
                    return match ChainField::from_str(&attr.kind) {
                        Ok(ChainField::Id) => base_type(U256),
                        Err(_) => {
                            context.fancy_error(
                                "Not a chain field",
                                vec![Label::primary(attr.span, "")],
                                vec!["Note: Only `id` can be accessed on `chain`.".into()],
                            );
                            fatal_err
                        }
                    }
                }
                Ok(Object::Msg) => {
                    return match MsgField::from_str(&attr.kind) {
                        Ok(MsgField::Sender) => base_type(Base::Address),
                        Ok(MsgField::Sig) => array_type(Array {
                            size: 32,
                            inner: Base::Byte,
                        }),
                        Ok(MsgField::Value) => base_type(U256),
                        Err(_) => {
                            context.fancy_error(
                                "Not a `msg` field",
                                vec![
                                    Label::primary(
                                        attr.span,
                                        "",
                                    ),
                                ],
                                vec!["Note: Only `sender`, `sig` and `value` can be accessed on `msg`.".into()],
                            );
                            fatal_err
                        }
                    }
                }
                Ok(Object::Tx) => {
                    return match TxField::from_str(&attr.kind) {
                        Ok(TxField::GasPrice) => base_type(U256),
                        Ok(TxField::Origin) => base_type(Base::Address),
                        Err(_) => {
                            context.fancy_error(
                                "Not a `tx` field",
                                vec![Label::primary(attr.span, "")],
                                vec![
                                    "Note: Only `gas_price` and `origin` can be accessed on `tx`."
                                        .into(),
                                ],
                            );
                            fatal_err
                        }
                    }
                }
                Err(_) => {}
            }
        }

        // We attempt to analyze the value as an expression. If this is successful, we
        // build a new set of attributes from the value attributes.
        let expression_attributes = expr(Rc::clone(&scope), context, value, None)?;
        return match expression_attributes {
            // If the value is a struct, we return the type of the attribute. The location stays the
            // same and can be memory or storage.
            ExpressionAttributes {
                typ: Type::Struct(struct_),
                location,
                ..
            } => {
                if let Some(typ) = struct_.get_field_type(&attr.kind) {
                    Ok(ExpressionAttributes::new(typ.to_owned().into(), location))
                } else {
                    context.fancy_error(
                        format!(
                            "No field `{}` exists on struct `{}`",
                            &attr.kind, struct_.name
                        ),
                        vec![Label::primary(attr.span, "undefined field")],
                        vec![],
                    );
                    fatal_err
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
                        context.fancy_error(
                            format!("No field `{}` exists on this tuple", &attr.kind),
                            vec![
                                Label::primary(
                                    attr.span,
                                    "undefined field",
                                )
                            ],
                            vec!["Note: Tuple values are accessed via `itemN` properties such as `item0` or `item1`".into()],
                        );
                        return fatal_err;
                    }
                };

                if let Some(typ) = tuple.items.get(item_index) {
                    Ok(ExpressionAttributes::new(typ.to_owned().into(), location))
                } else {
                    context.fancy_error(
                        format!("No field `item{}` exists on this tuple", item_index),
                        vec![Label::primary(attr.span, "unknown field")],
                        vec![format!(
                            "Note: The highest possible field for this tuple is `item{}`",
                            tuple.items.len() - 1
                        )],
                    );
                    fatal_err
                }
            }
            _ => {
                context.fancy_error(
                    format!(
                        "No field `{}` exists on type {}",
                        &attr.kind, expression_attributes.typ
                    ),
                    vec![Label::primary(attr.span, "unknown field")],
                    vec![],
                );
                fatal_err
            }
        };
    }

    unreachable!()
}

/// Pull the item index from the attribute string (e.g. "item4" -> "4").
fn tuple_item_index(item: &str) -> Option<usize> {
    if item.len() < 5 || &item[..4] != "item" {
        None
    } else {
        item[4..].parse::<usize>().ok()
    }
}

fn expr_attribute_self(
    scope: Shared<BlockScope>,
    context: &mut Context,
    attr: &Node<String>,
) -> Result<ExpressionAttributes, FatalError> {
    if let Ok(SelfField::Address) = SelfField::from_str(&attr.kind) {
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Address),
            Location::Value,
        ));
    }

    match scope.borrow().contract_field_def(&attr.kind) {
        Some(field) => Ok(ExpressionAttributes::new(
            field.typ,
            Location::Storage {
                nonce: Some(field.nonce),
            },
        )),
        None => {
            context.fancy_error(
                format!("No field `{}` exists on this contract", &attr.kind),
                vec![Label::primary(attr.span, "undefined field")],
                vec![],
            );
            Err(FatalError)
        }
    }
}

fn expr_bin_operation(
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
    expected_type: Option<Integer>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::BinOperation { left, op, right } = &exp.kind {
        let expected = expected_type.map(Type::int);
        let left_attributes = value_expr(Rc::clone(&scope), context, left, expected.as_ref())?;
        let right_attributes = value_expr(Rc::clone(&scope), context, right, expected.as_ref())?;

        let typ = match operations::bin(&left_attributes.typ, &op.kind, &right_attributes.typ) {
            Err(err) => {
                add_bin_operations_errors(context, left, right, err);
                return Err(FatalError);
            }
            Ok(val) => val,
        };

        return Ok(ExpressionAttributes::new(typ, Location::Value));
    }

    unreachable!()
}

fn expr_unary_operation(
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
    expected_type: Option<&Type>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::UnaryOperation { op, operand } = &exp.kind {
        let operand_attributes = value_expr(Rc::clone(&scope), context, operand, None)?;

        let emit_err = |context: &mut Context, expected| {
            context.error(
                format!(
                    "cannot apply unary operator `{}` to type `{}`",
                    op.kind, operand_attributes.typ
                ),
                operand.span,
                format!(
                    "this has type `{}`; expected {}",
                    operand_attributes.typ, expected
                ),
            )
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
                context.not_yet_implemented("unary invert", exp.span);
                Ok(ExpressionAttributes::new(Type::unit(), Location::Value))
            }
        };
    }

    unreachable!()
}

fn expr_call(
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Call {
        func,
        generic_args,
        args,
    } = &exp.kind
    {
        return match expr_call_type(Rc::clone(&scope), context, func, generic_args.as_ref())? {
            CallType::BuiltinFunction { func: builtin } => {
                expr_call_builtin_function(scope, context, builtin, func.span, args)
            }
            CallType::TypeConstructor { typ } => {
                expr_call_type_constructor(scope, context, func.span, typ, args)
            }
            CallType::SelfAttribute { func_name } => {
                expr_call_self_attribute(scope, context, &func_name, func.span, args)
            }
            CallType::ValueAttribute => expr_call_value_attribute(scope, context, func, args),
            CallType::TypeAttribute { typ, func_name } => {
                expr_call_type_attribute(scope, context, typ, &func_name, func.span, args)
            }
        };
    }

    unreachable!()
}

fn expr_call_builtin_function(
    scope: Shared<BlockScope>,
    context: &mut Context,
    typ: GlobalMethod,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    let argument_attributes = expr_call_args(Rc::clone(&scope), context, args)?;
    match typ {
        GlobalMethod::Keccak256 => {
            validate_arg_count(context, typ.into(), name_span, args, 1);
            validate_arg_labels(context, args, &[None]);

            // We only need to return with a FatalError here because the error is already reported
            // as part of a different check on function arguments.
            let arg_typ = argument_attributes
                .first()
                .map(|attr| &attr.typ)
                .ok_or(FatalError)?;

            if !matches!(
                arg_typ,
                Type::Array(Array {
                    inner: Base::Byte,
                    ..
                })
            ) {
                context.fancy_error(
                    format!(
                        "`{}` can not be used as a parameter to `keccak(..)`",
                        arg_typ
                    ),
                    vec![Label::primary(args.span, "wrong type")],
                    vec!["Note: keccak(..) expects a byte array as parameter".into()],
                );
            }
            Ok(ExpressionAttributes::new(Type::Base(U256), Location::Value))
        }
    }
}

pub fn validate_arg_count(
    context: &mut Context,
    name: &str,
    name_span: Span,
    args: &Node<Vec<impl Spanned>>,
    param_count: usize,
) {
    if args.kind.len() != param_count {
        let mut labels = vec![Label::primary(
            name_span,
            format!(
                "expects {} {}",
                param_count,
                pluralize_conditionally("argument", param_count)
            ),
        )];
        if args.kind.is_empty() {
            labels.push(Label::secondary(args.span, "supplied 0 arguments"));
        } else {
            for arg in &args.kind {
                labels.push(Label::secondary(arg.span(), ""));
            }
            labels.last_mut().unwrap().message = format!(
                "supplied {} {}",
                args.kind.len(),
                pluralize_conditionally("argument", args.kind.len())
            );
        }

        context.fancy_error(
            format!(
                "`{}` expects {} {}, but {} {} provided",
                name,
                param_count,
                pluralize_conditionally("argument", param_count),
                args.kind.len(),
                pluralize_conditionally(("was", "were"), args.kind.len())
            ),
            labels,
            vec![],
        );
        // TODO: add `defined here` label (need span for definition)
    }
}

pub fn validate_arg_labels(
    context: &mut Context,
    args: &Node<Vec<Node<fe::CallArg>>>,
    labels: &[Option<&str>],
) {
    for (expected_label, arg) in labels.iter().zip(args.kind.iter()) {
        let arg_val = &arg.kind.value;
        match (expected_label, &arg.kind.label) {
            (Some(expected_label), Some(actual_label)) => {
                if *expected_label != actual_label.kind {
                    let notes = if labels
                        .iter()
                        .any(|nm| nm == &Some(actual_label.kind.as_str()))
                    {
                        vec!["Note: arguments must be provided in order.".into()]
                    } else {
                        vec![]
                    };
                    context.fancy_error(
                        "argument label mismatch",
                        vec![Label::primary(
                            actual_label.span,
                            format!("expected `{}`", expected_label),
                        )],
                        notes,
                    );
                }
            }
            (Some(expected_label), None) => match &arg_val.kind {
                fe::Expr::Name(var_name) if var_name == *expected_label => {}
                _ => {
                    context.fancy_error(
                        "missing argument label",
                        vec![Label::primary(
                            Span::new(arg_val.span.start, arg_val.span.start),
                            format!("add `{}=` here", expected_label),
                        )],
                        vec![format!(
                            "Note: this label is optional if the argument is a variable named `{}`.",
                            expected_label
                        )],
                    );
                }
            },
            (None, Some(actual_label)) => {
                context.error(
                    "argument should not be labeled",
                    actual_label.span,
                    "remove this label",
                );
            }
            (None, None) => {}
        }
    }
}

pub fn validate_arg_types(
    scope: Shared<BlockScope>,
    context: &mut Context,
    name: &str,
    args: &Node<Vec<Node<fe::CallArg>>>,
    params: &[(String, FixedSize)],
) -> Result<(), FatalError> {
    for ((label, param_type), arg) in params.iter().zip(args.kind.iter()) {
        let val_attrs = assignable_expr(
            Rc::clone(&scope),
            context,
            &arg.kind.value,
            Some(&param_type.clone().into()),
        )?;
        if param_type != &val_attrs.typ {
            context.type_error(
                format!("incorrect type for `{}` argument `{}`", name, label),
                arg.kind.value.span,
                param_type,
                val_attrs.typ,
            );
        }
    }
    Ok(())
}

pub fn validate_named_args(
    scope: Shared<BlockScope>,
    context: &mut Context,
    name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
    params: &[(String, FixedSize)],
) -> Result<(), FatalError> {
    validate_arg_count(context, name, name_span, args, params.len());
    validate_arg_labels(
        context,
        args,
        &params
            .iter()
            .map(|(nm, _)| Some(nm.as_str()))
            .collect::<Vec<_>>(),
    );
    validate_arg_types(scope, context, name, args, params)?;
    Ok(())
}

fn expr_call_struct_constructor(
    scope: Shared<BlockScope>,
    context: &mut Context,
    name_span: Span,
    typ: Struct,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    validate_named_args(
        Rc::clone(&scope),
        context,
        &typ.name,
        name_span,
        &args,
        &typ.fields,
    )?;

    Ok(ExpressionAttributes::new(
        Type::Struct(typ),
        Location::Memory,
    ))
}

fn expr_call_type_constructor(
    scope: Shared<BlockScope>,
    context: &mut Context,
    name_span: Span,
    typ: Type,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    if let Type::Struct(struct_type) = typ {
        return expr_call_struct_constructor(scope, context, name_span, struct_type, args);
    }

    // These all expect 1 arg, for now.
    validate_arg_count(context, &format!("{}", typ), name_span, args, 1);
    validate_arg_labels(context, args, &[None]);

    match &typ {
        Type::String(string_type) => {
            if let Some(arg) = args.kind.first() {
                assignable_expr(Rc::clone(&scope), context, &arg.kind.value, None)?;
                validate_str_literal_fits_type(context, &arg.kind.value, string_type);
            }
            Ok(ExpressionAttributes::new(typ, Location::Memory))
        }
        Type::Contract(_) => {
            if let Some(arg) = args.kind.first() {
                let arg_attr = assignable_expr(Rc::clone(&scope), context, &arg.kind.value, None)?;
                if arg_attr.typ != Type::Base(Base::Address) {
                    context.type_error("type mismatch", arg.span, Base::Address, &typ);
                }
            }
            Ok(ExpressionAttributes::new(typ, Location::Value))
        }
        Type::Base(Base::Numeric(_)) => {
            if let Some(arg) = args.kind.first() {
                // This will check if the literal fits inside int_type.
                assignable_expr(Rc::clone(&scope), context, &arg.kind.value, Some(&typ))?;
                validate_is_numeric_literal(context, &arg.kind.value);
            }
            Ok(ExpressionAttributes::new(typ, Location::Value))
        }
        Type::Base(Base::Address) => {
            if let Some(arg) = args.kind.first() {
                let arg_attr = assignable_expr(Rc::clone(&scope), context, &arg.kind.value, None)?;
                match arg_attr.typ {
                    Type::Contract(_)
                    | Type::Base(Base::Numeric(_))
                    | Type::Base(Base::Address) => {}
                    _ => {
                        context.fancy_error(
                            format!("`{}` can not be used as a parameter to `address(..)`", arg_attr.typ),
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
        _ => Err(FatalError),
    }
}

fn expr_call_args(
    scope: Shared<BlockScope>,
    context: &mut Context,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<Vec<ExpressionAttributes>, FatalError> {
    args.kind
        .iter()
        .map(|arg| assignable_expr(Rc::clone(&scope), context, &arg.kind.value, None))
        .collect::<Result<Vec<_>, _>>()
}

fn expr_call_self_attribute(
    scope: Shared<BlockScope>,
    context: &mut Context,
    func_name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    let called_func = scope
        .borrow()
        .contract_scope()
        .borrow()
        .function_def(func_name);

    if let Some(ContractFunctionDef {
        params,
        return_type,
        ..
    }) = called_func
    {
        validate_arg_count(context, func_name, name_span, args, params.len());
        validate_arg_types(Rc::clone(&scope), context, func_name, args, &params)?;

        let return_location = match &return_type {
            FixedSize::Base(_) => Location::Value,
            _ => Location::Memory,
        };
        Ok(ExpressionAttributes::new(
            return_type.into(),
            return_location,
        ))
    } else {
        context.fancy_error(
            format!("no function `{}` exists this contract", func_name),
            vec![Label::primary(name_span, "undefined function")],
            vec![],
        );
        Err(FatalError)
    }
}

fn expr_call_value_attribute(
    scope: Shared<BlockScope>,
    context: &mut Context,
    func: &Node<fe::Expr>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Attribute { value, attr } = &func.kind {
        let value_attributes = expr(Rc::clone(&scope), context, &value, None)?;

        if let Type::Contract(contract) = &value_attributes.typ {
            // We must ensure the expression is loaded onto the stack.
            let expression = value_attributes.clone().into_loaded().map_err(|_| {
                // TODO: Add test code that triggers this
                context.fancy_error(
                    "can't move value onto stack",
                    vec![Label::primary(value.span, "Value to be moved")],
                    vec![format!(
                        "Note: Can't move `{}` types on the stack",
                        value_attributes.typ
                    )],
                );
                FatalError
            })?;

            context.update_expression(value, expression);
            return expr_call_contract_attribute(
                scope,
                context,
                contract.to_owned(),
                &attr.kind,
                attr.span,
                args,
            );
        }

        // for now all of these function expect 0 arguments
        validate_arg_count(context, &attr.kind, attr.span, args, 0);

        return match ValueMethod::from_str(&attr.kind) {
            Err(_) => {
                context.fancy_error(
                    format!(
                        "No function `{}` exists on type `{}`",
                        &attr.kind, &value_attributes.typ
                    ),
                    vec![Label::primary(attr.span, "undefined function")],
                    vec![],
                );
                return Err(FatalError);
            }
            Ok(ValueMethod::Clone) => {
                match value_attributes.location {
                    Location::Storage { .. } => {
                        context.fancy_error(
                            "`clone()` called on value in storage",
                            vec![
                                Label::primary(value.span, "this value is in storage"),
                                Label::secondary(attr.span, "hint: try `to_mem()` here"),
                            ],
                            vec![],
                        );
                    }
                    Location::Value => {
                        context.fancy_error(
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
                        context.fancy_error(
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
                        context.fancy_error(
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
                        context.fancy_error(
                            "value must be copied to memory",
                            vec![Label::primary(value.span, "this value is in storage")],
                            vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                                 "Example: `self.my_array.to_mem().abi_encode()`".into(),
                            ],
                        );
                    }

                    Ok(ExpressionAttributes::new(
                        Type::Array(Array {
                            inner: Base::Byte,
                            size: struct_.fields.len() * 32,
                        }),
                        Location::Memory,
                    ))
                }
                Type::Tuple(tuple) => {
                    if value_attributes.final_location() != Location::Memory {
                        context.fancy_error(
                            "value must be copied to memory",
                            vec![Label::primary(value.span, "this value is in storage")],
                            vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                                 "Example: `self.my_array.to_mem().abi_encode()`".into(),
                            ],
                        );
                    }

                    Ok(ExpressionAttributes::new(
                        Type::Array(Array {
                            inner: Base::Byte,
                            size: tuple.items.len() * 32,
                        }),
                        Location::Memory,
                    ))
                }
                _ => {
                    context.fancy_error(
                        format!(
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
    scope: Shared<BlockScope>,
    context: &mut Context,
    typ: Type,
    func_name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    let arg_attributes = expr_call_args(Rc::clone(&scope), context, args)?;
    let contract_name = scope.borrow().contract_scope().borrow().name.clone();

    let report_circular_dependency = |context: &mut Context, method: String| {
        context.fancy_error(
            format!("`{contract}.{}(...)` called within `{contract}` creates an illegal circular dependency", method, contract=contract_name),
            vec![Label::primary(name_span, "Contract creation")],
            vec![format!("Note: Consider using a dedicated factory contract to create instances of `{}`", contract_name)]);
    };

    match (typ.clone(), ContractTypeMethod::from_str(func_name)) {
        (Type::Contract(contract), Ok(ContractTypeMethod::Create2)) => {
            validate_arg_count(context, func_name, name_span, args, 2);

            if contract_name == contract.name {
                report_circular_dependency(context, ContractTypeMethod::Create2.to_string());
            }

            if matches!(
                (&arg_attributes[0].typ, &arg_attributes[1].typ),
                (Type::Base(Base::Numeric(_)), Type::Base(Base::Numeric(_)))
            ) {
                scope
                    .borrow()
                    .contract_scope()
                    .borrow_mut()
                    .add_created_contract(&contract.name);
            } else {
                context.fancy_error(
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
            validate_arg_count(context, func_name, name_span, args, 1);

            if contract_name == contract.name {
                report_circular_dependency(context, ContractTypeMethod::Create.to_string());
            }

            if matches!(&arg_attributes[0].typ, Type::Base(Base::Numeric(_))) {
                scope
                    .borrow()
                    .contract_scope()
                    .borrow_mut()
                    .add_created_contract(&contract.name);
            } else {
                context.fancy_error(
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
        _ => {
            context.fancy_error(
                format!("No function `{}` exists on type `{}`", func_name, &typ),
                vec![Label::primary(name_span, "undefined function")],
                vec![],
            );
            Err(FatalError)
        }
    }
}

fn expr_call_contract_attribute(
    scope: Shared<BlockScope>,
    context: &mut Context,
    contract: Contract,
    func_name: &str,
    name_span: Span,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, FatalError> {
    if let Some(function) = contract
        .functions
        .iter()
        .find(|function| function.name == func_name)
    {
        let return_type = function.return_type.to_owned();

        validate_arg_count(context, func_name, name_span, args, function.params.len());
        validate_arg_types(
            Rc::clone(&scope),
            context,
            func_name,
            args,
            &function.params,
        )?;

        Ok(ExpressionAttributes::new(
            return_type.clone().into(),
            Location::assign_location(&return_type),
        ))
    } else {
        context.fancy_error(
            format!(
                "No function `{}` exists on contract `{}`",
                func_name, contract.name
            ),
            vec![Label::primary(name_span, "undefined function")],
            vec![],
        );
        Err(FatalError)
    }
}

fn expr_call_type(
    scope: Shared<BlockScope>,
    context: &mut Context,
    func: &Node<fe::Expr>,
    generic_args: Option<&Node<Vec<fe::GenericArg>>>,
) -> Result<CallType, FatalError> {
    let call_type = match &func.kind {
        fe::Expr::Name(name) => expr_name_call_type(scope, context, name, func.span, generic_args),
        fe::Expr::Attribute { .. } => expr_attribute_call_type(scope, context, func),
        _ => {
            let expression = expr(scope, context, func, None)?;
            context.fancy_error(
                format!("the {} type is not callable", expression.typ),
                vec![Label::primary(
                    func.span,
                    format!("this has type {}", expression.typ),
                )],
                vec![],
            );
            Err(FatalError)
        }
    }?;

    context.add_call(func, call_type.clone());
    Ok(call_type)
}

fn expr_name_call_type(
    scope: Shared<BlockScope>,
    context: &mut Context,
    name: &str,
    name_span: Span,
    generic_args: Option<&Node<Vec<fe::GenericArg>>>,
) -> Result<CallType, FatalError> {
    match (name, generic_args) {
        ("keccak256", _) => Ok(CallType::BuiltinFunction {
            func: GlobalMethod::Keccak256,
        }),
        ("address", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Address),
        }),
        ("u256", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U256)),
        }),
        ("u128", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U128)),
        }),
        ("u64", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U64)),
        }),
        ("u32", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U32)),
        }),
        ("u16", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U16)),
        }),
        ("u8", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U8)),
        }),
        ("i256", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I256)),
        }),
        ("i128", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I128)),
        }),
        ("i64", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I64)),
        }),
        ("i32", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I32)),
        }),
        ("i16", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I16)),
        }),
        ("i8", _) => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I8)),
        }),
        ("String", Some(Node { kind: args, .. })) => match &args[..] {
            [fe::GenericArg::Int(len)] => Ok(CallType::TypeConstructor {
                typ: Type::String(FeString { max_size: len.kind }),
            }),
            _ => Err(FatalError),
        },
        (value, _) => {
            if let Some(typ) = scope.borrow().get_module_type_def(value) {
                Ok(CallType::TypeConstructor { typ })
            } else {
                context.error("undefined function", name_span, "undefined");
                Err(FatalError)
            }
        }
    }
}

fn expr_attribute_call_type(
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
) -> Result<CallType, FatalError> {
    if let fe::Expr::Attribute { value, attr } = &exp.kind {
        if let fe::Expr::Name(name) = &value.kind {
            match Object::from_str(&name) {
                Ok(Object::Block) | Ok(Object::Chain) | Ok(Object::Msg) | Ok(Object::Tx) => {
                    context.fancy_error(
                        format!("no function `{}` exists on builtin `{}`", &attr.kind, &name),
                        vec![Label::primary(exp.span, "undefined function")],
                        vec![],
                    );

                    return Err(FatalError);
                }
                Ok(Object::Self_) => {
                    return Ok(CallType::SelfAttribute {
                        func_name: attr.kind.to_string(),
                    })
                }
                Err(_) => {}
            }

            if let Some(typ) = scope.borrow().get_module_type_def(&name) {
                return Ok(CallType::TypeAttribute {
                    typ,
                    func_name: attr.kind.to_string(),
                });
            }
        };

        return Ok(CallType::ValueAttribute);
    }

    unreachable!()
}

fn validate_is_numeric_literal(context: &mut Context, value: &Node<fe::Expr>) -> Option<BigInt> {
    if let fe::Expr::UnaryOperation { operand, op: _ } = &value.kind {
        if let fe::Expr::Num(num) = &operand.kind {
            return Some(-to_bigint(num));
        }
    } else if let fe::Expr::Num(num) = &value.kind {
        return Some(to_bigint(num));
    }
    context.error("type mismatch", value.span, "expected a number literal");
    None
}

fn validate_numeric_literal_fits_type(
    context: &mut Context,
    num: BigInt,
    span: Span,
    int_type: Integer,
) {
    if !int_type.fits(num) {
        context.error(
            format!("literal out of range for `{}`", int_type),
            span,
            format!("does not fit into type `{}`", int_type),
        );
    }
}

fn validate_str_literal_fits_type(context: &mut Context, arg_val: &Node<fe::Expr>, typ: &FeString) {
    if let fe::Expr::Str(string) = &arg_val.kind {
        if string.len() > typ.max_size {
            context.error(
                "string capacity exceeded",
                arg_val.span,
                format!(
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
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::CompOperation { left, op, right } = &exp.kind {
        // comparison operands should be moved to the stack
        let left_attr = value_expr(Rc::clone(&scope), context, left, None)?;
        let right_attr = value_expr(Rc::clone(&scope), context, right, Some(&left_attr.typ))?;

        if left_attr.typ != right_attr.typ {
            context.fancy_error(
                format!("`{}` operands must have the same type", op.kind),
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
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::Ternary {
        if_expr,
        test,
        else_expr,
    } = &exp.kind
    {
        // test attributes should be stored as a value
        let test_attributes = value_expr(Rc::clone(&scope), context, test, None)?;
        // the return expressions should be stored in their default locations
        //
        // If, for example, one of the expressions is stored in memory and the other is
        // stored in storage, it's necessary that we move them to the same location.
        // This could be memory or the stack, depending on the type.
        let if_expr_attributes = assignable_expr(Rc::clone(&scope), context, if_expr, None)?;
        let else_expr_attributes = assignable_expr(
            Rc::clone(&scope),
            context,
            else_expr,
            Some(&if_expr_attributes.typ),
        )?;

        // Make sure the `test_attributes` is a boolean type.
        if Type::Base(Base::Bool) != test_attributes.typ {
            context.error(
                "`if` test expression must be a `bool`",
                test.span,
                format!("this has type `{}`; expected `bool`", test_attributes.typ),
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
    scope: Shared<BlockScope>,
    context: &mut Context,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, FatalError> {
    if let fe::Expr::BoolOperation { left, op, right } = &exp.kind {
        for operand in &[left, right] {
            let attributes = value_expr(Rc::clone(&scope), context, operand, None)?;
            if attributes.typ != Type::Base(Base::Bool) {
                context.error(
                    format!("binary op `{}` operands must have type `bool`", op.kind),
                    operand.span,
                    format!("this has type `{}`; expected `bool`", attributes.typ),
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
