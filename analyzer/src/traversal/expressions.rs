use crate::builtins;
use crate::context::{CallType, Context, ExpressionAttributes, Location};
use crate::errors::SemanticError;
use crate::namespace::scopes::{BlockScope, ContractFunctionDef, Shared};
use crate::namespace::types::{
    Array, Base, Contract, FeString, FixedSize, Integer, Struct, Tuple, Type, U256,
};
use crate::operations;
use crate::traversal::utils::{
    call_arg_value, expression_attributes_to_types, fixed_sizes_to_types, types_to_fixed_sizes,
};

use builtins::{
    BlockField, ChainField, ContractTypeMethod, GlobalMethod, MsgField, Object, TxField,
    ValueMethod,
};
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::convert::TryFrom;
use std::rc::Rc;
use std::str::FromStr;

/// Gather context information for expressions and check for type errors.
pub fn expr(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    let attributes = match &exp.kind {
        fe::Expr::Name(_) => expr_name(scope, exp),
        fe::Expr::Num(_) => expr_num(exp),
        fe::Expr::Bool(_) => expr_bool(exp),
        fe::Expr::Subscript { .. } => expr_subscript(scope, Rc::clone(&context), exp),
        fe::Expr::Attribute { .. } => expr_attribute(scope, Rc::clone(&context), exp),
        fe::Expr::Ternary { .. } => expr_ternary(scope, Rc::clone(&context), exp),
        fe::Expr::BoolOperation { .. } => expr_bool_operation(scope, Rc::clone(&context), exp),
        fe::Expr::BinOperation { .. } => expr_bin_operation(scope, Rc::clone(&context), exp),
        fe::Expr::UnaryOperation { .. } => expr_unary_operation(scope, Rc::clone(&context), exp),
        fe::Expr::CompOperation { .. } => expr_comp_operation(scope, Rc::clone(&context), exp),
        fe::Expr::Call { .. } => expr_call(scope, Rc::clone(&context), exp),
        fe::Expr::List { .. } => expr_list(scope, Rc::clone(&context), exp),
        fe::Expr::ListComp { .. } => unimplemented!(),
        fe::Expr::Tuple { .. } => expr_tuple(scope, Rc::clone(&context), exp),
        fe::Expr::Str(_) => expr_str(scope, exp),
    }
    .map_err(|error| error.with_context(exp.span))?;

    context.borrow_mut().add_expression(exp, attributes.clone());

    Ok(attributes)
}

pub fn expr_list(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::List { elts } = &exp.kind {
        // Assuming every element attribute should matches the attribute of 0th element
        // of list. Safe to unwrap as we already checked the length of the
        // list.
        if let Some(elt) = elts.first() {
            let attribute_to_be_matched = expr(Rc::clone(&scope), Rc::clone(&context), elt)?;

            // Rationale - A list contains only one type of element.
            // No need to iterate the whole list if the first attribute doesn't match next
            // one.
            for elt in elts.iter() {
                let next_attribute = expr(Rc::clone(&scope), Rc::clone(&context), elt)?;
                validate_types_equal(&next_attribute, &attribute_to_be_matched)?;
            }

            // TODO: Right now we are only supporting Base type arrays
            // Potential we can support the tuples as well.
            if let Type::Base(base) = attribute_to_be_matched.typ {
                let array_typ = Array {
                    size: elts.len(),
                    inner: base,
                };

                scope
                    .borrow()
                    .contract_scope()
                    .borrow_mut()
                    .add_used_list_expression(array_typ.clone());

                return Ok(ExpressionAttributes {
                    typ: Type::Array(array_typ),
                    location: Location::Memory,
                    move_location: None,
                });
            }
            return Err(SemanticError::type_error());
        }
    }
    unreachable!()
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is on the stack.
pub fn value_expr(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    let attributes = expr(Rc::clone(&scope), Rc::clone(&context), exp)?.into_loaded()?;

    context.borrow_mut().add_expression(exp, attributes.clone());

    Ok(attributes)
}

/// Gather context information for expressions and check for type errors.
///
/// Also ensures that the expression is in the type's assigment location.
pub fn assignable_expr(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    let attributes = expr(Rc::clone(&scope), Rc::clone(&context), exp)?.into_assignable()?;

    context.borrow_mut().add_expression(exp, attributes.clone());

    Ok(attributes)
}

/// Retrieves the String value of a name expression.
pub fn expr_name_string(exp: &Node<fe::Expr>) -> Result<String, SemanticError> {
    if let fe::Expr::Name(name) = &exp.kind {
        return Ok(name.to_owned());
    }

    unreachable!()
}

/// Gather context information for an index and check for type errors.
pub fn slices_index(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    slices: &Node<Vec<Node<fe::Slice>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let Some(first_slice) = slices.kind.first() {
        return slice_index(scope, context, first_slice);
    }

    unreachable!()
}

/// Gather context information for an index and check for type errors.
pub fn slice_index(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    slice: &Node<fe::Slice>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Slice::Index(index) = &slice.kind {
        let attributes = value_expr(scope, Rc::clone(&context), index)?;

        return Ok(attributes);
    }

    unreachable!()
}

fn expr_tuple(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Tuple { elts } = &exp.kind {
        return if elts.is_empty() {
            Ok(ExpressionAttributes::new(
                Type::Tuple(Tuple::empty()),
                Location::Memory,
            ))
        } else {
            let types = elts
                .iter()
                .map(|elt| {
                    assignable_expr(Rc::clone(&scope), Rc::clone(&context), elt)
                        .map(|attributes| attributes.typ)
                })
                .collect::<Result<Vec<_>, _>>()?;

            let tuple = Tuple {
                items: types_to_fixed_sizes(types)?,
            };

            scope
                .borrow()
                .module_scope()
                .borrow_mut()
                .tuples_used
                .insert(tuple.clone());

            Ok(ExpressionAttributes::new(
                Type::Tuple(tuple),
                Location::Memory,
            ))
        };
    }

    unreachable!()
}

fn expr_name(
    scope: Shared<BlockScope>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
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
            None => Err(SemanticError::undefined_value()),
        };
    }

    unreachable!()
}

fn expr_str(
    scope: Shared<BlockScope>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Str(lines) = &exp.kind {
        let string_val = lines.join("");
        let string_length = string_val.len();

        scope
            .borrow()
            .contract_scope()
            .borrow_mut()
            .add_string(&string_val)?;

        return Ok(ExpressionAttributes::new(
            Type::String(FeString {
                max_size: string_length,
            }),
            Location::Memory,
        ));
    }

    unreachable!()
}

fn expr_bool(exp: &Node<fe::Expr>) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Bool(_) = &exp.kind {
        return Ok(ExpressionAttributes::new(
            Type::Base(Base::Bool),
            Location::Value,
        ));
    }

    unreachable!()
}

fn expr_num(exp: &Node<fe::Expr>) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Num(num) = &exp.kind {
        validate_numeric_literal_fits_type(num, &Type::Base(U256))?;
        return Ok(ExpressionAttributes::new(Type::Base(U256), Location::Value));
    }

    unreachable!()
}

fn expr_subscript(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Subscript { value, slices } = &exp.kind {
        let value_attributes = expr(Rc::clone(&scope), Rc::clone(&context), value)?;
        let index_attributes = slices_index(scope, context, slices)?;

        // performs type checking
        let typ = operations::index(value_attributes.typ.clone(), index_attributes.typ)?;
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
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Attribute { value, attr } = &exp.kind {
        let base_type = |typ| Ok(ExpressionAttributes::new(Type::Base(typ), Location::Value));
        let array_type = |array| {
            Ok(ExpressionAttributes::new(
                Type::Array(array),
                Location::Value,
            ))
        };
        let undefined_value_err = Err(SemanticError::undefined_value());

        // If the value is a name, check if it is a builtin object and attribute.
        if let fe::Expr::Name(name) = &value.kind {
            match Object::from_str(name) {
                Ok(Object::Self_) => return expr_attribute_self(scope, attr),
                Ok(Object::Block) => {
                    return match BlockField::from_str(&attr.kind) {
                        Ok(BlockField::Coinbase) => base_type(Base::Address),
                        Ok(BlockField::Difficulty) => base_type(U256),
                        Ok(BlockField::Number) => base_type(U256),
                        Ok(BlockField::Timestamp) => base_type(U256),
                        Err(_) => undefined_value_err,
                    }
                }
                Ok(Object::Chain) => {
                    return match ChainField::from_str(&attr.kind) {
                        Ok(ChainField::Id) => base_type(U256),
                        Err(_) => undefined_value_err,
                    }
                }
                Ok(Object::Msg) => {
                    return match MsgField::from_str(&attr.kind) {
                        Ok(MsgField::Data) => todo!(),
                        Ok(MsgField::Sender) => base_type(Base::Address),
                        Ok(MsgField::Sig) => array_type(Array {
                            size: 32,
                            inner: Base::Byte,
                        }),
                        Ok(MsgField::Value) => base_type(U256),
                        Err(_) => undefined_value_err,
                    }
                }
                Ok(Object::Tx) => {
                    return match TxField::from_str(&attr.kind) {
                        Ok(TxField::GasPrice) => base_type(U256),
                        Ok(TxField::Origin) => base_type(Base::Address),
                        Err(_) => undefined_value_err,
                    }
                }
                Err(_) => {}
            }
        }

        // We attempt to analyze the value as an expression. If this is successful, we
        // build a new set of attributes from the value attributes.
        return match expr(scope, context, value)? {
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
                    undefined_value_err
                }
            }
            ExpressionAttributes {
                typ: Type::Tuple(tuple),
                location,
                ..
            } => {
                let item_index = tuple_item_index(&attr.kind)?;
                if let Some(typ) = tuple.items.get(item_index) {
                    Ok(ExpressionAttributes::new(typ.to_owned().into(), location))
                } else {
                    undefined_value_err
                }
            }
            _ => undefined_value_err,
        };
    }

    unreachable!()
}

/// Pull the item index from the attribute string (e.g. "item4" -> "4").
fn tuple_item_index(item: &str) -> Result<usize, SemanticError> {
    item[4..]
        .parse::<usize>()
        .map_err(|_| SemanticError::undefined_value())
}

fn expr_attribute_self(
    scope: Shared<BlockScope>,
    attr: &Node<String>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let Ok(builtins::SelfField::Address) = builtins::SelfField::from_str(&attr.kind) {
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
        None => Err(SemanticError::undefined_value()),
    }
}

fn expr_bin_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::BinOperation { left, op, right } = &exp.kind {
        let left_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), left)?;
        let right_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), right)?;

        return Ok(ExpressionAttributes::new(
            operations::bin(&left_attributes.typ, &op.kind, &right_attributes.typ)?,
            Location::Value,
        ));
    }

    unreachable!()
}

fn expr_unary_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::UnaryOperation { op, operand } = &exp.kind {
        let operand_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), operand)?;

        match &op.kind {
            fe::UnaryOperator::USub => {
                if !matches!(operand_attributes.typ, Type::Base(Base::Numeric(_))) {
                    return Err(SemanticError::type_error());
                }
                // No matter what numeric type the operand was before, the minus symbol turns it
                // into an I256 just like all positive values default to U256.
                return Ok(ExpressionAttributes::new(
                    Type::Base(Base::Numeric(Integer::I256)),
                    Location::Value,
                ));
            }
            fe::UnaryOperator::Not => {
                return if !matches!(operand_attributes.typ, Type::Base(Base::Bool)) {
                    Err(SemanticError::type_error())
                } else {
                    Ok(ExpressionAttributes::new(
                        Type::Base(Base::Bool),
                        Location::Value,
                    ))
                }
            }
            _ => todo!(),
        }
    }

    unreachable!()
}

fn validate_types_equal(
    expression_a: &ExpressionAttributes,
    expression_b: &ExpressionAttributes,
) -> Result<(), SemanticError> {
    if expression_a.typ == expression_b.typ {
        Ok(())
    } else {
        Err(SemanticError::type_error())
    }
}

pub fn call_arg(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    arg: &Node<fe::CallArg>,
) -> Result<ExpressionAttributes, SemanticError> {
    match &arg.kind {
        fe::CallArg::Arg(value) => {
            let attributes = assignable_expr(scope, Rc::clone(&context), value)?;

            Ok(attributes)
        }
        fe::CallArg::Kwarg(fe::Kwarg { name: _, value }) => assignable_expr(scope, context, value),
    }
}

fn expr_call(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Call { func, args } = &exp.kind {
        return match expr_call_type(Rc::clone(&scope), Rc::clone(&context), func)? {
            CallType::BuiltinFunction { func } => {
                expr_call_builtin_function(scope, context, func, args)
            }
            CallType::TypeConstructor { typ } => {
                expr_call_type_constructor(scope, context, typ, args)
            }
            CallType::SelfAttribute { func_name } => {
                expr_call_self_attribute(scope, context, &func_name, args)
            }
            CallType::ValueAttribute => expr_call_value_attribute(scope, context, func, args),
            CallType::TypeAttribute { typ, func_name } => {
                expr_call_type_attribute(scope, context, typ, &func_name, args)
            }
        };
    }

    unreachable!()
}

fn expr_call_builtin_function(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    typ: GlobalMethod,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    let argument_attributes = expr_call_args(Rc::clone(&scope), Rc::clone(&context), args)?;
    match typ {
        GlobalMethod::Keccak256 => {
            if argument_attributes.len() != 1 {
                return Err(SemanticError::wrong_number_of_params());
            }
            if !matches!(
                expression_attributes_to_types(argument_attributes).first(),
                Some(Type::Array(Array {
                    inner: Base::Byte,
                    ..
                }))
            ) {
                return Err(SemanticError::type_error());
            }
            Ok(ExpressionAttributes::new(Type::Base(U256), Location::Value))
        }
    }
}

fn expr_call_struct_constructor(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    typ: Struct,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    validate_are_kw_args(&args.kind)?;
    let argument_attributes = expr_call_args(Rc::clone(&scope), Rc::clone(&context), args)?;

    if fixed_sizes_to_types(typ.get_field_types())
        != expression_attributes_to_types(argument_attributes)
    {
        return Err(SemanticError::type_error());
    }

    Ok(ExpressionAttributes::new(
        Type::Struct(typ),
        Location::Memory,
    ))
}

fn expr_call_type_constructor(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    typ: Type,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let Type::Struct(val) = typ {
        return expr_call_struct_constructor(scope, context, val, args);
    }

    if args.kind.len() != 1 {
        return Err(SemanticError::wrong_number_of_params());
    }

    let arg_attributes = call_arg(Rc::clone(&scope), Rc::clone(&context), &args.kind[0])?;

    match typ {
        Type::String(ref fe_string) => {
            validate_str_literal_fits_type(&args.kind[0].kind, &fe_string)?;
            Ok(ExpressionAttributes::new(typ, Location::Memory))
        }
        Type::Contract(_) => {
            if arg_attributes.typ != Type::Base(Base::Address) {
                Err(SemanticError::type_error())
            } else {
                Ok(ExpressionAttributes::new(typ, Location::Value))
            }
        }
        Type::Base(Base::Numeric(_)) => {
            let num = validate_is_numeric_literal(&args.kind[0].kind)?;
            validate_numeric_literal_fits_type(&num, &typ)?;
            Ok(ExpressionAttributes::new(typ, Location::Value))
        }
        Type::Base(Base::Address) => {
            match arg_attributes.typ {
                Type::Contract(_) | Type::Base(Base::Numeric(_)) | Type::Base(Base::Address) => {}
                _ => return Err(SemanticError::type_error()),
            }

            Ok(ExpressionAttributes::new(
                Type::Base(Base::Address),
                Location::Value,
            ))
        }
        _ => Err(SemanticError::undefined_value()),
    }
}

fn expr_call_args(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<Vec<ExpressionAttributes>, SemanticError> {
    args.kind
        .iter()
        .map(|arg| call_arg(Rc::clone(&scope), Rc::clone(&context), arg))
        .collect::<Result<Vec<_>, _>>()
}

fn expr_call_self_attribute(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    func_name: &str,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, SemanticError> {
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
        let argument_attributes = expr_call_args(Rc::clone(&scope), Rc::clone(&context), args)?;

        if params.len() != argument_attributes.len() {
            return Err(SemanticError::wrong_number_of_params());
        }

        if fixed_sizes_to_types(params.iter().map(|(_, typ)| typ.to_owned()).collect())
            != expression_attributes_to_types(argument_attributes)
        {
            return Err(SemanticError::type_error());
        }

        let return_location = match &return_type {
            FixedSize::Base(_) => Location::Value,
            _ => Location::Memory,
        };
        Ok(ExpressionAttributes::new(
            return_type.into(),
            return_location,
        ))
    } else {
        Err(SemanticError::undefined_value())
    }
}

fn expr_call_value_attribute(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    func: &Node<fe::Expr>,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Attribute { value, attr } = &func.kind {
        let value_attributes = expr(Rc::clone(&scope), Rc::clone(&context), &value)?;

        if let Type::Contract(contract) = value_attributes.typ {
            return expr_call_contract_attribute(scope, context, contract, &attr.kind, args);
        }

        // for now all of these function expect 0 arguments
        if !args.kind.is_empty() {
            return Err(SemanticError::wrong_number_of_params());
        }

        return match ValueMethod::from_str(&attr.kind)
            .map_err(|_| SemanticError::undefined_value())?
        {
            ValueMethod::Clone => value_attributes.into_cloned(),
            ValueMethod::ToMem => value_attributes.into_cloned_from_sto(),
            ValueMethod::AbiEncode => match &value_attributes.typ {
                Type::Struct(struct_) => {
                    if value_attributes.final_location() != Location::Memory {
                        todo!("encode structs from storage")
                    }

                    Ok(ExpressionAttributes::new(
                        Type::Array(Array {
                            inner: Base::Byte,
                            size: struct_.get_num_fields() * 32,
                        }),
                        Location::Memory,
                    ))
                }
                Type::Tuple(tuple) => {
                    if value_attributes.final_location() != Location::Memory {
                        todo!("encode tuple from storage")
                    }

                    Ok(ExpressionAttributes::new(
                        Type::Array(Array {
                            inner: Base::Byte,
                            size: tuple.items.len() * 32,
                        }),
                        Location::Memory,
                    ))
                }
                _ => todo!(),
            },
            ValueMethod::AbiEncodePacked => todo!(),
        };
    }

    unreachable!()
}

fn expr_call_type_attribute(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    typ: Type,
    func_name: &str,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    let arg_attributes = expr_call_args(Rc::clone(&scope), context, args)?;
    let contract_name = scope.borrow().contract_scope().borrow().name.clone();

    match (typ, ContractTypeMethod::from_str(func_name)) {
        (Type::Contract(contract), Ok(ContractTypeMethod::Create2)) => {
            if arg_attributes.len() != 2 {
                return Err(SemanticError::wrong_number_of_params());
            }

            if contract_name == contract.name {
                return Err(SemanticError::circular_dependency());
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

                Ok(ExpressionAttributes::new(
                    Type::Contract(contract),
                    Location::Value,
                ))
            } else {
                Err(SemanticError::type_error())
            }
        }
        (Type::Contract(contract), Ok(ContractTypeMethod::Create)) => {
            if arg_attributes.len() != 1 {
                return Err(SemanticError::wrong_number_of_params());
            }

            if contract_name == contract.name {
                return Err(SemanticError::circular_dependency());
            }

            if matches!(&arg_attributes[0].typ, Type::Base(Base::Numeric(_))) {
                scope
                    .borrow()
                    .contract_scope()
                    .borrow_mut()
                    .add_created_contract(&contract.name);

                Ok(ExpressionAttributes::new(
                    Type::Contract(contract),
                    Location::Value,
                ))
            } else {
                Err(SemanticError::type_error())
            }
        }
        _ => Err(SemanticError::undefined_value()),
    }
}

fn expr_call_contract_attribute(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    contract: Contract,
    func_name: &str,
    args: &Node<Vec<Node<fe::CallArg>>>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let Some(function) = contract
        .functions
        .iter()
        .find(|function| function.name == func_name)
    {
        let return_type = function.return_type.to_owned();

        if matches!(return_type, FixedSize::String(_)) {
            // we need figure out how to deal with dynamically sized returns
            // for now, this only affects strings
            todo!("external call string returns")
        }

        let argument_attributes = args
            .kind
            .iter()
            .map(|arg| call_arg(Rc::clone(&scope), Rc::clone(&context), arg))
            .collect::<Result<Vec<_>, _>>()?;

        if function.params.len() != argument_attributes.len() {
            return Err(SemanticError::wrong_number_of_params());
        }

        if fixed_sizes_to_types(function.param_types())
            != expression_attributes_to_types(argument_attributes)
        {
            return Err(SemanticError::type_error());
        }

        Ok(ExpressionAttributes::new(
            return_type.clone().into(),
            Location::assign_location(return_type.into())?,
        ))
    } else {
        Err(SemanticError::undefined_value())
    }
}

fn expr_call_type(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    func: &Node<fe::Expr>,
) -> Result<CallType, SemanticError> {
    let call_type = match &func.kind {
        fe::Expr::Name(name) => expr_name_call_type(scope, Rc::clone(&context), name),
        fe::Expr::Attribute { .. } => expr_attribute_call_type(scope, Rc::clone(&context), func),
        _ => Err(SemanticError::not_callable()),
    }?;

    context.borrow_mut().add_call(func, call_type.clone());
    Ok(call_type)
}

fn expr_name_call_type(
    scope: Shared<BlockScope>,
    _context: Shared<Context>,
    name: &str,
) -> Result<CallType, SemanticError> {
    match name {
        "keccak256" => Ok(CallType::BuiltinFunction {
            func: GlobalMethod::Keccak256,
        }),
        "address" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Address),
        }),
        "u256" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U256)),
        }),
        "u128" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U128)),
        }),
        "u64" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U64)),
        }),
        "u32" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U32)),
        }),
        "u16" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U16)),
        }),
        "u8" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::U8)),
        }),
        "i256" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I256)),
        }),
        "i128" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I128)),
        }),
        "i64" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I64)),
        }),
        "i32" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I32)),
        }),
        "i16" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I16)),
        }),
        "i8" => Ok(CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(Integer::I8)),
        }),
        value if value.starts_with("string") => Ok(CallType::TypeConstructor {
            typ: Type::String(
                TryFrom::try_from(value).map_err(|_| SemanticError::undefined_value())?,
            ),
        }),
        value => {
            if let Some(typ) = scope.borrow().get_module_type_def(value) {
                Ok(CallType::TypeConstructor { typ })
            } else {
                Err(SemanticError::undefined_value())
            }
        }
    }
}

fn expr_attribute_call_type(
    scope: Shared<BlockScope>,
    _context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<CallType, SemanticError> {
    if let fe::Expr::Attribute { value, attr } = &exp.kind {
        if let fe::Expr::Name(name) = &value.kind {
            match Object::from_str(&name) {
                Ok(Object::Block) | Ok(Object::Chain) | Ok(Object::Msg) | Ok(Object::Tx) => {
                    return Err(SemanticError::undefined_value())
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

fn validate_are_kw_args(args: &[Node<fe::CallArg>]) -> Result<(), SemanticError> {
    if args
        .iter()
        .any(|arg| matches!(arg.kind, fe::CallArg::Arg(_)))
    {
        return Err(SemanticError::kw_args_required());
    }

    Ok(())
}

fn validate_is_numeric_literal(call_arg: &fe::CallArg) -> Result<String, SemanticError> {
    let value = call_arg_value(call_arg);

    if let fe::Expr::UnaryOperation { operand, op: _ } = &value.kind {
        if let fe::Expr::Num(num) = &operand.kind {
            return Ok(format!("-{}", num));
        }
    } else if let fe::Expr::Num(num) = &value.kind {
        return Ok(num.to_string());
    }

    Err(SemanticError::numeric_literal_expected())
}

fn validate_numeric_literal_fits_type(num: &str, typ: &Type) -> Result<(), SemanticError> {
    if let Type::Base(Base::Numeric(integer)) = typ {
        return if integer.fits(num) {
            Ok(())
        } else {
            Err(SemanticError::numeric_capacity_mismatch())
        };
    }

    Err(SemanticError::type_error())
}

fn validate_str_literal_fits_type(
    call_arg: &fe::CallArg,
    typ: &FeString,
) -> Result<(), SemanticError> {
    if let fe::Expr::Str(lines) = &call_arg_value(call_arg).kind {
        let string_length: usize = lines.join("").len();
        return if string_length > typ.max_size {
            Err(SemanticError::string_capacity_mismatch())
        } else {
            Ok(())
        };
    }

    Err(SemanticError::type_error())
}

fn expr_comp_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::CompOperation { left, op: _, right } = &exp.kind {
        // comparison operands should be moved to the stack
        let left_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), left)?;
        let right_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), right)?;

        validate_types_equal(&left_attributes, &right_attributes)?;

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
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::Ternary {
        if_expr,
        test,
        else_expr,
    } = &exp.kind
    {
        // test attributes should be stored as a value
        let test_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), test)?;
        // the return expressions should be stored in their default locations
        //
        // If, for example, one of the expressions is stored in memory and the other is
        // stored in storage, it's necessary that we move them to the same location.
        // This could be memory or the stack, depending on the type.
        let if_expr_attributes = assignable_expr(Rc::clone(&scope), Rc::clone(&context), if_expr)?;
        let else_expr_attributes =
            assignable_expr(Rc::clone(&scope), Rc::clone(&context), else_expr)?;

        // Make sure the `test_attributes` is a boolean type.
        if Type::Base(Base::Bool) == test_attributes.typ {
            // Should have the same return Type
            if if_expr_attributes.typ == else_expr_attributes.typ {
                // can return else_expr_attributes as well.
                return Ok(ExpressionAttributes::new(
                    if_expr_attributes.typ.clone(),
                    if_expr_attributes.final_location(),
                ));
            }
        }
        return Err(SemanticError::type_error());
    }
    unreachable!()
}

fn expr_bool_operation(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    exp: &Node<fe::Expr>,
) -> Result<ExpressionAttributes, SemanticError> {
    if let fe::Expr::BoolOperation { left, right, .. } = &exp.kind {
        let left_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), left)?;
        let right_attributes = value_expr(Rc::clone(&scope), Rc::clone(&context), right)?;

        let bool_ = Type::Base(Base::Bool);

        return if left_attributes.typ == bool_ && right_attributes.typ == bool_ {
            Ok(ExpressionAttributes::new(bool_, Location::Value))
        } else {
            Err(SemanticError::type_error())
        };
    }

    unreachable!()
}
