use crate::context::FnContext;
use crate::names;
use crate::operations::{
    abi as abi_operations, contracts as contract_operations, data as data_operations,
    math as math_operations, structs as struct_operations,
};
use crate::types::AsAbiType;
use crate::types::EvmSized;
use builtins::{BlockField, ChainField, MsgField, Object, TxField};
use fe_analyzer::builtins;
use fe_analyzer::builtins::{ContractTypeMethod, GlobalMethod};
use fe_analyzer::context::{CallType, Location};
use fe_analyzer::namespace::types::{Base, FixedSize, Type};
use fe_common::numeric;
use fe_common::utils::keccak;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use num_bigint::BigInt;
use std::convert::TryFrom;
use std::str::FromStr;
use yultsur::*;

/// Builds a Yul expression from a Fe expression.
pub fn expr(context: &mut FnContext, exp: &Node<fe::Expr>) -> yul::Expression {
    let expression = match &exp.kind {
        fe::Expr::Name(_) => expr_name(exp),
        fe::Expr::Num(_) => expr_num(exp),
        fe::Expr::Bool(_) => expr_bool(exp),
        fe::Expr::Subscript { .. } => expr_subscript(context, exp),
        fe::Expr::Attribute { .. } => expr_attribute(context, exp),
        fe::Expr::Ternary { .. } => expr_ternary(context, exp),
        fe::Expr::BoolOperation { .. } => expr_bool_operation(context, exp),
        fe::Expr::BinOperation { .. } => expr_bin_operation(context, exp),
        fe::Expr::UnaryOperation { .. } => expr_unary_operation(context, exp),
        fe::Expr::CompOperation { .. } => expr_comp_operation(context, exp),
        fe::Expr::Call { .. } => expr_call(context, exp),
        fe::Expr::List { .. } => panic!("list expressions should be lowered"),
        fe::Expr::Tuple { .. } => panic!("tuple expressions should be lowered"),
        fe::Expr::Str(val) => {
            context.contract.string_literals.insert(val.clone());
            expr_str(exp)
        }
        fe::Expr::Unit => expression! { 0x0 },
    };

    let attributes = context
        .expression_attributes(exp)
        .expect("missing expression attributes");
    match (
        attributes.location.to_owned(),
        attributes.move_location.to_owned(),
    ) {
        (from, Some(to)) => move_expression(expression, attributes.typ.to_owned(), from, to),
        (_, None) => expression,
    }
}

fn move_expression(
    val: yul::Expression,
    typ: Type,
    from: Location,
    to: Location,
) -> yul::Expression {
    let fixed_size = FixedSize::try_from(typ.clone()).expect("Invalid type");

    match (from, to) {
        (Location::Storage { .. }, Location::Value) => {
            if let Type::Base(Base::Numeric(integer)) = typ {
                math_operations::adjust_numeric_size(
                    &integer,
                    data_operations::sload(fixed_size, val),
                )
            } else {
                data_operations::sload(fixed_size, val)
            }
        }
        (Location::Memory, Location::Value) => {
            if let Type::Base(Base::Numeric(integer)) = typ {
                math_operations::adjust_numeric_size(
                    &integer,
                    data_operations::mload(fixed_size, val),
                )
            } else {
                data_operations::mload(fixed_size, val)
            }
        }
        (Location::Memory, Location::Memory) => data_operations::mcopym(fixed_size, val),
        (Location::Storage { .. }, Location::Memory) => data_operations::scopym(fixed_size, val),
        _ => panic!("invalid expression move: {:?} {:?}", from, to),
    }
}

fn expr_call(context: &mut FnContext, exp: &Node<fe::Expr>) -> yul::Expression {
    let (args, func) = match &exp.kind {
        fe::Expr::Call { args, func, .. } => (args, func),
        _ => unreachable!(),
    };
    let call_type = context.call_type(func).expect("missing call type");
    let yul_args: Vec<yul::Expression> = args
        .kind
        .iter()
        .map(|arg| expr(context, &arg.kind.value))
        .collect();

    return match call_type {
        CallType::BuiltinFunction(func) => match func {
            GlobalMethod::Keccak256 => {
                let first_arg = &args.kind.first().expect("Missing argument").kind.value;
                let attributes = context
                    .expression_attributes(first_arg)
                    .expect("missing attributes");
                let size = FixedSize::try_from(attributes.typ.clone()).expect("Invalid type");
                let func_name = identifier! { (func.as_ref()) };
                let size = identifier_expression! { (size.size()) };
                expression! { [func_name]([yul_args[0].to_owned()], [size]) }
            }
            GlobalMethod::SendValue => {
                expression! { send_value([yul_args[0].to_owned()], [yul_args[1].to_owned()]) }
            }
        },
        CallType::TypeConstructor {
            typ: Type::Struct(val),
        } => struct_operations::new(&val, yul_args),
        CallType::TypeConstructor {
            typ: Type::Base(Base::Numeric(integer)),
        } => math_operations::adjust_numeric_size(&integer, yul_args[0].to_owned()),
        CallType::TypeConstructor { .. } => yul_args[0].to_owned(),
        CallType::Pure(func) => {
            let func_name = names::func_name(&func.name(context.db));
            expression! { [func_name]([yul_args...]) }
        }
        CallType::SelfAttribute { func_name, .. } => {
            let func_name = names::func_name(&func_name);
            expression! { [func_name]([yul_args...]) }
        }

        CallType::ValueAttribute => {
            if let fe::Expr::Attribute { value, attr } = &func.kind {
                let value_attributes = context
                    .expression_attributes(value)
                    .expect("invalid attributes");

                return match (value_attributes.typ.to_owned(), &attr.kind) {
                    (Type::Contract(contract), func_name) => contract_operations::call(
                        contract,
                        func_name,
                        expr(context, value),
                        yul_args,
                    ),
                    (typ, func_name) => {
                        match builtins::ValueMethod::from_str(func_name)
                            .expect("uncaught analyzer error")
                        {
                            // Copying is done in `expr(..)` based on the move location set
                            // in the expression's attributes, so we just map the value for
                            // `to_mem` and `clone`.
                            builtins::ValueMethod::ToMem => expr(context, value),
                            builtins::ValueMethod::Clone => expr(context, value),
                            builtins::ValueMethod::AbiEncode => match typ {
                                Type::Struct(struct_) => abi_operations::encode(
                                    &[struct_.as_abi_type(context.db)],
                                    vec![expr(context, value)],
                                ),
                                _ => panic!("invalid attributes"),
                            },
                        }
                    }
                };
            }

            panic!("invalid attributes")
        }
        CallType::TypeAttribute { typ, func_name } => {
            match (
                typ,
                ContractTypeMethod::from_str(func_name.as_str()).expect("invalid attributes"),
            ) {
                (Type::Contract(contract), ContractTypeMethod::Create2) => {
                    context
                        .contract
                        .created_contracts
                        .insert(contract.name.clone());
                    contract_operations::create2(
                        &contract,
                        yul_args[0].to_owned(),
                        yul_args[1].to_owned(),
                    )
                }
                (Type::Contract(contract), ContractTypeMethod::Create) => {
                    context
                        .contract
                        .created_contracts
                        .insert(contract.name.clone());
                    contract_operations::create(&contract, yul_args[0].to_owned())
                }
                _ => panic!("invalid attributes"),
            }
        }
    };
}

pub fn expr_comp_operation(context: &mut FnContext, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::CompOperation { left, op, right } = &exp.kind {
        let yul_left = expr(context, left);
        let yul_right = expr(context, right);

        let typ = &context
            .expression_attributes(left)
            .expect("Missing `left` expression in context")
            .typ;

        return match op.kind {
            fe::CompOperator::Eq => expression! { eq([yul_left], [yul_right]) },
            fe::CompOperator::NotEq => expression! { iszero((eq([yul_left], [yul_right]))) },
            fe::CompOperator::Lt => match typ.is_signed_integer() {
                true => expression! { slt([yul_left], [yul_right]) },
                false => expression! { lt([yul_left], [yul_right]) },
            },
            fe::CompOperator::LtE => match typ.is_signed_integer() {
                true => expression! { iszero((sgt([yul_left], [yul_right]))) },
                false => expression! { iszero((gt([yul_left], [yul_right]))) },
            },
            fe::CompOperator::Gt => match typ.is_signed_integer() {
                true => expression! { sgt([yul_left], [yul_right]) },
                false => expression! { gt([yul_left], [yul_right]) },
            },
            fe::CompOperator::GtE => match typ.is_signed_integer() {
                true => expression! { iszero((slt([yul_left], [yul_right]))) },
                false => expression! { iszero((lt([yul_left], [yul_right]))) },
            },
        };
    }

    unreachable!()
}

pub fn expr_bin_operation(context: &mut FnContext, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::BinOperation { left, op, right } = &exp.kind {
        let yul_left = expr(context, left);
        let yul_right = expr(context, right);

        let typ = &context
            .expression_attributes(left)
            .expect("Missing `left` expression in context")
            .typ;

        return match op.kind {
            fe::BinOperator::Add => match typ {
                Type::Base(Base::Numeric(integer)) => {
                    expression! { [names::checked_add(integer)]([yul_left], [yul_right]) }
                }
                _ => unimplemented!("Addition for non-numeric types not yet supported"),
            },
            fe::BinOperator::Sub => match typ {
                Type::Base(Base::Numeric(integer)) => {
                    expression! { [names::checked_sub(integer)]([yul_left], [yul_right]) }
                }
                _ => unimplemented!("Subtraction for non-numeric types not yet supported"),
            },
            fe::BinOperator::Mult => match typ {
                Type::Base(Base::Numeric(integer)) => {
                    expression! { [names::checked_mul(integer)]([yul_left], [yul_right]) }
                }
                _ => unreachable!(),
            },
            fe::BinOperator::Div => match typ {
                Type::Base(Base::Numeric(integer)) => {
                    expression! { [names::checked_div(integer)]([yul_left], [yul_right]) }
                }
                _ => unreachable!(),
            },
            fe::BinOperator::BitAnd => expression! { and([yul_left], [yul_right]) },
            fe::BinOperator::BitOr => expression! { or([yul_left], [yul_right]) },
            fe::BinOperator::BitXor => expression! { xor([yul_left], [yul_right]) },
            fe::BinOperator::LShift => match typ {
                Type::Base(Base::Numeric(integer)) => math_operations::adjust_numeric_size(
                    integer,
                    expression! { shl([yul_right], [math_operations::adjust_numeric_size(integer, yul_left)]) },
                ),
                _ => unreachable!(),
            },
            fe::BinOperator::RShift => match typ.is_signed_integer() {
                true => expression! { sar([yul_right], [yul_left]) },
                false => expression! { shr([yul_right], [yul_left]) },
            },
            fe::BinOperator::Mod => match typ {
                Type::Base(Base::Numeric(integer)) => {
                    expression! { [names::checked_mod(integer)]([yul_left], [yul_right]) }
                }
                _ => unreachable!(),
            },
            fe::BinOperator::Pow => match typ {
                Type::Base(Base::Numeric(integer)) => {
                    expression! { [names::checked_exp(integer)]([yul_left], [yul_right]) }
                }
                _ => unreachable!(),
            },
        };
    }

    unreachable!()
}

pub fn expr_unary_operation(context: &mut FnContext, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::UnaryOperation { op, operand } = &exp.kind {
        let yul_operand = expr(context, operand);

        let typ = &context
            .expression_attributes(operand)
            .expect("Missing `operand` expression in context")
            .typ;

        return match &op.kind {
            fe::UnaryOperator::USub => match typ {
                Type::Base(Base::Numeric(integer)) if integer.is_signed() => {
                    expression! { [names::checked_neg(integer)]([yul_operand]) }
                }
                _ => unreachable!(),
            },
            fe::UnaryOperator::Not => expression! { iszero([yul_operand]) },
            fe::UnaryOperator::Invert => match typ {
                Type::Base(Base::Numeric(integer)) => {
                    math_operations::adjust_numeric_size(integer, expression! { not([yul_operand])})
                }
                _ => unreachable!(),
            },
        };
    }

    unreachable!()
}

/// Retrieves the String value of a name expression.
pub fn expr_name_string(exp: &Node<fe::Expr>) -> String {
    if let fe::Expr::Name(name) = &exp.kind {
        return name.to_owned();
    }

    unreachable!()
}

fn expr_name(exp: &Node<fe::Expr>) -> yul::Expression {
    let name = expr_name_string(exp);

    identifier_expression! { [names::var_name(&name)] }
}

fn expr_num(exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Num(num) = &exp.kind {
        let literal = numeric::Literal::new(num);
        let num = literal.parse::<BigInt>().expect("Invalid numeric literal");
        let num = if matches!(literal.radix(), numeric::Radix::Decimal) {
            format!("{}", num)
        } else {
            format!("{:#x}", num)
        };

        return literal_expression! {(num)};
    }

    unreachable!()
}

fn expr_bool(exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Bool(val) = &exp.kind {
        return literal_expression! {(val)};
    }

    unreachable!()
}

fn expr_str(exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Str(content) = &exp.kind {
        let string_identifier = format!(r#""{}""#, keccak::full(content.as_bytes()));

        let offset = expression! { dataoffset([literal_expression! { (string_identifier) }]) };
        let size = expression! { datasize([literal_expression! { (string_identifier) }]) };

        return expression! {load_data_string([offset], [size])};
    }

    unreachable!()
}

fn expr_subscript(context: &mut FnContext, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Subscript {
        value: value_node,
        index: index_node,
    } = &exp.kind
    {
        let value = expr(context, value_node);
        let index = expr(context, index_node);

        let value_attributes = context
            .expression_attributes(value_node)
            .expect("missing expr attributes");

        return match &value_attributes.typ {
            Type::Map(_) => data_operations::keyed_map(value, index),
            Type::Array(array) => data_operations::indexed_array(array.clone(), value, index),
            _ => panic!("invalid attributes"),
        };
    }

    unreachable!()
}

fn expr_attribute(context: &mut FnContext, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Attribute { value, attr } = &exp.kind {
        // If the given value has expression attributes, we handle it as an expression
        // by first mapping the value and then performing the expected operation
        // inferred from the attribute name.
        //
        // If the given value does not have expression attributes, we assume it is a
        // builtin object and map it as such.
        return if let Some(attributes) = context.expression_attributes(value).cloned() {
            let value = expr(context, value);

            match &attributes.typ {
                Type::Struct(struct_) => {
                    struct_operations::get_attribute(struct_, &attr.kind, value)
                }
                _ => panic!("invalid attributes"),
            }
        } else {
            match Object::from_str(&expr_name_string(value)) {
                Ok(Object::Self_) => expr_attribute_self(context, exp),
                Ok(Object::Block) => match BlockField::from_str(&attr.kind) {
                    Ok(BlockField::Coinbase) => expression! { coinbase() },
                    Ok(BlockField::Difficulty) => expression! { difficulty() },
                    Ok(BlockField::Number) => expression! { number() },
                    Ok(BlockField::Timestamp) => expression! { timestamp() },
                    Err(_) => panic!("invalid `block` attribute name"),
                },
                Ok(Object::Chain) => match ChainField::from_str(&attr.kind) {
                    Ok(ChainField::Id) => expression! { chainid() },
                    Err(_) => panic!("invalid `chain` attribute name"),
                },
                Ok(Object::Msg) => match MsgField::from_str(&attr.kind) {
                    Ok(MsgField::Sender) => expression! { caller() },
                    Ok(MsgField::Sig) => expression! { cloadn(0, 4) },
                    Ok(MsgField::Value) => expression! { callvalue() },
                    Err(_) => panic!("invalid `msg` attribute name"),
                },
                Ok(Object::Tx) => match TxField::from_str(&attr.kind) {
                    Ok(TxField::GasPrice) => expression! { gasprice() },
                    Ok(TxField::Origin) => expression! { origin() },
                    Err(_) => panic!("invalid `msg` attribute name"),
                },
                Err(_) => panic!("invalid attributes"),
            }
        };
    }

    unreachable!()
}

fn expr_attribute_self(context: &mut FnContext, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Attribute { attr, .. } = &exp.kind {
        if let Ok(builtins::SelfField::Address) = builtins::SelfField::from_str(&attr.kind) {
            return expression! { address() };
        }
    }

    if let Some(attributes) = context.expression_attributes(exp) {
        let nonce = if let Location::Storage { nonce: Some(nonce) } = attributes.location {
            nonce
        } else {
            panic!("invalid attributes");
        };

        return match attributes.typ {
            Type::Map(_) => literal_expression! { (nonce) },
            _ => nonce_to_ptr(nonce),
        };
    }

    panic!("missing attributes")
}

/// Converts a storage nonce into a pointer based on the keccak256 hash
///
/// Pointers created here have the last byte set to zero. This is to ensure that
/// our byte pointer sits at the start of a word (32 | `ptr` ).
pub fn nonce_to_ptr(nonce: usize) -> yul::Expression {
    // set the last byte to `0x00` to ensure our pointer sits at the start of a word
    let ptr = keccak::partial_right_padded(nonce.to_string().as_bytes(), 31);
    literal_expression! { (ptr) }
}

fn expr_ternary(context: &mut FnContext, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Ternary {
        if_expr,
        test,
        else_expr,
    } = &exp.kind
    {
        let yul_test_expr = expr(context, test);
        let yul_if_expr = expr(context, if_expr);
        let yul_else_expr = expr(context, else_expr);

        return expression! {ternary([yul_test_expr], [yul_if_expr], [yul_else_expr])};
    }
    unreachable!()
}

fn expr_bool_operation(context: &mut FnContext, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::BoolOperation { left, op, right } = &exp.kind {
        let yul_left = expr(context, left);
        let yul_right = expr(context, right);

        return match op.kind {
            fe::BoolOperator::And => expression! {and([yul_left], [yul_right])},
            fe::BoolOperator::Or => expression! {or([yul_left], [yul_right])},
        };
    }

    unreachable!()
}
