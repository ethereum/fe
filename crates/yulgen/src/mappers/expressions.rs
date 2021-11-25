use crate::context::FnContext;
use crate::names;
use crate::operations::{
    abi as abi_operations, contracts as contract_operations, data as data_operations,
    math as math_operations, structs as struct_operations,
};
use crate::types::{AsAbiType, EvmSized};
use fe_analyzer::builtins::{
    self, BlockField, ChainField, ContractSelfField, ContractTypeMethod, GlobalFunction,
    GlobalObject, MsgField, TxField,
};
use fe_analyzer::context::{CallType, Location};
use fe_analyzer::namespace::items::Class;
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
        fe::Expr::Path(_) => panic!("path expressions should be lowered or rejected"),
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
            GlobalFunction::Keccak256 => {
                let first_arg = &args.kind.first().expect("Missing argument").kind.value;
                let attributes = context
                    .expression_attributes(first_arg)
                    .expect("missing attributes");
                let size = FixedSize::try_from(attributes.typ.clone()).expect("Invalid type");
                let func_name = identifier! { (func.as_ref()) };
                let size = identifier_expression! { (size.size()) };
                expression! { [func_name]([yul_args[0].to_owned()], [size]) }
            }
            GlobalFunction::SendValue => {
                expression! { send_value([yul_args[0].to_owned()], [yul_args[1].to_owned()]) }
            }
            GlobalFunction::Balance => {
                expression! { selfbalance() }
            }
            GlobalFunction::BalanceOf => {
                expression! { balance([yul_args[0].to_owned()]) }
            }
        },
        CallType::BuiltinValueMethod(method) => {
            let target = match &func.kind {
                fe::Expr::Attribute { value, .. } => value,
                _ => unreachable!(),
            };
            match method {
                // Copying is done in `expr(..)` based on the move location set
                // in the expression's attributes, so we just map the value for
                // `to_mem` and `clone`.
                builtins::ValueMethod::ToMem => expr(context, target),
                builtins::ValueMethod::Clone => expr(context, target),
                builtins::ValueMethod::AbiEncode => match context
                    .expression_attributes(target)
                    .expect("missing expr attributes")
                    .typ
                    .clone()
                {
                    Type::Struct(struct_) => abi_operations::encode(
                        &[struct_.as_abi_type(context.db)],
                        vec![expr(context, target)],
                    ),
                    _ => panic!("invalid attributes"),
                },
            }
        }
        CallType::TypeConstructor(Type::Struct(val)) => struct_operations::new(&val, yul_args),
        CallType::TypeConstructor(Type::Base(Base::Numeric(integer))) => {
            math_operations::adjust_numeric_size(&integer, yul_args[0].to_owned())
        }
        CallType::TypeConstructor(_) => yul_args[0].to_owned(),
        CallType::Pure(func) => {
            let func_name = names::func_name(&func.name(context.db));
            expression! { [func_name]([yul_args...]) }
        }
        CallType::BuiltinAssociatedFunction { contract, function } => {
            let contract_name = contract.name(context.db);
            context
                .contract
                .created_contracts
                .insert(contract_name.clone());
            match function {
                ContractTypeMethod::Create2 => contract_operations::create2(
                    &contract_name,
                    yul_args[0].to_owned(),
                    yul_args[1].to_owned(),
                ),
                ContractTypeMethod::Create => {
                    contract_operations::create(&contract_name, yul_args[0].to_owned())
                }
            }
        }
        CallType::AssociatedFunction { class, function } => {
            assert!(
                matches!(class, Class::Struct(_)),
                "call to contract-associated fn should be rejected by analyzer as not-yet-implemented"
            );
            let func_name = names::associated_function_name(
                &class.name(context.db),
                &function.name(context.db),
            );
            expression! { [func_name]([yul_args...]) }
        }
        CallType::ValueMethod {
            is_self,
            class,
            method,
        } => {
            let target = match &func.kind {
                fe::Expr::Attribute { value, .. } => value,
                _ => unreachable!(),
            };

            let class_name = class.name(context.db);
            match class {
                Class::Contract(contract) => {
                    if is_self {
                        // TODO: contract fns should use names::associated_function_name
                        let fn_name = names::func_name(&method.name(context.db));
                        expression! { [fn_name]([yul_args...]) }
                    } else {
                        let address = expr(context, target);
                        let fn_name = names::contract_call(
                            &contract.name(context.db),
                            &method.name(context.db),
                        );
                        expression! { [fn_name]([address], [yul_args...]) }
                    }
                }
                Class::Struct(_) => {
                    let target = expr(context, target);
                    let fn_name =
                        names::associated_function_name(&class_name, &method.name(context.db));
                    expression! { [fn_name]([target], [yul_args...]) }
                }
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
            fe::UnaryOperator::USub => {
                match typ {
                    Type::Base(Base::Numeric(integer)) => {
                        if let fe::Expr::Num(_) = &operand.kind {
                            // Literals are checked at compile time (e.g. -128) so there's no point
                            // in adding a runtime check.
                            let zero = literal_expression! {0};
                            expression! { sub([zero], [yul_operand]) }
                        } else {
                            expression! { [names::checked_neg(integer)]([yul_operand]) }
                        }
                    }
                    _ => unreachable!(),
                }
            }
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
    let (target, field) = match &exp.kind {
        fe::Expr::Attribute { value, attr } => (value, attr),
        _ => unreachable!(),
    };

    // Check if it's a magical global object first.
    if let fe::Expr::Name(name) = &target.kind {
        match GlobalObject::from_str(name) {
            Ok(GlobalObject::Block) => match BlockField::from_str(&field.kind) {
                Ok(BlockField::Coinbase) => return expression! { coinbase() },
                Ok(BlockField::Difficulty) => return expression! { difficulty() },
                Ok(BlockField::Number) => return expression! { number() },
                Ok(BlockField::Timestamp) => return expression! { timestamp() },
                Err(_) => panic!("invalid `block` attribute name"),
            },
            Ok(GlobalObject::Chain) => match ChainField::from_str(&field.kind) {
                Ok(ChainField::Id) => return expression! { chainid() },
                Err(_) => panic!("invalid `chain` attribute name"),
            },
            Ok(GlobalObject::Msg) => match MsgField::from_str(&field.kind) {
                Ok(MsgField::Sender) => return expression! { caller() },
                Ok(MsgField::Sig) => return expression! { cloadn(0, 4) },
                Ok(MsgField::Value) => return expression! { callvalue() },
                Err(_) => panic!("invalid `msg` attribute name"),
            },
            Ok(GlobalObject::Tx) => match TxField::from_str(&field.kind) {
                Ok(TxField::GasPrice) => return expression! { gasprice() },
                Ok(TxField::Origin) => return expression! { origin() },
                Err(_) => panic!("invalid `msg` attribute name"),
            },
            Err(_) => {}
        }
    }

    let target_attrs = context
        .expression_attributes(target)
        .unwrap_or_else(|| panic!("missing attributes for expr: {:?}", target))
        .clone();

    match &target_attrs.typ {
        Type::Contract(_) => {
            // TODO: verify that this is caught by analyzer
            unreachable!("only `self` contract fields can be accessed for now")
        }
        Type::SelfContract(_) => {
            if let Ok(ContractSelfField::Address) = ContractSelfField::from_str(&field.kind) {
                return expression! { address() };
            }
            let exp_attrs = context
                .expression_attributes(exp)
                .expect("missing expr attributes");
            let nonce = match exp_attrs.location {
                Location::Storage { nonce: Some(nonce) } => nonce,
                _ => unreachable!("expected contract `self` field to be in storage and have nonce"),
            };
            match exp_attrs.typ {
                Type::Map(_) => literal_expression! { (nonce) },
                _ => nonce_to_ptr(nonce),
            }
        }
        Type::Struct(struct_) => {
            // struct `self` is handled like any other struct value,
            // and keeps the name `self` in the generated yul.
            let target = expr(context, target);
            struct_operations::get_attribute(struct_, &field.kind, target)
        }
        _ => panic!("invalid type for field access: {:?}", &target_attrs.typ),
    }
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
