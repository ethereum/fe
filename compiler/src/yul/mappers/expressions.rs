use crate::yul::names;
use crate::yul::operations::{
    abi as abi_operations, contracts as contract_operations, data as data_operations,
    structs as struct_operations,
};
use crate::yul::utils::call_arg_value;
use builtins::{BlockField, ChainField, MsgField, Object, TxField};
use fe_analyzer::builtins;
use fe_analyzer::builtins::{ContractTypeMethod, GlobalMethod};
use fe_analyzer::context::{CallType, Context, Location};
use fe_analyzer::namespace::types::{Base, FeSized, FixedSize, Type};
use fe_common::utils::keccak;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::convert::TryFrom;
use std::str::FromStr;
use yultsur::*;

/// Builds a Yul expression from a Fe expression.
pub fn expr(context: &Context, exp: &Node<fe::Expr>) -> yul::Expression {
    if let Some(attributes) = context.get_expression(exp) {
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
            fe::Expr::List { .. } => unimplemented!(),
            fe::Expr::ListComp { .. } => unimplemented!(),
            fe::Expr::Tuple { .. } => expr_tuple(exp),
            fe::Expr::Str(_) => expr_str(exp),
        };

        match (
            attributes.location.to_owned(),
            attributes.move_location.to_owned(),
        ) {
            (from, Some(to)) => move_expression(expression, attributes.typ.to_owned(), from, to),
            (_, None) => expression,
        }
    } else {
        panic!("missing expression attributes for {:?}", exp)
    }
}

fn move_expression(
    val: yul::Expression,
    typ: Type,
    from: Location,
    to: Location,
) -> yul::Expression {
    let typ = FixedSize::try_from(typ).expect("Invalid type");

    match (from.clone(), to.clone()) {
        (Location::Storage { .. }, Location::Value) => data_operations::sload(typ, val),
        (Location::Memory, Location::Value) => data_operations::mload(typ, val),
        (Location::Memory, Location::Memory) => data_operations::mcopym(typ, val),
        (Location::Storage { .. }, Location::Memory) => data_operations::scopym(typ, val),
        _ => panic!("invalid expression move: {:?} {:?}", from, to),
    }
}

pub fn call_arg(context: &Context, arg: &Node<fe::CallArg>) -> yul::Expression {
    match &arg.kind {
        fe::CallArg::Arg(value) => expr(context, value),
        fe::CallArg::Kwarg(fe::Kwarg { name: _, value }) => expr(context, value),
    }
}

fn expr_call(context: &Context, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Call { args, func } = &exp.kind {
        if let Some(call_type) = context.get_call(func) {
            let yul_args: Vec<yul::Expression> =
                args.kind.iter().map(|val| call_arg(context, val)).collect();

            return match call_type {
                CallType::BuiltinFunction { func } => match func {
                    GlobalMethod::Keccak256 => {
                        let first_arg =
                            call_arg_value(&args.kind.first().expect("Missing argument").kind);
                        let attributes = context
                            .get_expression(first_arg)
                            .expect("missing attributes");
                        let size =
                            FixedSize::try_from(attributes.typ.clone()).expect("Invalid type");
                        let func_name: &str = func.into();

                        let func_name = identifier! { (func_name) };
                        let size = identifier_expression! { (size.size()) };
                        expression! { [func_name]([yul_args[0].to_owned()], [size]) }
                    }
                },
                CallType::TypeConstructor {
                    typ: Type::Struct(val),
                } => struct_operations::new(val, yul_args),
                CallType::TypeConstructor { .. } => yul_args[0].to_owned(),
                CallType::SelfAttribute { func_name } => {
                    let func_name = names::func_name(func_name);
                    expression! { [func_name]([yul_args...]) }
                }
                CallType::ValueAttribute => {
                    if let fe::Expr::Attribute { value, attr } = &func.kind {
                        let value_attributes =
                            context.get_expression(value).expect("invalid attributes");

                        return match (value_attributes.typ.to_owned(), &attr.kind) {
                            (Type::Contract(contract), func_name) => contract_operations::call(
                                contract,
                                &func_name,
                                expr(context, value),
                                yul_args,
                            ),
                            (typ, func_name) => {
                                match builtins::ValueMethod::from_str(&func_name)
                                    .expect("uncaught analyzer error")
                                {
                                    // Copying is done in `expr(..)` based on the move location set
                                    // in the expression's attributes, so we just map the value for
                                    // `to_mem` and `clone`.
                                    builtins::ValueMethod::ToMem => expr(context, value),
                                    builtins::ValueMethod::Clone => expr(context, value),
                                    builtins::ValueMethod::AbiEncode => match typ {
                                        Type::Struct(struct_) => abi_operations::encode(
                                            vec![struct_],
                                            vec![expr(context, value)],
                                        ),
                                        _ => panic!("invalid attributes"),
                                    },
                                    builtins::ValueMethod::AbiEncodePacked => todo!(),
                                }
                            }
                        };
                    }

                    panic!("invalid attributes")
                }
                CallType::TypeAttribute { typ, func_name } => {
                    match (
                        typ,
                        ContractTypeMethod::from_str(func_name.as_str())
                            .expect("invalid attributes"),
                    ) {
                        (Type::Contract(contract), ContractTypeMethod::Create2) => {
                            contract_operations::create2(
                                &contract,
                                yul_args[0].to_owned(),
                                yul_args[1].to_owned(),
                            )
                        }
                        (Type::Contract(contract), ContractTypeMethod::Create) => {
                            contract_operations::create(&contract, yul_args[0].to_owned())
                        }
                        _ => panic!("invalid attributes"),
                    }
                }
            };
        }
    }

    unreachable!()
}

pub fn expr_comp_operation(context: &Context, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::CompOperation { left, op, right } = &exp.kind {
        let yul_left = expr(context, left);
        let yul_right = expr(context, right);

        let typ = &context
            .get_expression(left)
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
            _ => unimplemented!(),
        };
    }

    unreachable!()
}

pub fn expr_bin_operation(context: &Context, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::BinOperation { left, op, right } = &exp.kind {
        let yul_left = expr(context, left);
        let yul_right = expr(context, right);

        let typ = &context
            .get_expression(left)
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
            fe::BinOperator::LShift => expression! { shl([yul_right], [yul_left]) },
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
            fe::BinOperator::FloorDiv => unimplemented!(),
        };
    }

    unreachable!()
}

pub fn expr_unary_operation(context: &Context, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::UnaryOperation { op, operand } = &exp.kind {
        let yul_operand = expr(context, operand);

        return match &op.kind {
            fe::UnaryOperator::USub => {
                let zero = literal_expression! {0};
                expression! { sub([zero], [yul_operand]) }
            }
            fe::UnaryOperator::Not => expression! { iszero([yul_operand]) },
            _ => todo!(),
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

/// Builds a Yul expression from the first slice, if it is an index.
pub fn slices_index(context: &Context, slices: &Node<Vec<Node<fe::Slice>>>) -> yul::Expression {
    if let Some(first_slice) = slices.kind.first() {
        return slice_index(context, first_slice);
    }

    unreachable!()
}

pub fn slice_index(context: &Context, slice: &Node<fe::Slice>) -> yul::Expression {
    if let fe::Slice::Index(index) = &slice.kind {
        return expr(context, index);
    }

    unreachable!()
}

fn expr_tuple(exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Tuple { elts } = &exp.kind {
        if !elts.is_empty() {
            todo!("Non empty Tuples aren't yet supported")
        } else {
            return literal_expression! {0x0};
        }
    }

    unreachable!()
}

fn expr_name(exp: &Node<fe::Expr>) -> yul::Expression {
    let name = expr_name_string(exp);

    identifier_expression! { [names::var_name(&name)] }
}

fn expr_num(exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Num(num) = &exp.kind {
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
    if let fe::Expr::Str(lines) = &exp.kind {
        let content = lines.join("");
        let string_identifier = format!(r#""{}""#, keccak::full(content.as_bytes()));

        let offset = expression! { dataoffset([literal_expression! { (string_identifier) }]) };
        let size = expression! { datasize([literal_expression! { (string_identifier) }]) };

        return expression! {load_data_string([offset], [size])};
    }

    unreachable!()
}

fn expr_subscript(context: &Context, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Subscript { value, slices } = &exp.kind {
        if let Some(value_attributes) = context.get_expression(value) {
            let value = expr(context, value);
            let index = slices_index(context, slices);

            return match value_attributes.typ.to_owned() {
                Type::Map(_) => data_operations::keyed_map(value, index),
                Type::Array(array) => data_operations::indexed_array(array, value, index),
                _ => panic!("invalid attributes"),
            };
        }

        panic!("missing attributes");
    }

    unreachable!()
}

fn expr_attribute(context: &Context, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Attribute { value, attr } = &exp.kind {
        // If the given value has expression attributes, we handle it as an expression
        // by first mapping the value and then performing the expected operation
        // inferred from the attribute name.
        //
        // If the given value does not have expression attributes, we assume it is a
        // builtin object and map it as such.
        return if let Some(attributes) = context.get_expression(value) {
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
                    Ok(MsgField::Data) => todo!(),
                    Ok(MsgField::Sender) => expression! { caller() },
                    Ok(MsgField::Sig) => expression! {
                        and(
                            [ expression! { calldataload(0) } ],
                            [ expression! { shl(224, 0xffffffff) } ]
                        )
                    },
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

fn expr_attribute_self(context: &Context, exp: &Node<fe::Expr>) -> yul::Expression {
    if let fe::Expr::Attribute { attr, .. } = &exp.kind {
        if let Ok(builtins::SelfField::Address) = builtins::SelfField::from_str(&attr.kind) {
            return expression! { address() };
        }
    }

    if let Some(attributes) = context.get_expression(exp) {
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

fn expr_ternary(context: &Context, exp: &Node<fe::Expr>) -> yul::Expression {
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

fn expr_bool_operation(context: &Context, exp: &Node<fe::Expr>) -> yul::Expression {
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

#[cfg(test)]
#[cfg(feature = "fix-context-harness")]
mod tests {
    use crate::yul::mappers::expressions::{expr, Location};
    use fe_analyzer::namespace::types::{Array, Base, Map, Type, U256};
    use fe_analyzer::test_utils::ContextHarness;
    use fe_analyzer::{Context, ExpressionAttributes};
    use fe_parser as parser;
    use rstest::rstest;

    fn map(context: &Context, src: &str) -> String {
        let tokens = parser::get_parse_tokens(src).expect("Couldn't parse expression");
        let expression = &parser::parsers::expr(&tokens[..])
            .expect("Couldn't build expression AST")
            .1;

        expr(context, expression)
            .expect("Couldn't map expression AST")
            .to_string()
    }

    #[test]
    fn map_sload_u256() {
        let mut harness = ContextHarness::new("self.foo[3]");

        harness.add_expression(
            "3",
            ExpressionAttributes::new(Type::Base(U256), Location::Value),
        );

        harness.add_expression(
            "self.foo",
            ExpressionAttributes::new(
                Type::Map(Map {
                    key: Base::Address,
                    value: Box::new(Type::Base(U256)),
                }),
                Location::Storage { nonce: Some(0) },
            ),
        );

        let mut attributes =
            ExpressionAttributes::new(Type::Base(U256), Location::Storage { nonce: None });
        attributes.move_location = Some(Location::Value);
        harness.add_expression("self.foo[3]", attributes);

        let result = map(&harness.context, &harness.src);

        assert_eq!(result, "bytes_sloadn(map_value_ptr(0, 3), 32)");
    }

    #[test]
    fn map_sload_with_array_elem() {
        let mut harness = ContextHarness::new("self.foo_map[bar_array[index]]");

        let foo_key = Base::Address;
        let foo_value = Type::Array(Array {
            size: 8,
            inner: Base::Address,
        });
        let bar_value = Type::Base(Base::Address);

        harness.add_expression(
            "self.foo_map",
            ExpressionAttributes::new(
                Type::Map(Map {
                    key: foo_key,
                    value: Box::new(foo_value.clone()),
                }),
                Location::Storage { nonce: Some(0) },
            ),
        );

        harness.add_expression(
            "bar_array",
            ExpressionAttributes::new(
                Type::Array(Array {
                    size: 100,
                    inner: Base::Address,
                }),
                Location::Memory,
            ),
        );

        harness.add_expression(
            "index",
            ExpressionAttributes::new(Type::Base(U256), Location::Value),
        );

        let mut attributes = ExpressionAttributes::new(bar_value, Location::Memory);
        attributes.move_location = Some(Location::Value);
        harness.add_expression("bar_array[index]", attributes);

        let mut attributes =
            ExpressionAttributes::new(foo_value, Location::Storage { nonce: None });
        attributes.move_location = Some(Location::Memory);
        harness.add_expression("self.foo_map[bar_array[index]]", attributes);

        let result = map(&harness.context, &harness.src);

        assert_eq!(
            result,
            "scopym(div(map_value_ptr(0, mloadn(add($bar_array, mul($index, 32)), 32)), 32), 256)"
        );
    }

    #[rstest(
        expression,
        expected_yul,
        typ,
        case("block.coinbase", "coinbase()", Type::Base(Base::Address)),
        case("block.difficulty", "difficulty()", Type::Base(U256)),
        case("block.number", "number()", Type::Base(U256)),
        case("block.timestamp", "timestamp()", Type::Base(U256)),
        case("chain.id", "chainid()", Type::Base(U256)),
        case("msg.sender", "caller()", Type::Base(Base::Address)),
        case("msg.value", "callvalue()", Type::Base(U256)),
        case("tx.origin", "origin()", Type::Base(Base::Address)),
        case("tx.gas_price", "gasprice()", Type::Base(U256))
    )]
    fn builtin_attribute(expression: &str, expected_yul: &str, typ: Type) {
        let mut harness = ContextHarness::new(expression);
        harness.add_expression(expression, ExpressionAttributes::new(typ, Location::Value));
        let result = map(&harness.context, expression);
        assert_eq!(result, expected_yul);
    }

    #[rstest(
        expression,
        expected_yul,
        case("1 + 2", "checked_add_u256(1, 2)"),
        case("1 - 2", "checked_sub_unsigned(1, 2)"),
        case("1 * 2", "checked_mul_u256(1, 2)"),
        case("1 / 2", "checked_div_unsigned(1, 2)"),
        case("1 ** 2", "checked_exp_u256(1, 2)"),
        case("1 % 2", "checked_mod_unsigned(1, 2)"),
        case("1 & 2", "and(1, 2)"),
        case("1 | 2", "or(1, 2)"),
        case("1 ^ 2", "xor(1, 2)"),
        case("1 << 2", "shl(2, 1)"),
        case("1 >> 2", "shr(2, 1)")
    )]
    fn arithmetic_expression(expression: &str, expected_yul: &str) {
        let mut harness = ContextHarness::new(expression);
        harness.add_expressions(
            vec!["1", "2", expression],
            ExpressionAttributes::new(Type::Base(U256), Location::Value),
        );

        let result = map(&harness.context, expression);

        assert_eq!(result, expected_yul);
    }

    #[rstest(
        expression,
        expected_yul,
        case("1 == 2", "eq(1, 2)"),
        case("1 != 2", "iszero(eq(1, 2))"),
        case("1 < 2", "lt(1, 2)"),
        case("1 <= 2", "iszero(gt(1, 2))"),
        case("1 > 2", "gt(1, 2)"),
        case("1 >= 2", "iszero(lt(1, 2))")
    )]
    fn comparison_expression(expression: &str, expected_yul: &str) {
        let mut harness = ContextHarness::new(expression);
        harness.add_expressions(
            vec!["1", "2"],
            ExpressionAttributes::new(Type::Base(U256), Location::Value),
        );
        harness.add_expression(
            expression,
            ExpressionAttributes::new(Type::Base(Base::Bool), Location::Value),
        );

        let result = map(&harness.context, expression);

        assert_eq!(result, expected_yul);
    }
}
