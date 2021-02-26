use crate::errors::CompileError;
use crate::yul::names;
use crate::yul::operations::{
    abi as abi_operations,
    contracts as contract_operations,
    data as data_operations,
    structs as struct_operations,
};
use crate::yul::utils;
use builtins::{
    BlockField,
    ChainField,
    MsgField,
    Object,
    TxField,
};
use fe_analyzer::builtins;
use fe_analyzer::builtins::{
    ContractTypeMethod,
    GlobalMethod,
};
use fe_analyzer::namespace::types::{
    Base,
    FeSized,
    FixedSize,
    Type,
};
use fe_analyzer::{
    CallType,
    Context,
    Location,
};
use fe_common::utils::keccak;
use fe_parser::ast as fe;
use fe_parser::ast::BoolOperator;
use fe_parser::span::Spanned;
use std::convert::TryFrom;
use std::str::FromStr;
use yultsur::*;

/// Builds a Yul expression from a Fe expression.
pub fn expr(context: &Context, exp: &Spanned<fe::Expr>) -> Result<yul::Expression, CompileError> {
    if let Some(attributes) = context.get_expression(exp) {
        let expression = match &exp.node {
            fe::Expr::Name(_) => Ok(expr_name(exp)),
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
            fe::Expr::Tuple { .. } => unimplemented!(),
            fe::Expr::Str(_) => expr_str(exp),
            fe::Expr::Ellipsis => unimplemented!(),
        }?;

        match (
            attributes.location.to_owned(),
            attributes.move_location.to_owned(),
        ) {
            (from, Some(to)) => move_expression(expression, attributes.typ.to_owned(), from, to),
            (_, None) => Ok(expression),
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
) -> Result<yul::Expression, CompileError> {
    let typ = FixedSize::try_from(typ).expect("Invalid type");

    match (from.clone(), to.clone()) {
        (Location::Storage { .. }, Location::Value) => Ok(data_operations::sload(typ, val)),
        (Location::Memory, Location::Value) => Ok(data_operations::mload(typ, val)),
        (Location::Memory, Location::Memory) => Ok(data_operations::mcopym(typ, val)),
        (Location::Storage { .. }, Location::Memory) => Ok(data_operations::scopym(typ, val)),
        _ => Err(CompileError::str(&format!(
            "invalid expression move: {:?} {:?}",
            from, to
        ))),
    }
}

pub fn call_arg(
    context: &Context,
    arg: &Spanned<fe::CallArg>,
) -> Result<yul::Expression, CompileError> {
    match &arg.node {
        fe::CallArg::Arg(value) => {
            let spanned = utils::spanned_expression(&arg.span, value);
            expr(context, &spanned)
        }
        fe::CallArg::Kwarg(fe::Kwarg { name: _, value }) => expr(context, value),
    }
}

fn expr_call(context: &Context, exp: &Spanned<fe::Expr>) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Call { args, func } = &exp.node {
        if let Some(call_type) = context.get_call(func) {
            let yul_args: Vec<yul::Expression> = args
                .node
                .iter()
                .map(|val| call_arg(context, val))
                .collect::<Result<_, _>>()?;

            return match call_type {
                CallType::BuiltinFunction { func } => match func {
                    GlobalMethod::Keccak256 => {
                        let first_arg = args.node.first().expect("Missing argument");
                        let arg_expr = context
                            .get_expression(first_arg)
                            .expect("invalid attributes");
                        let size = FixedSize::try_from(arg_expr.typ.clone()).expect("Invalid type");
                        let func_name: &str = func.into();

                        let func_name = identifier! { (func_name) };
                        let size = identifier_expression! { (size.size()) };
                        Ok(expression! { [func_name]([yul_args[0].to_owned()], [size]) })
                    }
                },
                CallType::TypeConstructor {
                    typ: Type::Struct(val),
                } => Ok(struct_operations::new(val, yul_args)),
                CallType::TypeConstructor { .. } => Ok(yul_args[0].to_owned()),
                CallType::SelfAttribute { func_name } => {
                    let func_name = names::func_name(func_name);
                    Ok(expression! { [func_name]([yul_args...]) })
                }
                CallType::ValueAttribute => {
                    if let fe::Expr::Attribute { value, attr } = &func.node {
                        let value_attributes =
                            context.get_expression(value).expect("invalid attributes");

                        return match (value_attributes.typ.to_owned(), attr.node) {
                            (Type::Contract(contract), func_name) => Ok(contract_operations::call(
                                contract,
                                func_name,
                                expr(context, value)?,
                                yul_args,
                            )),
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
                                        Type::Struct(struct_) => Ok(abi_operations::encode(
                                            vec![struct_],
                                            vec![expr(context, value)?],
                                        )),
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
                            Ok(contract_operations::create2(
                                &contract,
                                yul_args[0].to_owned(),
                                yul_args[1].to_owned(),
                            ))
                        }
                        (Type::Contract(contract), ContractTypeMethod::Create) => Ok(
                            contract_operations::create(&contract, yul_args[0].to_owned()),
                        ),
                        _ => panic!("invalid attributes"),
                    }
                }
            };
        }
    }

    unreachable!()
}

pub fn expr_comp_operation(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::CompOperation { left, op, right } = &exp.node {
        let yul_left = expr(context, left)?;
        let yul_right = expr(context, right)?;

        let typ = &context
            .get_expression(left)
            .expect("Missing `left` expression in context")
            .typ;

        return match op.node {
            fe::CompOperator::Eq => Ok(expression! { eq([yul_left], [yul_right]) }),
            fe::CompOperator::NotEq => Ok(expression! { iszero((eq([yul_left], [yul_right]))) }),
            fe::CompOperator::Lt => match typ.is_signed_integer() {
                true => Ok(expression! { slt([yul_left], [yul_right]) }),
                false => Ok(expression! { lt([yul_left], [yul_right]) }),
            },
            fe::CompOperator::LtE => match typ.is_signed_integer() {
                true => Ok(expression! { iszero((sgt([yul_left], [yul_right]))) }),
                false => Ok(expression! { iszero((gt([yul_left], [yul_right]))) }),
            },
            fe::CompOperator::Gt => match typ.is_signed_integer() {
                true => Ok(expression! { sgt([yul_left], [yul_right]) }),
                false => Ok(expression! { gt([yul_left], [yul_right]) }),
            },
            fe::CompOperator::GtE => match typ.is_signed_integer() {
                true => Ok(expression! { iszero((slt([yul_left], [yul_right]))) }),
                false => Ok(expression! { iszero((lt([yul_left], [yul_right]))) }),
            },
            _ => unimplemented!(),
        };
    }

    unreachable!()
}

pub fn expr_bin_operation(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::BinOperation { left, op, right } = &exp.node {
        let yul_left = expr(context, left)?;
        let yul_right = expr(context, right)?;

        let typ = &context
            .get_expression(left)
            .expect("Missing `left` expression in context")
            .typ;

        return match op.node {
            fe::BinOperator::Add => match typ {
                Type::Base(Base::Numeric(integer)) => {
                    Ok(expression! { [names::checked_add(integer)]([yul_left], [yul_right]) })
                }
                _ => unimplemented!("Addition for non-numeric types not yet supported"),
            },
            fe::BinOperator::Sub => match typ {
                Type::Base(Base::Numeric(integer)) => {
                    Ok(expression! { [names::checked_sub(integer)]([yul_left], [yul_right]) })
                }
                _ => unimplemented!("Subtraction for non-numeric types not yet supported"),
            },
            fe::BinOperator::Mult => Ok(expression! { mul([yul_left], [yul_right]) }),
            fe::BinOperator::Div => match typ.is_signed_integer() {
                true => Ok(expression! { sdiv([yul_left], [yul_right]) }),
                false => Ok(expression! { div([yul_left], [yul_right]) }),
            },
            fe::BinOperator::BitAnd => Ok(expression! { and([yul_left], [yul_right]) }),
            fe::BinOperator::BitOr => Ok(expression! { or([yul_left], [yul_right]) }),
            fe::BinOperator::BitXor => Ok(expression! { xor([yul_left], [yul_right]) }),
            fe::BinOperator::LShift => Ok(expression! { shl([yul_right], [yul_left]) }),
            fe::BinOperator::RShift => match typ.is_signed_integer() {
                true => Ok(expression! { sar([yul_right], [yul_left]) }),
                false => Ok(expression! { shr([yul_right], [yul_left]) }),
            },
            fe::BinOperator::Mod => match typ.is_signed_integer() {
                true => Ok(expression! { smod([yul_left], [yul_right]) }),
                false => Ok(expression! { mod([yul_left], [yul_right]) }),
            },
            fe::BinOperator::Pow => Ok(expression! { exp([yul_left], [yul_right]) }),
            _ => unimplemented!(),
        };
    }

    unreachable!()
}

pub fn expr_unary_operation(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::UnaryOperation { op, operand } = &exp.node {
        let yul_operand = expr(context, operand)?;

        return match &op.node {
            fe::UnaryOperator::USub => {
                let zero = literal_expression! {0};
                Ok(expression! { sub([zero], [yul_operand]) })
            }
            fe::UnaryOperator::Not => Ok(expression! { iszero([yul_operand]) }),
            _ => todo!(),
        };
    }

    unreachable!()
}

/// Retrieves the &str value of a name expression.
pub fn expr_name_str<'a>(exp: &Spanned<fe::Expr<'a>>) -> &'a str {
    if let fe::Expr::Name(name) = exp.node {
        return name;
    }

    unreachable!()
}

/// Builds a Yul expression from the first slice, if it is an index.
pub fn slices_index(
    context: &Context,
    slices: &Spanned<Vec<Spanned<fe::Slice>>>,
) -> Result<yul::Expression, CompileError> {
    if let Some(first_slice) = slices.node.first() {
        return slice_index(context, first_slice);
    }

    unreachable!()
}

pub fn slice_index(
    context: &Context,
    slice: &Spanned<fe::Slice>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Slice::Index(index) = &slice.node {
        let spanned = utils::spanned_expression(&slice.span, index.as_ref());
        return expr(context, &spanned);
    }

    unreachable!()
}

fn expr_name(exp: &Spanned<fe::Expr>) -> yul::Expression {
    let name = expr_name_str(exp);

    identifier_expression! { [names::var_name(name)] }
}

fn expr_num(exp: &Spanned<fe::Expr>) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Num(num) = &exp.node {
        return Ok(literal_expression! {(num)});
    }

    unreachable!()
}

fn expr_bool(exp: &Spanned<fe::Expr>) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Bool(val) = &exp.node {
        return Ok(literal_expression! {(val)});
    }

    unreachable!()
}

fn expr_str(exp: &Spanned<fe::Expr>) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Str(lines) = &exp.node {
        let content = lines.join("");
        let string_identifier = format!(r#""{}""#, keccak::full(content.as_bytes()));

        let offset = expression! { dataoffset([literal_expression! { (string_identifier) }]) };
        let size = expression! { datasize([literal_expression! { (string_identifier) }]) };

        return Ok(expression! {load_data_string([offset], [size])});
    }

    unreachable!()
}

fn expr_subscript(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Subscript { value, slices } = &exp.node {
        if let Some(value_attributes) = context.get_expression(value) {
            let value = expr(context, value)?;
            let index = slices_index(context, slices)?;

            return match value_attributes.typ.to_owned() {
                Type::Map(_) => Ok(data_operations::keyed_map(value, index)),
                Type::Array(array) => Ok(data_operations::indexed_array(array, value, index)),
                _ => Err(CompileError::static_str("invalid attributes")),
            };
        }

        return Err(CompileError::static_str("missing attributes"));
    }

    unreachable!()
}

fn expr_attribute(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Attribute { value, attr } = &exp.node {
        // If the given value has expression attributes, we handle it as an expression
        // by first mapping the value and then performing the expected operation
        // inferred from the attribute name.
        //
        // If the given value does not have expression attributes, we assume it is a
        // builtin object and map it as such.
        return if let Some(attributes) = context.get_expression(value) {
            let value = expr(context, value)?;

            match &attributes.typ {
                Type::Struct(struct_) => {
                    Ok(struct_operations::get_attribute(struct_, attr.node, value))
                }
                _ => panic!("invalid attributes"),
            }
        } else {
            match Object::from_str(expr_name_str(value)) {
                Ok(Object::Self_) => expr_attribute_self(context, exp),
                Ok(Object::Block) => match BlockField::from_str(attr.node) {
                    Ok(BlockField::Coinbase) => Ok(expression! { coinbase() }),
                    Ok(BlockField::Difficulty) => Ok(expression! { difficulty() }),
                    Ok(BlockField::Number) => Ok(expression! { number() }),
                    Ok(BlockField::Timestamp) => Ok(expression! { timestamp() }),
                    Err(_) => Err(CompileError::static_str("invalid `block` attribute name")),
                },
                Ok(Object::Chain) => match ChainField::from_str(attr.node) {
                    Ok(ChainField::Id) => Ok(expression! { chainid() }),
                    Err(_) => Err(CompileError::static_str("invalid `chain` attribute name")),
                },
                Ok(Object::Msg) => match MsgField::from_str(attr.node) {
                    Ok(MsgField::Data) => todo!(),
                    Ok(MsgField::Sender) => Ok(expression! { caller() }),
                    Ok(MsgField::Sig) => todo!(),
                    Ok(MsgField::Value) => Ok(expression! { callvalue() }),
                    Err(_) => Err(CompileError::static_str("invalid `msg` attribute name")),
                },
                Ok(Object::Tx) => match TxField::from_str(attr.node) {
                    Ok(TxField::GasPrice) => Ok(expression! { gasprice() }),
                    Ok(TxField::Origin) => Ok(expression! { origin() }),
                    Err(_) => Err(CompileError::static_str("invalid `msg` attribute name")),
                },
                Err(_) => Err(CompileError::static_str("invalid attributes")),
            }
        };
    }

    unreachable!()
}

fn expr_attribute_self(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Attribute { attr, .. } = &exp.node {
        if let Ok(builtins::SelfField::Address) = builtins::SelfField::from_str(attr.node) {
            return Ok(expression! { address() });
        }
    }

    if let Some(attributes) = context.get_expression(exp) {
        let nonce = if let Location::Storage { nonce: Some(nonce) } = attributes.location {
            nonce
        } else {
            return Err(CompileError::static_str("invalid attributes"));
        };

        return match attributes.typ {
            Type::Map(_) => Ok(literal_expression! { (nonce) }),
            _ => Ok(nonce_to_ptr(nonce)),
        };
    }

    Err(CompileError::static_str("missing attributes"))
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

fn expr_ternary(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Ternary {
        if_expr,
        test,
        else_expr,
    } = &exp.node
    {
        let yul_test_expr = expr(context, test)?;
        let yul_if_expr = expr(context, if_expr)?;
        let yul_else_expr = expr(context, else_expr)?;

        return Ok(expression! {ternary([yul_test_expr], [yul_if_expr], [yul_else_expr])});
    }
    unreachable!()
}

fn expr_bool_operation(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::BoolOperation { left, op, right } = &exp.node {
        let yul_left = expr(context, left)?;
        let yul_right = expr(context, right)?;

        return match op.node {
            BoolOperator::And => Ok(expression! {and([yul_left], [yul_right])}),
            BoolOperator::Or => Ok(expression! {or([yul_left], [yul_right])}),
        };
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::yul::mappers::expressions::{
        expr,
        Location,
    };
    use fe_analyzer::namespace::types::{
        Array,
        Base,
        Map,
        Type,
        U256,
    };
    use fe_analyzer::test_utils::ContextHarness;
    use fe_analyzer::{
        Context,
        ExpressionAttributes,
    };
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
        case("1 * 2", "mul(1, 2)"),
        case("1 / 2", "div(1, 2)"),
        case("1 ** 2", "exp(1, 2)"),
        case("1 % 2", "mod(1, 2)"),
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
