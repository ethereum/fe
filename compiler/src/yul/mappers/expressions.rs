use crate::errors::CompileError;
use crate::yul::mappers::_utils::spanned_expression;
use crate::yul::operations;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use fe_semantics::namespace::types::{
    Array,
    FeSized,
    Map,
    Type,
};
use fe_semantics::{
    Context,
    ExpressionAttributes,
    Location,
};
use yultsur::*;

/// Builds a Yul expression from a Fe expression.
pub fn expr(context: &Context, exp: &Spanned<fe::Expr>) -> Result<yul::Expression, CompileError> {
    match &exp.node {
        fe::Expr::Name(_) => expr_name(context, exp),
        fe::Expr::Num(_) => expr_num(exp),
        fe::Expr::Bool(_) => expr_bool(exp),
        fe::Expr::Subscript { .. } => expr_subscript(context, exp),
        fe::Expr::Attribute { .. } => expr_attribute(context, exp),
        fe::Expr::Ternary { .. } => expr_ternary(context, exp),
        fe::Expr::BoolOperation { .. } => unimplemented!(),
        fe::Expr::BinOperation { .. } => expr_bin_operation(context, exp),
        fe::Expr::UnaryOperation { .. } => unimplemented!(),
        fe::Expr::CompOperation { .. } => expr_comp_operation(context, exp),
        fe::Expr::Call { .. } => expr_call(context, exp),
        fe::Expr::List { .. } => unimplemented!(),
        fe::Expr::ListComp { .. } => unimplemented!(),
        fe::Expr::Tuple { .. } => unimplemented!(),
        fe::Expr::Str(_) => unimplemented!(),
        fe::Expr::Ellipsis => unimplemented!(),
    }
}

pub fn call_arg(
    context: &Context,
    arg: &Spanned<fe::CallArg>,
) -> Result<yul::Expression, CompileError> {
    match &arg.node {
        fe::CallArg::Arg(value) => {
            let spanned = spanned_expression(&arg.span, value);
            expr(context, &spanned)
        }
        fe::CallArg::Kwarg(fe::Kwarg { name: _, value }) => expr(context, value),
    }
}

pub fn expr_call(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Call { args, func } = &exp.node {
        if let fe::Expr::Attribute { value: _, attr } = &func.node {
            let arguments = &args.node;
            let yul_args: Vec<yul::Expression> = arguments
                .iter()
                .map(|val| call_arg(context, val))
                .collect::<Result<_, _>>()?;

            let func_name = identifier! { (attr.node) };

            return Ok(expression! { [func_name]([yul_args...]) });
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

        return match op.node {
            fe::CompOperator::Eq => Ok(expression! { eq([yul_left], [yul_right]) }),
            fe::CompOperator::NotEq => {
                Ok(expression! { iszero([expression! { eq([yul_left], [yul_right]) }]) })
            }
            fe::CompOperator::Lt => Ok(expression! { lt([yul_left], [yul_right]) }),
            fe::CompOperator::LtE => {
                Ok(expression! { iszero([expression! {gt([yul_left], [yul_right])}]) })
            }
            fe::CompOperator::Gt => Ok(expression! { gt([yul_left], [yul_right]) }),
            fe::CompOperator::GtE => {
                Ok(expression! { iszero([expression! {lt([yul_left], [yul_right])}]) })
            }
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

        return match op.node {
            fe::BinOperator::Add => Ok(expression! { add([yul_left], [yul_right]) }),
            fe::BinOperator::Sub => Ok(expression! { sub([yul_left], [yul_right]) }),
            fe::BinOperator::Mult => Ok(expression! { mul([yul_left], [yul_right]) }),
            fe::BinOperator::Div => Ok(expression! { div([yul_left], [yul_right]) }),
            fe::BinOperator::BitAnd => Ok(expression! { and([yul_left], [yul_right]) }),
            fe::BinOperator::BitOr => Ok(expression! { or([yul_left], [yul_right]) }),
            fe::BinOperator::BitXor => Ok(expression! { xor([yul_left], [yul_right]) }),
            fe::BinOperator::LShift => Ok(expression! { shl([yul_right], [yul_left]) }),
            fe::BinOperator::RShift => Ok(expression! { shr([yul_right], [yul_left]) }),
            fe::BinOperator::Mod => Ok(expression! { mod([yul_left], [yul_right]) }),
            fe::BinOperator::Pow => Ok(expression! { exp([yul_left], [yul_right]) }),
            _ => unimplemented!(),
        };
    }

    unreachable!()
}

/// Retrieves the &str value of a name expression.
pub fn expr_name_str<'a>(exp: &Spanned<fe::Expr<'a>>) -> Result<&'a str, CompileError> {
    if let fe::Expr::Name(name) = exp.node {
        return Ok(name);
    }

    unreachable!()
}

/// Retrieves the &str value of a name expression and converts it to a String.
pub fn expr_name_string(exp: &Spanned<fe::Expr>) -> Result<String, CompileError> {
    expr_name_str(exp).map(|name| name.to_string())
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
        let spanned = spanned_expression(&slice.span, index.as_ref());
        return expr(context, &spanned);
    }

    unreachable!()
}

fn expr_name(_context: &Context, exp: &Spanned<fe::Expr>) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Name(name) = exp.node {
        return Ok(identifier_expression! {(name)});
    }

    unreachable!()
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

fn expr_subscript(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Subscript { value, slices } = &exp.node {
        if let Some(value_attributes) = context.get_expression(value) {
            let value = expr(context, value)?;
            let index = slices_index(context, slices)?;

            return match value_attributes.to_tuple() {
                (Type::Map(map), Location::Storage { .. }) => {
                    Ok(keyed_storage_map(map, value, index))
                }
                (Type::Array(_), Location::Storage { .. }) => unimplemented!(),
                (Type::Array(array), Location::Memory) => {
                    Ok(indexed_memory_array(array, value, index))
                }
                (_, _) => unreachable!(),
            };
        }
    }

    unreachable!()
}

fn keyed_storage_map(typ: Map, map: yul::Expression, key: yul::Expression) -> yul::Expression {
    let sptr = expression! { dualkeccak256([map], [key]) };

    match *typ.value {
        Type::Array(array) => operations::sto_to_mem(array, sptr),
        Type::Base(base) => operations::sto_to_val(base, sptr),
        Type::Map(_) => sptr,
        Type::Tuple(_) => unimplemented!(),
        Type::String(_) => unimplemented!(),
    }
}

fn indexed_memory_array(
    typ: Array,
    array: yul::Expression,
    index: yul::Expression,
) -> yul::Expression {
    let inner_size = literal_expression! { (typ.inner.size()) };
    let mptr = expression! { add([array], (mul([index], [inner_size]))) };

    operations::mem_to_val(typ.inner, mptr)
}

fn expr_attribute(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Attribute { value, attr } = &exp.node {
        return match expr_name_str(value)? {
            "msg" => expr_attribute_msg(attr),
            "self" => expr_attribute_self(context, exp),
            _ => Err(CompileError::static_str("invalid attribute value")),
        };
    }

    unreachable!()
}

fn expr_attribute_msg(attr: &Spanned<&str>) -> Result<yul::Expression, CompileError> {
    match attr.node {
        "sender" => Ok(expression! { caller() }),
        _ => Err(CompileError::static_str("invalid msg attribute name")),
    }
}

fn expr_attribute_self(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    match context.get_expression(exp) {
        Some(ExpressionAttributes {
            typ: Type::Map(_),
            location: Location::Storage { index },
        }) => Ok(literal_expression! { (index) }),
        _ => unimplemented!(),
    }
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

#[cfg(test)]
mod tests {
    use crate::yul::mappers::expressions::{
        expr,
        Location,
    };
    use fe_parser as parser;
    use fe_semantics::namespace::types::{
        Array,
        Base,
        Map,
        Type,
    };
    use fe_semantics::test_utils::ContextHarness;
    use fe_semantics::{
        Context,
        ExpressionAttributes,
    };
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
            "self.foo",
            ExpressionAttributes {
                typ: Type::Map(Map {
                    key: Base::Address,
                    value: Box::new(Type::Base(Base::U256)),
                }),
                location: Location::Storage { index: 0 },
            },
        );

        let result = map(&harness.context, &harness.src);

        assert_eq!(result, "sloadn(dualkeccak256(0, 3), 32)");
    }

    #[test]
    fn map_sload_w_array_elem() {
        let mut harness = ContextHarness::new("self.foo_map[bar_array[index]]");
        harness.add_expression(
            "self.foo_map",
            ExpressionAttributes {
                typ: Type::Map(Map {
                    key: Base::Byte,
                    value: Box::new(Type::Array(Array {
                        dimension: 8,
                        inner: Base::Address,
                    })),
                }),
                location: Location::Storage { index: 0 },
            },
        );

        harness.add_expression(
            "bar_array",
            ExpressionAttributes {
                typ: Type::Array(Array {
                    dimension: 100,
                    inner: Base::Byte,
                }),
                location: Location::Memory,
            },
        );

        let result = map(&harness.context, &harness.src);

        assert_eq!(
            result,
            "scopy(dualkeccak256(0, mloadn(add(bar_array, mul(index, 1)), 1)), 160)"
        );
    }

    #[test]
    fn msg_sender() {
        let result = map(&Context::new(), "msg.sender");

        assert_eq!(result, "caller()");
    }

    #[rstest(
        expression,
        expected_yul,
        case("1 + 2 ", "add(1, 2)"),
        case("1 - 2", "sub(1, 2)"),
        case("1 * 2", "mul(1, 2)"),
        case("1 / 2", "div(1, 2)"),
        case("1 ** 2", "exp(1, 2)"),
        case("5 % 2", "mod(5, 2)"),
        case("1 & 2", "and(1, 2)"),
        case("1 | 2", "or(1, 2)"),
        case("1 ^ 2", "xor(1, 2)"),
        case("1 << 2", "shl(2, 1)"),
        case("1 >> 2", "shr(2, 1)")
    )]
    fn arithmetic_expression(expression: &str, expected_yul: &str) {
        let result = map(&Context::new(), expression);

        assert_eq!(result, expected_yul);
    }

    #[rstest(
        expression,
        expected_yul,
        case("1 == 2 ", "eq(1, 2)"),
        case("1 != 2 ", "iszero(eq(1, 2))"),
        case("1 < 2 ", "lt(1, 2)"),
        case("1 <= 2 ", "iszero(gt(1, 2))"),
        case("1 > 2 ", "gt(1, 2)"),
        case("1 >= 2 ", "iszero(lt(1, 2))")
    )]
    fn comparision_expression(expression: &str, expected_yul: &str) {
        let result = map(&Context::new(), expression);

        assert_eq!(result, expected_yul);
    }
}
