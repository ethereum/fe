use crate::errors::CompileError;
use fe_parser::ast as fe;
use fe_parser::span::{
    Span,
    Spanned,
};
use fe_semantics::namespace::types::{
    Array,
    FixedSize,
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
        fe::Expr::Subscript { .. } => expr_subscript(context, exp),
        fe::Expr::Attribute { .. } => expr_attribute(context, exp),
        fe::Expr::Ternary { .. } => unimplemented!(),
        fe::Expr::BoolOperation { .. } => unimplemented!(),
        fe::Expr::BinOperation { .. } => expr_bin_operation(context, exp),
        fe::Expr::UnaryOperation { .. } => unimplemented!(),
        fe::Expr::CompOperation { .. } => expr_comp_operation(context, exp),
        fe::Expr::Call { .. } => unimplemented!(),
        fe::Expr::List { .. } => unimplemented!(),
        fe::Expr::ListComp { .. } => unimplemented!(),
        fe::Expr::Tuple { .. } => unimplemented!(),
        fe::Expr::Str(_) => unimplemented!(),
        fe::Expr::Ellipsis => unimplemented!(),
    }
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

/// Creates a new spanned expression. Useful in cases where an `Expr` is nested
/// within the node of a `Spanned` object.
pub fn spanned_expression<'a>(span: &Span, exp: &fe::Expr<'a>) -> Spanned<fe::Expr<'a>> {
    Spanned {
        node: (*exp).clone(),
        span: (*span).to_owned(),
    }
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

fn expr_subscript(
    context: &Context,
    exp: &Spanned<fe::Expr>,
) -> Result<yul::Expression, CompileError> {
    if let fe::Expr::Subscript { value, slices } = &exp.node {
        if let Some(value_attributes) = context.get_expression(value) {
            let value = expr(context, value)?;
            let index = slices_index(context, slices)?;

            return match (&value_attributes.typ, &value_attributes.location) {
                (Type::Map(map), Location::Storage { .. }) => {
                    keyed_storage_map(value, index, map.to_owned())
                }
                (Type::Array(_), Location::Storage { .. }) => unimplemented!(),
                (Type::Array(array), Location::Memory) => {
                    indexed_memory_array(value, index, array.to_owned())
                }
                (_, _) => unreachable!(),
            };
        }
    }

    unreachable!()
}

fn keyed_storage_map(
    map: yul::Expression,
    key: yul::Expression,
    map_type: Map,
) -> Result<yul::Expression, CompileError> {
    let sptr = expression! { dualkeccak256([map], [key]) };

    match map_type.value {
        FixedSize::Array(array) => Ok(array.scopy(sptr)),
        FixedSize::Base(base) => Ok(base.sload(sptr)),
    }
}

fn indexed_memory_array(
    array: yul::Expression,
    index: yul::Expression,
    array_type: Array,
) -> Result<yul::Expression, CompileError> {
    Ok(array_type.mload_elem(array, index))
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
        FixedSize,
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
                    key: FixedSize::Base(Base::Address),
                    value: FixedSize::Base(Base::U256),
                }),
                location: Location::Storage { index: 0 },
            },
        );

        let result = map(&harness.context, &harness.src);

        assert_eq!(result.to_string(), "sloadn(dualkeccak256(0, 3), 32)");
    }

    #[test]
    fn map_sload_w_array_elem() {
        let mut harness = ContextHarness::new("self.foo_map[bar_array[index]]");
        harness.add_expression(
            "self.foo_map",
            ExpressionAttributes {
                typ: Type::Map(Map {
                    key: FixedSize::Base(Base::Byte),
                    value: FixedSize::Array(Array {
                        dimension: 8,
                        inner: Base::Address,
                    }),
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
            result.to_string(),
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
