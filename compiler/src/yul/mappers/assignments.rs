use crate::errors::CompileError;
use crate::yul::mappers::expressions;
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
    Location,
};
use yultsur::*;

/// Builds a Yul statement from a Fe assignment.
pub fn assign(
    context: &Context,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::Assign { targets, value } = &stmt.node {
        if targets.len() > 1 {
            unimplemented!("multiple assignment targets")
        }

        if let Some(first_target) = targets.first() {
            return match &first_target.node {
                fe::Expr::Name(_) => assign_name(context, first_target, value),
                fe::Expr::Subscript { .. } => assign_subscript(context, first_target, value),
                _ => unreachable!(),
            };
        }
    }

    unreachable!()
}

fn assign_subscript(
    context: &Context,
    target: &Spanned<fe::Expr>,
    value: &Spanned<fe::Expr>,
) -> Result<yul::Statement, CompileError> {
    if let fe::Expr::Subscript {
        value: target,
        slices,
    } = &target.node
    {
        if let Some(target_attributes) = context.get_expression(target) {
            let target = expressions::expr(context, target)?;
            let index = expressions::slices_index(context, slices)?;
            let value = expressions::expr(context, value)?;

            return match target_attributes.to_tuple() {
                (Type::Map(map), _) => assign_map(map, target, index, value),
                (Type::Array(array), Location::Memory) => {
                    assign_mem_array(array, target, index, value)
                }
                (Type::Array(_), Location::Storage { .. }) => unimplemented!(),
                _ => unreachable!(),
            };
        }
    }

    unreachable!()
}

fn assign_map(
    map: Map,
    target: yul::Expression,
    index: yul::Expression,
    value: yul::Expression,
) -> Result<yul::Statement, CompileError> {
    let sptr = expression! { dualkeccak256([target], [index]) };

    match *map.value {
        Type::Array(array) => Ok(operations::mem_to_sto(array, sptr, value)),
        Type::Base(base) => Ok(operations::val_to_sto(base, sptr, value)),
        Type::Map(_) => unreachable!(),
        Type::Tuple(_) => unimplemented!(),
        Type::String(_) => unimplemented!(),
    }
}

fn assign_mem_array(
    array: Array,
    target: yul::Expression,
    index: yul::Expression,
    value: yul::Expression,
) -> Result<yul::Statement, CompileError> {
    let inner_size = literal_expression! { (array.inner.size()) };
    let mptr = expression! { add([target], (mul([index], [inner_size]))) };

    Ok(operations::val_to_mem(array.inner, mptr, value))
}

fn assign_name(
    context: &Context,
    target: &Spanned<fe::Expr>,
    value: &Spanned<fe::Expr>,
) -> Result<yul::Statement, CompileError> {
    if let fe::Expr::Name(name) = target.node {
        let target = identifier! {(name)};
        let value = expressions::expr(context, value)?;

        return Ok(statement! { [target] := [value] });
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::yul::mappers::assignments::assign;
    use fe_parser as parser;
    use fe_semantics::namespace::types::{
        Array,
        Base,
        Type,
    };
    use fe_semantics::test_utils::ContextHarness;
    use fe_semantics::{
        Context,
        ExpressionAttributes,
        Location,
    };
    use rstest::rstest;

    fn map(context: &Context, src: &str) -> String {
        let tokens = parser::get_parse_tokens(src).expect("couldn't parse assignment");
        let stmt = parser::parsers::assign_stmt(&tokens[..])
            .expect("couldn't build assignment AST")
            .1;

        let assign = assign(context, &stmt).expect("couldn't map assignment AST");
        assign.to_string()
    }

    #[test]
    fn assign_u256() {
        let mut harness = ContextHarness::new("foo = bar");
        harness.add_expression(
            "foo",
            ExpressionAttributes {
                typ: Type::Base(Base::U256),
                location: Location::Value,
            },
        );

        assert_eq!(map(&harness.context, &harness.src), "foo := bar")
    }

    #[rstest(
        assignment,
        expected_yul,
        case("foo = 1 + 2", "foo := add(1, 2)"),
        case("foo = 1 - 2", "foo := sub(1, 2)"),
        case("foo = 1 * 2", "foo := mul(1, 2)"),
        case("foo = 1 / 2", "foo := div(1, 2)"),
        case("foo = 1 ** 2", "foo := exp(1, 2)"),
        case("foo = 1 % 2", "foo := mod(1, 2)"),
        case("foo = 1 & 2", "foo := and(1, 2)"),
        case("foo = 1 | 2", "foo := or(1, 2)"),
        case("foo = 1 ^ 2", "foo := xor(1, 2)"),
        case("foo = 1 << 2", "foo := shl(2, 1)"),
        case("foo = 1 >> 2", "foo := shr(2, 1)")
    )]
    fn assign_arithmetic_expression(assignment: &str, expected_yul: &str) {
        assert_eq!(map(&Context::new(), assignment), expected_yul)
    }

    #[test]
    fn assign_subscript_u256() {
        let mut harness = ContextHarness::new("foo[4] = 2");
        harness.add_expression(
            "foo",
            ExpressionAttributes {
                typ: Type::Array(Array {
                    dimension: 10,
                    inner: Base::U256,
                }),
                location: Location::Memory,
            },
        );

        assert_eq!(
            map(&harness.context, &harness.src),
            "mstoren(add(foo, mul(4, 32)), 2, 32)"
        )
    }
}
