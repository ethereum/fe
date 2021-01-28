use crate::errors::CompileError;
use crate::yul::mappers::expressions;
use crate::yul::operations::data as data_operations;
use fe_analyzer::namespace::types::FixedSize;
use fe_analyzer::{
    Context,
    Location,
};
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::convert::TryFrom;
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
            if let (Some(target_attributes), Some(value_attributes)) = (
                context.get_expression(first_target),
                context.get_expression(value),
            ) {
                let target = expressions::expr(&context, first_target)?;
                let value = expressions::expr(&context, value)?;

                let typ = FixedSize::try_from(target_attributes.typ.to_owned())
                    .map_err(|_| CompileError::static_str("invalid attributes"))?;

                return Ok(
                    match (
                        value_attributes.final_location(),
                        target_attributes.final_location(),
                    ) {
                        (Location::Memory, Location::Storage { .. }) => {
                            data_operations::mcopys(typ, target, value)
                        }
                        (Location::Memory, Location::Value) => {
                            let target = expr_as_ident(target)?;
                            let value = data_operations::mload(typ, value);
                            statement! { [target] := [value] }
                        }
                        (Location::Memory, Location::Memory) => {
                            let target = expr_as_ident(target)?;
                            statement! { [target] := [value] }
                        }
                        (Location::Storage { .. }, Location::Storage { .. }) => {
                            data_operations::scopys(typ, target, value)
                        }
                        (Location::Storage { .. }, Location::Value) => {
                            let target = expr_as_ident(target)?;
                            let value = data_operations::sload(typ, value);
                            statement! { [target] := [value] }
                        }
                        (Location::Storage { .. }, Location::Memory) => {
                            unreachable!("raw sto to mem assign")
                        }
                        (Location::Value, Location::Memory) => {
                            data_operations::mstore(typ, target, value)
                        }
                        (Location::Value, Location::Storage { .. }) => {
                            data_operations::sstore(typ, target, value)
                        }
                        (Location::Value, Location::Value) => {
                            let target = expr_as_ident(target)?;
                            statement! { [target] := [value] }
                        }
                    },
                );
            }
        }
    }

    unreachable!()
}

fn expr_as_ident(expr: yul::Expression) -> Result<yul::Identifier, CompileError> {
    if let yul::Expression::Identifier(ident) = expr {
        return Ok(ident);
    }

    Err(CompileError::static_str("expression is not an identifier"))
}

#[cfg(test)]
mod tests {
    use crate::yul::mappers::assignments::assign;
    use fe_analyzer::namespace::types::{
        Array,
        Type,
        U256,
    };
    use fe_analyzer::test_utils::ContextHarness;
    use fe_analyzer::{
        Context,
        ExpressionAttributes,
        Location,
    };
    use fe_parser as parser;
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
        harness.add_expressions(
            vec!["foo", "bar"],
            ExpressionAttributes::new(Type::Base(U256), Location::Value),
        );

        assert_eq!(map(&harness.context, &harness.src), "$foo := $bar")
    }

    #[rstest(
        assignment,
        expected_yul,
        case("foo = 1 + 2", "$foo := add(1, 2)"),
        case("foo = 1 - 2", "$foo := sub(1, 2)"),
        case("foo = 1 * 2", "$foo := mul(1, 2)"),
        case("foo = 1 / 2", "$foo := div(1, 2)"),
        case("foo = 1 ** 2", "$foo := exp(1, 2)"),
        case("foo = 1 % 2", "$foo := mod(1, 2)"),
        case("foo = 1 & 2", "$foo := and(1, 2)"),
        case("foo = 1 | 2", "$foo := or(1, 2)"),
        case("foo = 1 ^ 2", "$foo := xor(1, 2)"),
        case("foo = 1 << 2", "$foo := shl(2, 1)"),
        case("foo = 1 >> 2", "$foo := shr(2, 1)")
    )]
    fn assign_arithmetic_expression(assignment: &str, expected_yul: &str) {
        let mut harness = ContextHarness::new(assignment);
        harness.add_expressions(
            vec!["foo", "1", "2", &assignment[6..]],
            ExpressionAttributes::new(Type::Base(U256), Location::Value),
        );

        assert_eq!(map(&harness.context, assignment), expected_yul)
    }

    #[test]
    fn assign_subscript_u256() {
        let mut harness = ContextHarness::new("foo[4] = 2");

        harness.add_expressions(
            vec!["2", "4"],
            ExpressionAttributes::new(Type::Base(U256), Location::Value),
        );

        harness.add_expression(
            "foo[4]",
            ExpressionAttributes::new(Type::Base(U256), Location::Memory),
        );

        harness.add_expression(
            "foo",
            ExpressionAttributes::new(
                Type::Array(Array {
                    dimension: 10,
                    inner: U256,
                }),
                Location::Memory,
            ),
        );

        assert_eq!(
            map(&harness.context, &harness.src),
            "mstoren(add($foo, mul(4, 32)), 2, 32)"
        )
    }
}
