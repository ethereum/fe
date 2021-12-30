use crate::context::FnContext;
use crate::mappers::expressions;
use crate::operations::data as data_operations;
use fe_analyzer::context::Location;
use fe_analyzer::namespace::types::FixedSize;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use yultsur::*;

/// Builds a Yul statement from a Fe assignment.
pub fn assign(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Assign {
        target: target_node,
        value: value_node,
    } = &stmt.kind
    {
        let target = expressions::expr(context, target_node);
        let value = expressions::expr(context, value_node);

        let target_attributes = context.expression_attributes(target_node);
        let value_attributes = context.expression_attributes(value_node);

        let typ =
            FixedSize::try_from(target_attributes.typ.to_owned()).expect("invalid attributes");

        return match (
            value_attributes.final_location(),
            target_attributes.final_location(),
        ) {
            (Location::Memory, Location::Storage { .. }) => {
                data_operations::mcopys(typ, target, value)
            }
            (Location::Memory, Location::Value) => {
                let target = expr_as_ident(target);
                let value = data_operations::mload(typ, value);
                statement! { [target] := [value] }
            }
            (Location::Memory, Location::Memory) => {
                let target = expr_as_ident(target);
                statement! { [target] := [value] }
            }
            (Location::Storage { .. }, Location::Storage { .. }) => {
                data_operations::scopys(typ, target, value)
            }
            (Location::Storage { .. }, Location::Value) => {
                let target = expr_as_ident(target);
                let value = data_operations::sload(typ, value);
                statement! { [target] := [value] }
            }
            (Location::Storage { .. }, Location::Memory) => {
                unreachable!("raw sto to mem assign")
            }
            (Location::Value, Location::Memory) => data_operations::mstore(typ, target, value),
            (Location::Value, Location::Storage { .. }) => {
                data_operations::sstore(typ, target, value)
            }
            (Location::Value, Location::Value) => {
                let target = expr_as_ident(target);
                statement! { [target] := [value] }
            }
        };
    }

    unreachable!()
}

fn expr_as_ident(expr: yul::Expression) -> yul::Identifier {
    if let yul::Expression::Identifier(ident) = expr {
        ident
    } else {
        panic!("expression is not an identifier");
    }
}
