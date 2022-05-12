use crate::context::FnContext;
use crate::mappers::expressions;
use crate::operations::data as data_operations;
use crate::operations::structs as struct_operations;
use crate::types::AsEvmSized;
use fe_analyzer::context::Location;
use fe_analyzer::namespace::types::Type;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use yultsur::yul::FunctionCall;
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
        let typ = target_attributes.typ.as_evm_sized();
        let value_attributes = context.expression_attributes(value_node);

        return match (
            value_attributes.final_location(),
            target_attributes.final_location(),
        ) {
            (Location::Memory, Location::Storage { .. }) => {
                if let fe::Expr::Attribute { .. } = &target_node.kind {
                    if let Type::Struct(struct_) = &target_attributes.typ {
                        return struct_operations::copy_to_storage(
                            context.db, struct_, target, value,
                        );
                    }
                }

                data_operations::mcopys(typ, target, value)
            }
            (Location::Memory, Location::Value) => {
                let target = expr_as_ident(target);
                let value = data_operations::mload(typ, value);
                statement! { [target] := [value] }
            }
            (Location::Memory, Location::Memory) => {
                if let fe::Expr::Attribute { value: val, .. } = &target_node.kind {
                    if let Type::Struct(_) = context.expression_attributes(val).typ {
                        return statement! { mstoren([drop_deref(&target)], 32, [value]) };
                    }
                }

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
        panic!("expression is not an identifier {}", expr);
    }
}

fn drop_deref(expr: &yul::Expression) -> yul::Expression {
    if let yul::Expression::FunctionCall(FunctionCall {
        identifier,
        arguments,
    }) = expr
    {
        if identifier.identifier != "mload" {
            panic!("Expected mload(some_fn(..)");
        }
        arguments
            .first()
            .expect("expected mload(..) to contain an argument")
            .clone()
    } else {
        panic!("expression is not a function call {}", expr);
    }
}
