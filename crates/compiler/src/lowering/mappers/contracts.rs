use crate::lowering::context::Context;
use crate::lowering::mappers::{functions, types};
use crate::lowering::names;
use crate::lowering::utils::ZeroSpanNode;
use fe_analyzer::namespace::types::{Array, FixedSize};
use fe_parser::ast as fe;
use fe_parser::node::Node;

/// Lowers a contract definition.
pub fn contract_def(context: &mut Context, stmt: Node<fe::Contract>) -> Node<fe::Contract> {
    let fe::Contract { name, fields, body } = stmt.kind;

    let lowered_fields = fields
        .into_iter()
        .map(|field| contract_field(context, field))
        .collect();

    let lowered_body = body
        .into_iter()
        .map(|stmt| match stmt {
            fe::ContractStmt::Event(def) => fe::ContractStmt::Event(event_def(context, def)),
            fe::ContractStmt::Function(def) => {
                fe::ContractStmt::Function(functions::func_def(context, def))
            }
        })
        .collect();

    let func_defs_from_list_expr = context
        .list_expressions
        .iter()
        .map(|expr| fe::ContractStmt::Function(list_expr_to_fn_def(expr).into_node()))
        .collect::<Vec<fe::ContractStmt>>();

    Node::new(
        fe::Contract {
            name,
            fields: lowered_fields,
            body: [lowered_body, func_defs_from_list_expr].concat(),
        },
        stmt.span,
    )
}

fn contract_field(context: &mut Context, field: Node<fe::Field>) -> Node<fe::Field> {
    Node::new(
        fe::Field {
            is_pub: field.kind.is_pub,
            is_const: field.kind.is_const,
            name: field.kind.name,
            typ: types::type_desc(context, field.kind.typ),
            value: field.kind.value,
        },
        field.span,
    )
}

fn event_def(context: &mut Context, stmt: Node<fe::Event>) -> Node<fe::Event> {
    let fe::Event { name, fields } = stmt.kind;
    let lowered_fields = fields
        .into_iter()
        .map(|field| {
            Node::new(
                fe::EventField {
                    is_idx: field.kind.is_idx,
                    name: field.kind.name,
                    typ: types::type_desc(context, field.kind.typ),
                },
                field.span,
            )
        })
        .collect();

    Node::new(
        fe::Event {
            name,
            fields: lowered_fields,
        },
        stmt.span,
    )
}

fn list_expr_to_fn_def(array: &Array) -> fe::Function {
    // Built the AST nodes for the function arguments
    let args = (0..array.size)
        .map(|index| {
            fe::FunctionArg {
                name: format!("val{}", index).into_node(),
                typ: names::fixed_size_type_desc(&FixedSize::Base(array.inner)).into_node(),
            }
            .into_node()
        })
        .collect::<Vec<_>>();

    // Build the AST node for the array declaration
    let var_decl_name = "generated_array";
    let var_decl = fe::FuncStmt::VarDecl {
        target: fe::VarDeclTarget::Name(var_decl_name.to_string()).into_node(),
        typ: names::fixed_size_type_desc(&FixedSize::Array(array.clone())).into_node(),
        value: None,
    }
    .into_node();

    // Build the AST nodes for the individual assignments of array slots
    let assignments = (0..array.size)
        .map(|index| {
            fe::FuncStmt::Assign {
                target: fe::Expr::Subscript {
                    value: fe::Expr::Name(var_decl_name.to_string()).into_boxed_node(),
                    index: fe::Expr::Num(index.to_string()).into_boxed_node(),
                }
                .into_node(),
                value: fe::Expr::Name(format!("val{}", index)).into_node(),
            }
            .into_node()
        })
        .collect::<Vec<_>>();

    // Build the AST node for the return statement
    let return_stmt = fe::FuncStmt::Return {
        value: Some(fe::Expr::Name(var_decl_name.to_string()).into_node()),
    }
    .into_node();

    let return_type =
        Some(names::fixed_size_type_desc(&FixedSize::Array(array.clone())).into_node());

    // Put it all together in one AST node that holds the entire function definition
    fe::Function {
        is_pub: false,
        name: names::list_expr_generator_fn_name(array).into_node(),
        args,
        return_type,
        body: [vec![var_decl], assignments, vec![return_stmt]].concat(),
    }
}
