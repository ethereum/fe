use crate::context::{ContractContext, ModuleContext};
use crate::mappers::{functions, types};
use crate::names;
use crate::utils::ZeroSpanNode;
use fe_analyzer::namespace::items::{ContractFieldId, ContractId, EventId};
use fe_analyzer::namespace::types::{Array, FixedSize};
use fe_parser::ast;
use fe_parser::node::Node;

/// Lowers a contract definition.
pub fn contract_def(module: &mut ModuleContext, contract: ContractId) -> Node<ast::Contract> {
    let mut context = ContractContext::new(module);
    let fields = contract
        .all_fields(context.db())
        .iter()
        .map(|field| contract_field(&mut context, *field))
        .collect();

    let events = contract
        .all_events(context.db())
        .iter()
        .map(|event| ast::ContractStmt::Event(event_def(&mut context, *event)))
        .collect();
    let functions = contract
        .all_functions(context.db())
        .iter()
        .map(|function| ast::ContractStmt::Function(functions::func_def(&mut context, *function)))
        .collect();

    let func_defs_from_list_expr = context
        .list_expressions
        .iter()
        .map(|expr| ast::ContractStmt::Function(list_expr_to_fn_def(expr).into_node()))
        .collect::<Vec<ast::ContractStmt>>();

    let node = &contract.data(context.db()).ast;
    Node::new(
        ast::Contract {
            name: node.kind.name.clone(),
            fields,
            body: [events, functions, func_defs_from_list_expr].concat(),
        },
        node.span,
    )
}

fn contract_field(context: &mut ContractContext, field: ContractFieldId) -> Node<ast::Field> {
    let node = &field.data(context.db()).ast;
    let typ = field.typ(context.db()).expect("contract field type error");
    Node::new(
        ast::Field {
            is_pub: node.kind.is_pub,
            is_const: node.kind.is_const,
            name: node.kind.name.clone(),
            typ: types::type_desc(&mut context.module, node.kind.typ.clone(), &typ),
            value: node.kind.value.clone(),
        },
        node.span,
    )
}

fn event_def(context: &mut ContractContext, event: EventId) -> Node<ast::Event> {
    let ast_fields = &event.data(context.db()).ast.kind.fields;
    let fields = event
        .typ(context.db())
        .fields
        .iter()
        .zip(ast_fields.iter())
        .map(|(field, node)| {
            ast::EventField {
                is_idx: field.is_indexed,
                name: field.name.clone().into_node(),
                typ: types::type_desc(
                    context.module,
                    node.kind.typ.clone(),
                    &field
                        .typ
                        .as_ref()
                        .expect("event field type error")
                        .clone()
                        .into(),
                ),
            }
            .into_node()
        })
        .collect();

    let node = &event.data(context.db()).ast;
    Node::new(
        ast::Event {
            name: node.kind.name.clone(),
            fields,
        },
        node.span,
    )
}

fn list_expr_to_fn_def(array: &Array) -> ast::Function {
    // Built the AST nodes for the function arguments
    let args = (0..array.size)
        .map(|index| {
            ast::FunctionArg {
                name: format!("val{}", index).into_node(),
                typ: names::fixed_size_type_desc(&FixedSize::Base(array.inner)).into_node(),
            }
            .into_node()
        })
        .collect::<Vec<_>>();

    // Build the AST node for the array declaration
    let var_decl_name = "generated_array";
    let var_decl = ast::FuncStmt::VarDecl {
        target: ast::VarDeclTarget::Name(var_decl_name.to_string()).into_node(),
        typ: names::fixed_size_type_desc(&FixedSize::Array(array.clone())).into_node(),
        value: None,
    }
    .into_node();

    // Build the AST nodes for the individual assignments of array slots
    let assignments = (0..array.size)
        .map(|index| {
            ast::FuncStmt::Assign {
                target: ast::Expr::Subscript {
                    value: ast::Expr::Name(var_decl_name.to_string()).into_boxed_node(),
                    index: ast::Expr::Num(index.to_string()).into_boxed_node(),
                }
                .into_node(),
                value: ast::Expr::Name(format!("val{}", index)).into_node(),
            }
            .into_node()
        })
        .collect::<Vec<_>>();

    // Build the AST node for the return statement
    let return_stmt = ast::FuncStmt::Return {
        value: Some(ast::Expr::Name(var_decl_name.to_string()).into_node()),
    }
    .into_node();

    let return_type =
        Some(names::fixed_size_type_desc(&FixedSize::Array(array.clone())).into_node());

    // Put it all together in one AST node that holds the entire function definition
    ast::Function {
        is_pub: false,
        name: names::list_expr_generator_fn_name(array).into_node(),
        args,
        return_type,
        body: [vec![var_decl], assignments, vec![return_stmt]].concat(),
    }
}
