use crate::context::ModuleContext;
use crate::mappers::{events, functions, types};
use fe_analyzer::namespace::items::{ContractFieldId, ContractId};
use fe_parser::ast;
use fe_parser::node::Node;

/// Lowers a contract definition.
pub fn contract_def(context: &mut ModuleContext, contract: ContractId) -> Node<ast::Contract> {
    let db = context.db;
    let fields = contract
        .fields(db)
        .values()
        .map(|field| contract_field(context, *field))
        .collect();

    let events = contract
        .events(db)
        .values()
        .map(|event| ast::ContractStmt::Event(events::event_def(context, *event)))
        .collect::<Vec<_>>();

    let mut functions = contract
        .functions(db)
        .values()
        .map(|function| ast::ContractStmt::Function(functions::func_def(context, *function)))
        .collect::<Vec<_>>();

    if let Some(init_fn) = contract.init_function(db) {
        functions.push(ast::ContractStmt::Function(functions::func_def(
            context, init_fn,
        )));
    }

    if let Some(call_fn) = contract.call_function(db) {
        functions.push(ast::ContractStmt::Function(functions::func_def(
            context, call_fn,
        )));
    }

    let node = &contract.data(context.db).ast;
    Node::new(
        ast::Contract {
            name: node.kind.name.clone(),
            fields,
            body: [events, functions].concat(),
            pub_qual: None,
        },
        node.span,
    )
}

fn contract_field(context: &mut ModuleContext, field: ContractFieldId) -> Node<ast::Field> {
    let node = &field.data(context.db).ast;
    let typ = field.typ(context.db).expect("contract field type error");
    Node::new(
        ast::Field {
            is_pub: node.kind.is_pub,
            is_const: node.kind.is_const,
            name: node.kind.name.clone(),
            typ: types::type_desc(context, node.kind.typ.clone(), &typ),
            value: node.kind.value.clone(),
        },
        node.span,
    )
}
