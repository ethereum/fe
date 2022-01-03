use crate::context::ModuleContext;
use crate::mappers::{functions, types};
use crate::utils::ZeroSpanNode;
use fe_analyzer::namespace::items::{ContractFieldId, ContractId, EventId};
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
        .map(|event| ast::ContractStmt::Event(event_def(context, *event)))
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

fn event_def(context: &mut ModuleContext, event: EventId) -> Node<ast::Event> {
    let ast_fields = &event.data(context.db).ast.kind.fields;
    let fields = event
        .typ(context.db)
        .fields
        .iter()
        .zip(ast_fields.iter())
        .map(|(field, node)| {
            ast::EventField {
                is_idx: field.is_indexed,
                name: field.name.clone().into_node(),
                typ: types::type_desc(
                    context,
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

    let node = &event.data(context.db).ast;
    Node::new(
        ast::Event {
            name: node.kind.name.clone(),
            fields,
        },
        node.span,
    )
}
