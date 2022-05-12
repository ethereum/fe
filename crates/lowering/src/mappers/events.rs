use crate::context::ModuleContext;
use crate::mappers::types;
use crate::utils::ZeroSpanNode;
use fe_analyzer::namespace::items::EventId;
use fe_parser::ast;
use fe_parser::node::Node;

pub fn event_def(context: &mut ModuleContext, event: EventId) -> Node<ast::Event> {
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
                    &field.typ.as_ref().expect("event field type error").clone(),
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
            pub_qual: node.kind.pub_qual,
        },
        node.span,
    )
}
