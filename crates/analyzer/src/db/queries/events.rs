use crate::constants::MAX_INDEXED_EVENT_FIELDS;
use crate::context::AnalyzerContext;
use crate::db::Analysis;
use crate::errors::TypeError;
use crate::namespace::items::EventId;
use crate::namespace::scopes::ItemScope;
use crate::namespace::types;
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;
use fe_common::diagnostics::Label;
use fe_common::utils::humanize::pluralize_conditionally;
use fe_parser::ast;
use fe_parser::node::Node;
use std::collections::HashMap;
use std::rc::Rc;

// Event fields aren't interned for now, but they probably should be. If/when
// events are handled as normal type definitions, the current setup will run
// into a salsa cycle if a user tries to define an event that contains itself.

pub fn event_type(db: &dyn AnalyzerDb, event: EventId) -> Analysis<Rc<types::Event>> {
    let mut scope = ItemScope::new(db, event.module(db));

    let ast::Event {
        name: event_name,
        fields: field_nodes,
        pub_qual: _,
    } = &event.data(db).ast.kind;

    let mut names = HashMap::new();
    let mut indexed_count = 0;
    let fields = field_nodes
        .iter()
        .enumerate()
        .filter_map(|(index, field)| {
            let ast::EventField {
                is_idx,
                name,
                typ: typ_node,
            } = &field.kind;

            let typ = type_desc(&mut scope, typ_node).and_then(|typ| match typ {
                typ if typ.has_fixed_size(scope.db()) => Ok(typ),
                _ => Err(TypeError::new(scope.error(
                    "event field type must have a fixed size",
                    typ_node.span,
                    "this can't be used as an event field",
                ))),
            });

            // If we've already seen the max number of indexed fields,
            // ignore the `idx` qualifier on this one. We'll emit an error below.
            indexed_count += *is_idx as usize;

            if let Some(dup_idx) = names.get(&name.kind) {
                let dup_field: &Node<ast::EventField> = &field_nodes[*dup_idx];
                scope.fancy_error(
                    &format!("duplicate field names in `event {}`", &event_name.kind,),
                    vec![
                        Label::primary(
                            dup_field.span,
                            format!("`{}` first defined here", name.kind),
                        ),
                        Label::secondary(field.span, format!("`{}` redefined here", name.kind)),
                    ],
                    vec![],
                );
                None
            } else {
                names.insert(&name.kind, index);
                Some(types::EventField {
                    name: name.kind.clone(),
                    typ,
                    is_indexed: *is_idx,
                })
            }
        })
        .collect();

    if indexed_count > MAX_INDEXED_EVENT_FIELDS {
        let excess_count = indexed_count - MAX_INDEXED_EVENT_FIELDS;

        let mut labels = field_nodes
            .iter()
            .filter_map(|field| {
                field
                    .kind
                    .is_idx
                    .then(|| Label::primary(field.span, String::new()))
            })
            .collect::<Vec<Label>>();
        labels.last_mut().unwrap().message = format!("{} indexed fields", indexed_count);

        scope.fancy_error(
            &format!(
                "more than three indexed fields in `event {}`",
                event_name.kind
            ),
            labels,
            vec![format!(
                "Note: Remove the `idx` keyword from at least {} {}.",
                excess_count,
                pluralize_conditionally("field", excess_count)
            )],
        );
    }

    Analysis {
        value: Rc::new(types::Event {
            name: event_name.kind.clone(),
            fields,
        }),
        diagnostics: scope.diagnostics.take().into(),
    }
}
