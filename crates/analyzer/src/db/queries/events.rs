use crate::constants::MAX_INDEXED_EVENT_FIELDS;
use crate::context::AnalyzerContext;
use crate::db::Analysis;
use crate::namespace::items::EventId;
use crate::namespace::scopes::ItemScope;
use crate::namespace::{events, types};
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;
use fe_common::diagnostics::Label;
use fe_common::utils::humanize::pluralize_conditionally;
use fe_parser::ast;
use fe_parser::node::Node;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

pub fn event_type(db: &dyn AnalyzerDb, event: EventId) -> Analysis<Rc<events::EventDef>> {
    let mut scope = ItemScope::new(db, event.module(db));

    let ast::Event {
        name: event_name,
        fields: field_nodes,
    } = &event.data(db).ast.kind;

    let mut names = HashMap::new();
    let mut indexed_count = 0;
    let fields = field_nodes
        .iter()
        .enumerate()
        .filter_map(|(index, field)| {
            let ast::EventField {
                mut is_idx,
                name,
                typ,
            } = &field.kind;

            let typ = match type_desc(&mut scope, typ).try_into() {
                Ok(typ) => typ,
                Err(_) => types::FixedSize::unknown(),
            };

            // If we've already seen the max number of indexed fields,
            // ignore the `idx` qualifier on this one. We'll emit an error below.
            indexed_count += is_idx as usize;
            is_idx = indexed_count <= MAX_INDEXED_EVENT_FIELDS;

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
                Some(events::EventField {
                    name: name.kind.clone(),
                    typ,
                    is_indexed: is_idx,
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
        value: Rc::new(events::EventDef {
            name: event_name.kind.clone(),
            fields,
        }),
        diagnostics: Rc::new(scope.diagnostics),
    }
}
