use crate::context::AnalyzerContext;
use crate::db::Analysis;
use crate::namespace::items::StructId;
use crate::namespace::scopes::ItemScope;
use crate::namespace::types;
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;
use fe_common::diagnostics::Label;
use fe_parser::ast;
use fe_parser::node::Node;
use std::collections::HashMap;
use std::rc::Rc;

pub fn struct_type(db: &dyn AnalyzerDb, struct_: StructId) -> Analysis<Rc<types::Struct>> {
    let mut scope = ItemScope::new(db, struct_.data(db).module);

    let ast::Struct {
        name: struct_name,
        fields: field_nodes,
    } = &struct_.data(db).ast.kind;

    let mut names = HashMap::new();
    let fields = field_nodes
        .iter()
        .enumerate()
        .filter_map(|(index, field)| {
            let ast::Field {
                is_pub,
                is_const,
                name,
                typ,
                value,
            } = &field.kind;

            if *is_pub {
                scope.not_yet_implemented("struct `pub` fields", field.span);
            }
            if *is_const {
                scope.not_yet_implemented("struct `const` fields", field.span);
            }
            if let Some(node) = value {
                scope.not_yet_implemented("struct field initial value assignment", field.span);
            }
            let typ = match type_desc(&mut scope, typ) {
                types::Type::Base(base) => types::FixedSize::Base(base),
                _ => {
                    scope.not_yet_implemented("non-primitive type struct fields", field.span);
                    types::FixedSize::unknown()
                }
            };

            if let Some(dup_idx) = names.get(&name.kind) {
                let dup_field: &Node<ast::Field> = &field_nodes[*dup_idx];
                scope.fancy_error(
                    &format!("duplicate field names in `struct {}`", &struct_name.kind,),
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
                Some(types::StructField {
                    name: name.kind.clone(),
                    typ,
                })
            }
        })
        .collect();

    Analysis {
        value: Rc::new(types::Struct {
            name: struct_name.kind.clone(),
            fields,
        }),
        diagnostics: Rc::new(scope.diagnostics),
    }
}
