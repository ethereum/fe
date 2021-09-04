use crate::context::AnalyzerContext;
use crate::db::Analysis;
use crate::errors::TypeError;
use crate::namespace::items::{StructField, StructFieldId, StructId};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types;
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;
use fe_parser::ast;
use indexmap::map::{Entry, IndexMap};
use std::rc::Rc;

pub fn struct_type(db: &dyn AnalyzerDb, struct_: StructId) -> Rc<types::Struct> {
    Rc::new(types::Struct {
        name: struct_.name(db),
        id: struct_,
        field_count: struct_.all_fields(db).len(),
    })
}

pub fn struct_all_fields(db: &dyn AnalyzerDb, struct_: StructId) -> Rc<Vec<StructFieldId>> {
    let struct_data = struct_.data(db);
    let fields = struct_data
        .ast
        .kind
        .fields
        .iter()
        .map(|node| {
            db.intern_struct_field(Rc::new(StructField {
                ast: node.clone(),
                parent: struct_,
            }))
        })
        .collect();
    Rc::new(fields)
}

pub fn struct_field_map(
    db: &dyn AnalyzerDb,
    struct_: StructId,
) -> Analysis<Rc<IndexMap<String, StructFieldId>>> {
    let mut scope = ItemScope::new(db, struct_.module(db));
    let mut fields = IndexMap::<String, StructFieldId>::new();

    let struct_name = struct_.name(db);
    for field in db.struct_all_fields(struct_).iter() {
        let node = &field.data(db).ast;

        match fields.entry(node.name().to_string()) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    &format!("duplicate field names in `struct {}`", struct_name,),
                    entry.key(),
                    entry.get().data(db).ast.span,
                    node.span,
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(*field);
            }
        }
    }

    Analysis {
        value: Rc::new(fields),
        diagnostics: Rc::new(scope.diagnostics),
    }
}

pub fn struct_field_type(
    db: &dyn AnalyzerDb,
    field: StructFieldId,
) -> Analysis<Result<types::FixedSize, TypeError>> {
    let field_data = field.data(db);

    let mut scope = ItemScope::new(db, field_data.parent.module(db));

    let ast::Field {
        is_pub,
        is_const,
        name: _,
        typ,
        value,
    } = &field_data.ast.kind;

    if *is_pub {
        scope.not_yet_implemented("struct `pub` fields", field_data.ast.span);
    }
    if *is_const {
        scope.not_yet_implemented("struct `const` fields", field_data.ast.span);
    }
    if let Some(_node) = value {
        scope.not_yet_implemented("struct field initial value assignment", field_data.ast.span);
    }
    let typ = match type_desc(&mut scope, typ) {
        Ok(types::Type::Base(base)) => Ok(types::FixedSize::Base(base)),
        Ok(_) => Err(TypeError::new(scope.not_yet_implemented(
            "non-primitive type struct fields",
            field_data.ast.span,
        ))),
        Err(err) => Err(err),
    };

    Analysis {
        value: typ,
        diagnostics: Rc::new(scope.diagnostics),
    }
}
