use crate::builtins;
use crate::context::AnalyzerContext;
use crate::db::Analysis;
use crate::errors::TypeError;
use crate::namespace::items::{
    self, DepGraph, DepGraphWrapper, DepLocality, FunctionId, Item, StructField, StructFieldId,
    StructId, TypeDef,
};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types::{Type, TypeId};
use crate::traversal::types::type_desc;
use crate::AnalyzerDb;
use fe_parser::ast;
use indexmap::map::{Entry, IndexMap};
use smol_str::SmolStr;
use std::rc::Rc;
use std::str::FromStr;

pub fn struct_all_fields(db: &dyn AnalyzerDb, struct_: StructId) -> Rc<[StructFieldId]> {
    struct_
        .data(db)
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
        .collect()
}

pub fn struct_field_map(
    db: &dyn AnalyzerDb,
    struct_: StructId,
) -> Analysis<Rc<IndexMap<SmolStr, StructFieldId>>> {
    let scope = ItemScope::new(db, struct_.module(db));
    let mut fields = IndexMap::<SmolStr, StructFieldId>::new();

    let struct_name = struct_.name(db);
    for field in db.struct_all_fields(struct_).iter() {
        let node = &field.data(db).ast;

        match fields.entry(node.name().into()) {
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

    Analysis::new(Rc::new(fields), scope.diagnostics.take().into())
}

pub fn struct_field_type(
    db: &dyn AnalyzerDb,
    field: StructFieldId,
) -> Analysis<Result<TypeId, TypeError>> {
    let field_data = field.data(db);

    let mut scope = ItemScope::new(db, field_data.parent.module(db));

    let ast::Field {
        is_pub: _,
        is_const,
        name: _,
        typ,
        value,
    } = &field_data.ast.kind;

    if *is_const {
        scope.not_yet_implemented("struct `const` fields", field_data.ast.span);
    }
    if let Some(_node) = value {
        scope.not_yet_implemented("struct field initial value assignment", field_data.ast.span);
    }
    let typ = match type_desc(&mut scope, typ) {
        Ok(typ) => match typ.typ(db) {
            Type::Contract(_) => {
                scope.not_yet_implemented(
                    "contract types aren't yet supported as struct fields",
                    field_data.ast.span,
                );
                Ok(typ)
            }
            t if t.has_fixed_size() => Ok(typ),
            _ => Err(TypeError::new(scope.error(
                "struct field type must have a fixed size",
                field_data.ast.span,
                "this can't be used as an struct field",
            ))),
        },
        Err(err) => Err(err),
    };

    Analysis::new(typ, scope.diagnostics.take().into())
}

pub fn struct_all_functions(db: &dyn AnalyzerDb, struct_: StructId) -> Rc<[FunctionId]> {
    let struct_data = struct_.data(db);
    struct_data
        .ast
        .kind
        .functions
        .iter()
        .map(|node| {
            db.intern_function(Rc::new(items::Function::new(
                db,
                node,
                Some(Item::Type(TypeDef::Struct(struct_))),
                struct_data.module,
            )))
        })
        .collect()
}

pub fn struct_function_map(
    db: &dyn AnalyzerDb,
    struct_: StructId,
) -> Analysis<Rc<IndexMap<SmolStr, FunctionId>>> {
    let mut scope = ItemScope::new(db, struct_.module(db));
    let mut map = IndexMap::<SmolStr, FunctionId>::new();

    for func in db.struct_all_functions(struct_).iter() {
        let def = &func.data(db).ast;
        let def_name = def.name();
        if def_name == "__init__" {
            continue;
        }

        if let Ok(Some(named_item)) = scope.resolve_name(def_name, func.name_span(db)) {
            scope.name_conflict_error(
                "function",
                def_name,
                &named_item,
                named_item.name_span(db),
                func.name_span(db),
            );
            continue;
        }

        if builtins::ValueMethod::from_str(def_name).is_ok() {
            scope.error(
                &format!(
                    "function name `{}` conflicts with built-in function",
                    def_name
                ),
                func.name_span(db),
                &format!("`{}` is a built-in function", def_name),
            );
            continue;
        }

        match map.entry(def_name.into()) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    &format!("duplicate function names in `struct {}`", struct_.name(db)),
                    entry.key(),
                    entry.get().data(db).ast.span,
                    def.span,
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(*func);
            }
        }
    }
    Analysis::new(Rc::new(map), scope.diagnostics.take().into())
}

pub fn struct_dependency_graph(
    db: &dyn AnalyzerDb,
    struct_: StructId,
) -> Analysis<DepGraphWrapper> {
    // A struct depends on the types of its fields and on everything they depend on.
    // It *does not* depend on its public functions; those will only be part of
    // the broader dependency graph if they're in the call graph of some public
    // contract function.

    let scope = ItemScope::new(db, struct_.module(db));
    let root = Item::Type(TypeDef::Struct(struct_));
    let fields = struct_
        .fields(db)
        .values()
        .filter_map(|field| match field.typ(db).ok()?.typ(db) {
            Type::Contract(id) => Some((
                root,
                Item::Type(TypeDef::Contract(id)),
                DepLocality::External,
            )),
            // Not possible yet, but it will be soon
            Type::Struct(id) => Some((root, Item::Type(TypeDef::Struct(id)), DepLocality::Local)),
            _ => None,
        })
        .collect::<Vec<_>>();

    let mut graph = DepGraph::from_edges(fields.iter());
    for (_, item, _) in fields {
        if let Some(subgraph) = item.dependency_graph(db) {
            graph.extend(subgraph.all_edges())
        }
    }

    Analysis::new(
        DepGraphWrapper(Rc::new(graph)),
        scope.diagnostics.take().into(),
    )
}

pub fn struct_cycle(
    db: &dyn AnalyzerDb,
    _cycle: &[String],
    struct_: &StructId,
) -> Analysis<DepGraphWrapper> {
    let mut scope = ItemScope::new(db, struct_.module(db));
    let struct_data = &struct_.data(db).ast;
    scope.error(
        &format!("recursive struct `{}`", struct_data.name()),
        struct_data.kind.name.span,
        &format!(
            "struct `{}` has infinite size due to recursive definition",
            struct_data.name(),
        ),
    );

    Analysis::new(
        DepGraphWrapper(Rc::new(DepGraph::new())),
        scope.diagnostics.take().into(),
    )
}
