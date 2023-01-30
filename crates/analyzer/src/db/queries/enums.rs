use std::{rc::Rc, str::FromStr};

use fe_parser::ast;
use indexmap::{map::Entry, IndexMap};
use smallvec::SmallVec;
use smol_str::SmolStr;

use crate::{
    builtins,
    context::{Analysis, AnalyzerContext},
    errors::TypeError,
    namespace::{
        items::{
            self, DepGraph, DepGraphWrapper, DepLocality, EnumId, EnumVariant, EnumVariantId,
            EnumVariantKind, FunctionId, Item, TypeDef,
        },
        scopes::ItemScope,
        types::Type,
    },
    traversal::types::type_desc,
    AnalyzerDb,
};

pub fn enum_all_variants(db: &dyn AnalyzerDb, enum_: EnumId) -> Rc<[EnumVariantId]> {
    enum_
        .data(db)
        .ast
        .kind
        .variants
        .iter()
        .enumerate()
        .map(|(tag, variant)| {
            db.intern_enum_variant(Rc::new(EnumVariant {
                ast: variant.clone(),
                tag,
                parent: enum_,
            }))
        })
        .collect()
}

pub fn enum_variant_map(
    db: &dyn AnalyzerDb,
    enum_: EnumId,
) -> Analysis<Rc<IndexMap<SmolStr, EnumVariantId>>> {
    let scope = ItemScope::new(db, enum_.module(db));
    let mut variants = IndexMap::<SmolStr, EnumVariantId>::new();

    for &variant in db.enum_all_variants(enum_).iter() {
        let variant_name = variant.name(db);

        match variants.entry(variant_name) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    &format!("duplicate variant names in `enum {}`", enum_.name(db)),
                    entry.key(),
                    entry.get().data(db).ast.span,
                    variant.span(db),
                );
            }

            Entry::Vacant(entry) => {
                entry.insert(variant);
            }
        }
    }

    Analysis::new(Rc::new(variants), scope.diagnostics.take().into())
}

pub fn enum_variant_kind(
    db: &dyn AnalyzerDb,
    variant: EnumVariantId,
) -> Analysis<Result<EnumVariantKind, TypeError>> {
    let variant_data = variant.data(db);
    let mut scope = ItemScope::new(db, variant_data.parent.module(db));
    let self_ty = Some(variant.parent(db).as_type(db).as_trait_or_type());
    let kind = match &variant_data.ast.kind.kind {
        ast::VariantKind::Unit => Ok(EnumVariantKind::Unit),
        ast::VariantKind::Tuple(tuple) => {
            let elem_tys: Result<SmallVec<[_; 4]>, _> = tuple
                .iter()
                .map(
                    |ast_ty| match type_desc(&mut scope, ast_ty, self_ty.clone()) {
                        Ok(ty) if ty.has_fixed_size(db) => Ok(ty),
                        Ok(_) => Err(TypeError::new(scope.error(
                            "enum variant type must have a fixed size",
                            variant_data.ast.span,
                            "this can't be used as an struct field",
                        ))),
                        Err(err) => Err(err),
                    },
                )
                .collect();
            elem_tys.map(EnumVariantKind::Tuple)
        }
    };

    Analysis::new(kind, scope.diagnostics.take().into())
}

pub fn enum_all_functions(db: &dyn AnalyzerDb, enum_: EnumId) -> Rc<[FunctionId]> {
    let enum_data = enum_.data(db);
    enum_data
        .ast
        .kind
        .functions
        .iter()
        .map(|node| {
            db.intern_function(Rc::new(items::Function::new(
                db,
                node,
                Some(Item::Type(TypeDef::Enum(enum_))),
                enum_data.module,
            )))
        })
        .collect()
}

pub fn enum_function_map(
    db: &dyn AnalyzerDb,
    enum_: EnumId,
) -> Analysis<Rc<IndexMap<SmolStr, FunctionId>>> {
    let scope = ItemScope::new(db, enum_.module(db));
    let mut map = IndexMap::<SmolStr, FunctionId>::new();
    let variant_map = enum_.variants(db);

    for func in db.enum_all_functions(enum_).iter() {
        let def = &func.data(db).ast;
        let def_name = def.name();

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
                &format!("function name `{def_name}` conflicts with built-in function"),
                func.name_span(db),
                &format!("`{def_name}` is a built-in function"),
            );
            continue;
        }

        match map.entry(def_name.into()) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    &format!("duplicate function names in `struct {}`", enum_.name(db)),
                    entry.key(),
                    entry.get().data(db).ast.span,
                    def.span,
                );
            }

            Entry::Vacant(entry) => {
                if let Some(variant) = variant_map.get(def_name) {
                    scope.duplicate_name_error(
                        &format!("function name `{def_name}` conflicts with enum variant"),
                        def_name,
                        variant.span(db),
                        func.name_span(db),
                    );
                    continue;
                }
                entry.insert(*func);
            }
        }
    }
    Analysis::new(Rc::new(map), scope.diagnostics.take().into())
}

pub fn enum_dependency_graph(db: &dyn AnalyzerDb, enum_: EnumId) -> Analysis<DepGraphWrapper> {
    let scope = ItemScope::new(db, enum_.module(db));
    let root = Item::Type(TypeDef::Enum(enum_));
    let mut edges = vec![];

    for variant in enum_.variants(db).values() {
        match variant.kind(db) {
            Ok(EnumVariantKind::Unit) | Err(_) => {}
            Ok(EnumVariantKind::Tuple(elts)) => {
                for ty in elts {
                    let edge = match ty.typ(db) {
                        Type::Contract(id) => (
                            root,
                            Item::Type(TypeDef::Contract(id)),
                            DepLocality::External,
                        ),

                        Type::Struct(id) => {
                            (root, Item::Type(TypeDef::Struct(id)), DepLocality::Local)
                        }

                        Type::Enum(id) => (root, Item::Type(TypeDef::Enum(id)), DepLocality::Local),

                        _ => continue,
                    };
                    edges.push(edge);
                }
            }
        }
    }

    let mut graph = DepGraph::from_edges(edges.iter());
    for (_, item, _) in edges {
        if let Some(subgraph) = item.dependency_graph(db) {
            graph.extend(subgraph.all_edges())
        }
    }

    Analysis::new(
        DepGraphWrapper(Rc::new(graph)),
        scope.diagnostics.take().into(),
    )
}

pub fn enum_cycle(
    db: &dyn AnalyzerDb,
    _cycle: &[String],
    enum_: &EnumId,
) -> Analysis<DepGraphWrapper> {
    let scope = ItemScope::new(db, enum_.module(db));
    let enum_name = enum_.name(db);
    let enum_span = enum_.span(db);
    scope.error(
        &format!("recursive enum `{enum_name}`"),
        enum_span,
        &format!("enum `{enum_name}` has infinite size due to recursive definition",),
    );

    Analysis::new(
        DepGraphWrapper(Rc::new(DepGraph::new())),
        scope.diagnostics.take().into(),
    )
}
