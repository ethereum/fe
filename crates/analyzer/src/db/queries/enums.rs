use std::rc::Rc;

use fe_parser::ast;
use indexmap::{map::Entry, IndexMap};
use smallvec::SmallVec;
use smol_str::SmolStr;

use crate::{
    context::{Analysis, AnalyzerContext},
    errors::TypeError,
    namespace::{
        items::{
            DepGraph, DepGraphWrapper, DepLocality, EnumId, EnumVariant, EnumVariantId,
            EnumVariantKind, Item, TypeDef,
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
                    &format!("duplicate field names in `enum {}`", enum_.name(db)),
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

    let kind = match &variant_data.ast.kind.kind {
        ast::VariantKind::Unit => Ok(EnumVariantKind::Unit),
        ast::VariantKind::Tuple(tuple) => {
            let elem_tys: Result<SmallVec<[_; 4]>, _> = tuple
                .iter()
                .map(|ast_ty| match type_desc(&mut scope, ast_ty) {
                    Ok(ty) if ty.typ(db).has_fixed_size() => Ok(ty),
                    Ok(_) => Err(TypeError::new(scope.error(
                        "enum variant type must have a fixed size",
                        variant_data.ast.span,
                        "this can't be used as an struct field",
                    ))),
                    Err(err) => Err(err),
                })
                .collect();
            elem_tys.map(EnumVariantKind::Tuple)
        }
    };

    Analysis::new(kind, scope.diagnostics.take().into())
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
    let mut scope = ItemScope::new(db, enum_.module(db));
    let enum_name = enum_.name(db);
    let enum_span = enum_.span(db);
    scope.error(
        &format!("recursive enum `{}`", enum_name),
        enum_span,
        &format!(
            "enum `{}` has infinite size due to recursive definition",
            enum_name,
        ),
    );

    Analysis::new(
        DepGraphWrapper(Rc::new(DepGraph::new())),
        scope.diagnostics.take().into(),
    )
}
