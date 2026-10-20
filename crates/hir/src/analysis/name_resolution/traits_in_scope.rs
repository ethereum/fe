use crate::core::hir_def::{
    Body, ExprId, ItemKind, Mod, TopLevelMod, Trait, WhereClauseOwner, scope_graph::ScopeId,
};
use common::indexmap::IndexSet;

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        NameResKind,
        path_resolver::{resolve_type_path_definition, shadowed_trait},
        resolve_imports,
    },
};

/// Returns the all traits that are available in the given scope.
pub fn available_traits_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> &'db IndexSet<Trait<'db>> {
    let scope_kind = TraitScopeKind::from_scope(db, scope);
    let trait_scope = TraitScope::new(db, scope_kind);
    available_traits_in_scope_impl(db, trait_scope)
}

/// The traits whose items a method call or `Type::Item` path in `scope` may
/// select among several candidates: those in scope, `traits` (such as the
/// traits of the assumptions), and all their super-traits.
///
/// Super-traits are found from their paths alone, as lowering names them.
/// Lowering a super-trait reference could resolve the path being selected
/// again, as `u8::N` in `trait Top: Parent<{ u8::N }>`, where `Top` is itself
/// in scope. Any super-trait missed here would silently make another candidate
/// the only one in scope.
pub(crate) fn traits_available_for_selection<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    traits: impl IntoIterator<Item = Trait<'db>>,
) -> IndexSet<Trait<'db>> {
    let mut pending: Vec<Trait<'db>> = available_traits_in_scope(db, scope)
        .iter()
        .copied()
        .chain(traits)
        .collect();
    let mut available = IndexSet::default();
    while let Some(trait_def) = pending.pop() {
        if !available.insert(trait_def) {
            continue;
        }
        let super_refs = trait_def
            .super_trait_refs(db)
            .map(|super_ref| super_ref.trait_ref(db));
        let self_bounds = WhereClauseOwner::Trait(trait_def)
            .clause(db)
            .predicates(db)
            .filter(|pred| pred.is_self_subject(db))
            .flat_map(|pred| pred.bounds(db).map(|bound| bound.trait_ref(db)));
        pending.extend(super_refs.chain(self_bounds).filter_map(|trait_ref| {
            let path = trait_ref.path(db).to_opt()?;
            let scope = trait_def.scope();
            let definition = resolve_type_path_definition(db, path, scope)?;
            match shadowed_trait(db, path, scope, definition).map_or(definition, |outer| outer.kind)
            {
                NameResKind::Scope(ScopeId::Item(ItemKind::Trait(super_trait))) => {
                    Some(super_trait)
                }
                NameResKind::Scope(_) | NameResKind::Prim(_) => None,
            }
        }));
    }
    available
}

#[salsa::tracked(return_ref)]
pub(crate) fn available_traits_in_scope_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    t_scope: TraitScope<'db>,
) -> IndexSet<Trait<'db>> {
    let mut traits = IndexSet::default();
    let scope = t_scope.inner(db).to_scope();

    let imports = &resolve_imports(db, scope.ingot(db)).1;
    if let Some(named) = imports.named_resolved.get(&scope) {
        named
            .values()
            .flat_map(|bucket| bucket.iter_ok())
            .for_each(|name_res| {
                if let Some(trait_) = name_res.trait_() {
                    traits.insert(trait_);
                }
            })
    }

    if let Some(glob) = imports.glob_resolved.get(&scope) {
        glob.iter()
            .flat_map(|(_, map)| map.values().flat_map(|v| v.iter()))
            .for_each(|name_res| {
                if let Some(trait_) = name_res.trait_() {
                    traits.insert(trait_);
                }
            })
    }

    if let Some(unnamed) = imports.unnamed_resolved.get(&scope) {
        unnamed
            .iter()
            .flat_map(|bucket| bucket.iter_ok())
            .for_each(|name_res| {
                if let Some(trait_) = name_res.trait_() {
                    traits.insert(trait_);
                }
            })
    }

    for child in scope.child_items(db) {
        if let ItemKind::Trait(trait_) = child {
            traits.insert(trait_);
        }
    }

    if let Some(parent) = scope.parent(db) {
        let parent_traits = available_traits_in_scope(db, parent);
        traits.extend(parent_traits.iter().copied());
    }

    traits
}

#[salsa::interned]
#[derive(Debug)]
pub struct TraitScope<'db> {
    inner: TraitScopeKind<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitScopeKind<'db> {
    TopLevelMod(TopLevelMod<'db>),
    Module(Mod<'db>),
    Block(Body<'db>, ExprId),
}

impl<'db> TraitScopeKind<'db> {
    fn from_scope(db: &'db dyn HirAnalysisDb, mut scope: ScopeId<'db>) -> Self {
        loop {
            match scope {
                ScopeId::Item(item) => match item {
                    ItemKind::TopMod(top_level_mod) => {
                        return TraitScopeKind::TopLevelMod(top_level_mod);
                    }
                    ItemKind::Mod(mod_) => {
                        return TraitScopeKind::Module(mod_);
                    }
                    _ => {}
                },
                ScopeId::Block(body, expr_id) => {
                    return TraitScopeKind::Block(body, expr_id);
                }
                _ => {}
            }
            scope = scope.parent(db).unwrap();
        }
    }

    fn to_scope(&self) -> ScopeId<'db> {
        match self {
            TraitScopeKind::TopLevelMod(top_level_mod) => {
                ScopeId::Item(ItemKind::TopMod(*top_level_mod))
            }
            TraitScopeKind::Module(mod_) => ScopeId::Item(ItemKind::Mod(*mod_)),
            TraitScopeKind::Block(body, expr_id) => ScopeId::Block(*body, *expr_id),
        }
    }
}
