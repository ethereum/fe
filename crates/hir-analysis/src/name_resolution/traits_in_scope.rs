use hir::hir_def::{scope_graph::ScopeId, Body, ExprId, ItemKind, Mod, TopLevelMod, Trait};
use rustc_hash::FxHashSet;

use crate::{name_resolution::resolve_imports, HirAnalysisDb};

/// Returns the all traits that are available in the given scope.
pub fn available_traits_in_scope(db: &dyn HirAnalysisDb, scope: ScopeId) -> &FxHashSet<Trait> {
    let scope_kind = TraitScopeKind::from_scope(db, scope);
    let trait_scope = TraitScope::new(db, scope_kind);
    available_traits_in_scope_impl(db, trait_scope)
}

#[salsa::tracked(return_ref)]
pub(crate) fn available_traits_in_scope_impl(
    db: &dyn HirAnalysisDb,
    t_scope: TraitScope,
) -> FxHashSet<Trait> {
    let mut traits = FxHashSet::default();
    let scope = t_scope.inner(db).to_scope();

    let imports = resolve_imports(db, scope.ingot(db.as_hir_db()));
    if let Some(named) = imports.named_resolved.get(&scope) {
        named
            .values()
            .flat_map(|bucket| bucket.iter())
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
            .flat_map(|bucket| bucket.iter())
            .for_each(|name_res| {
                if let Some(trait_) = name_res.trait_() {
                    traits.insert(trait_);
                }
            })
    }

    for child in scope.child_items(db.as_hir_db()) {
        if let ItemKind::Trait(trait_) = child {
            traits.insert(trait_);
        }
    }

    if let Some(parent) = scope.parent(db.as_hir_db()) {
        let parent_traits = available_traits_in_scope(db, parent);
        traits.extend(parent_traits.iter().copied());
    }

    traits
}

#[salsa::interned]
pub(crate) struct TraitScope {
    inner: TraitScopeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TraitScopeKind {
    TopLevelMod(TopLevelMod),
    Module(Mod),
    Block(Body, ExprId),
}

impl TraitScopeKind {
    fn from_scope(db: &dyn HirAnalysisDb, mut scope: ScopeId) -> Self {
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
            scope = scope.parent(db.as_hir_db()).unwrap();
        }
    }

    fn to_scope(&self) -> ScopeId {
        match self {
            TraitScopeKind::TopLevelMod(top_level_mod) => {
                ScopeId::Item(ItemKind::TopMod(*top_level_mod))
            }
            TraitScopeKind::Module(mod_) => ScopeId::Item(ItemKind::Mod(*mod_)),
            TraitScopeKind::Block(body, expr_id) => ScopeId::Block(*body, *expr_id),
        }
    }
}
