use hir::hir_def::{scope_graph::ScopeId, Use};

use crate::HirAnalysisDb;

/// Return `true` if the given `target_scope` is visible from the `ref_scope`.
/// The resolved name is visible from `ref_scope` if
/// 1. It is declared as public, or
/// 2. The `ref_scope` is a transitive reflexive child of the scope where the
/// name is defined.
pub fn is_scope_visible(db: &dyn HirAnalysisDb, ref_scope: ScopeId, target_scope: ScopeId) -> bool {
    // If resolved is public, then it is visible.
    if target_scope.data(db.as_hir_db()).vis.is_pub() {
        return true;
    }

    let Some(def_scope) = (if matches!(target_scope, ScopeId::Field(..) | ScopeId::Variant(..)) {
        // We treat fields as if they are defined in the parent of the parent scope so
        // that field can be accessible from the scope where the parent is defined.
            target_scope.parent(db.as_hir_db()).and_then(|scope| scope.parent(db.as_hir_db()))
        } else {
            target_scope.parent(db.as_hir_db())
        })
    else {
        return false;
    };

    ref_scope.is_transitive_child_of(db.as_hir_db(), def_scope)
}

/// Return `true` if the given `use_` is visible from the `ref_scope`.
pub(super) fn is_use_visible(db: &dyn HirAnalysisDb, ref_scope: ScopeId, use_: Use) -> bool {
    let use_scope = ScopeId::from_item(use_.into());

    if use_scope.data(db.as_hir_db()).vis.is_pub() {
        return true;
    }

    let use_def_scope = use_scope.parent(db.as_hir_db()).unwrap();
    ref_scope.is_transitive_child_of(db.as_hir_db(), use_def_scope)
}
