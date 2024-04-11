use hir::hir_def::{scope_graph::ScopeId, ItemKind, Use};

use crate::HirAnalysisDb;

/// Return `true` if the given `scope` is visible from the `scope`.
/// name is defined.
pub(crate) fn is_scope_visible_from(
    db: &dyn HirAnalysisDb,
    scope: ScopeId,
    from_scope: ScopeId,
) -> bool {
    let hir_db = db.as_hir_db();
    // If resolved is public, then it is visible.
    if scope.data(hir_db).vis.is_pub() {
        return true;
    }

    let Some(def_scope) = (match scope {
        ScopeId::Item(ItemKind::Func(func)) => {
            let parent_item = scope.parent_item(hir_db);
            if matches!(parent_item, Some(ItemKind::Trait(..))) {
                return true;
            }

            if func.is_associated_func(hir_db) {
                scope
                    .parent_item(hir_db)
                    .and_then(|item| ScopeId::Item(item).parent(hir_db))
            } else {
                Some(scope)
            }
        }
        ScopeId::Item(_) => scope.parent(hir_db),
        ScopeId::Field(..) | ScopeId::Variant(..) => {
            let parent_item = scope.item();
            ScopeId::Item(parent_item).parent(hir_db)
        }

        _ => scope.parent(hir_db),
    }) else {
        return false;
    };

    from_scope.is_transitive_child_of(db.as_hir_db(), def_scope)
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
