use hir::hir_def::scope_graph::ScopeId;

use crate::HirAnalysisDb;

use super::name_resolver::{NameDomain, ResolvedName, ResolvedNameKind};

/// Return `true` if the given `resolved` is visible from the `ref_scope`.
/// The resolved name is visible from `ref_scope` if
/// 1. It is declared as public, or
/// 2. The `ref_scope` is a child or the same scope of the scope where the
///    resolved name is defined.
pub fn check_visibility(
    db: &dyn HirAnalysisDb,
    ref_scope: ScopeId,
    resolved: &ResolvedName,
) -> bool {
    let ResolvedNameKind::Scope{scope, .. } = resolved.kind else {
        // If resolved is a builtin name, then it's always visible .
        return true;
    };

    // If resolved is public, then it is visible.
    if scope.data(db.upcast()).vis.is_pub() {
        return true;
    }

    let Some(def_scope) = (if resolved.domain == NameDomain::Field {
        // We treat fields as if they are defined in the parent of the parent scope so
        // that field can be accessible from the scope where the parent is defined.
            scope.parent(db.upcast()).and_then(|scope| scope.parent(db.upcast()))
        } else {
            scope.parent(db.upcast())
        })
    else {
        return false;
    };

    // If ref scope is a child scope or the same scope of the def scope, then it is
    // visible.
    let mut parent = Some(ref_scope);
    while let Some(scope) = parent {
        if scope == def_scope {
            return true;
        } else {
            parent = scope.parent(db.upcast());
        }
    }

    false
}
