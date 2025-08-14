//! Type normalization module
//!
//! This module provides functionality to normalize types by resolving associated types
//! to concrete types when possible. This happens before type unification to ensure
//! that types are in their most resolved form.

use common::indexmap::IndexSet;
use hir::hir_def::{scope_graph::ScopeId, ImplTrait};

use super::{
    canonical::Canonical,
    fold::{TyFoldable, TyFolder},
    trait_resolution::{constraint::collect_constraints, PredicateListId},
    ty_def::{AssocTy, TyData, TyId, TyParam},
    ty_lower::lower_hir_ty,
    unify::UnificationTable,
};
use crate::{name_resolution::find_associated_type, HirAnalysisDb};

/// Normalizes a type by resolving all associated types to concrete types when possible.
///
/// This function takes a type and attempts to resolve any associated types within it
/// using the provided assumptions and scope context. It handles:
/// - Simple associated types (e.g., `T::Output`)
/// - Nested associated types (e.g., `T::Encoder::Output`)
/// - Associated types with generic parameters
pub fn normalize_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let mut normalizer = TypeNormalizer {
        db,
        scope,
        assumptions,
        seen: IndexSet::new(),
    };

    // Keep normalizing until we reach a fixed point
    let mut current = ty;
    loop {
        let normalized = current.fold_with(&mut normalizer);
        if normalized == current {
            break normalized;
        }
        current = normalized;
        // Clear the seen set for the next iteration
        normalizer.seen.clear();
    }
}

struct TypeNormalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    seen: IndexSet<AssocTy<'db>>,
}

impl<'db> TyFolder<'db> for TypeNormalizer<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    fn fold_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(self.db) {
            TyData::TyParam(p @ TyParam { owner, .. }) if p.is_trait_self() => {
                if let Some(impl_) = owner.resolve_to::<ImplTrait>(self.db) {
                    if let Some(hir_ty) = impl_.ty(self.db).to_opt() {
                        let impl_assumptions =
                            collect_constraints(self.db, impl_.into()).instantiate_identity();
                        return lower_hir_ty(self.db, hir_ty, impl_.scope(), impl_assumptions);
                    }
                }
                ty
            }
            TyData::AssocTy(assoc_ty) => {
                // Prevent infinite recursion
                if self.seen.contains(assoc_ty) {
                    return ty;
                }
                self.seen.insert(assoc_ty.clone());

                let resolved = self.try_resolve_assoc_ty(ty, assoc_ty);
                let result = resolved.unwrap_or(ty);

                self.seen.remove(assoc_ty);

                // Continue folding the result in case it contains more associated types
                if result != ty {
                    self.fold_ty(result)
                } else {
                    result.super_fold_with(self)
                }
            }
            _ => ty.super_fold_with(self),
        }
    }
}

impl<'db> TypeNormalizer<'db> {
    fn try_resolve_assoc_ty(&mut self, ty: TyId<'db>, assoc: &AssocTy<'db>) -> Option<TyId<'db>> {
        // 1) Check if the trait instance itself carries an explicit binding
        if let Some(&bound_ty) = assoc.trait_.assoc_type_bindings(self.db).get(&assoc.name) {
            return Some(bound_ty);
        }

        // 2) Check assumptions for an equivalent trait instance that carries
        //    an explicit associated type binding (e.g., from where-clauses).
        for &pred in self.assumptions.list(self.db) {
            if pred.def(self.db) != assoc.trait_.def(self.db) {
                continue;
            }

            let mut table = UnificationTable::new(self.db);
            let lhs_self = assoc.trait_.self_ty(self.db);
            let rhs_self = pred.self_ty(self.db);
            if table.unify(lhs_self, rhs_self).is_ok() {
                if let Some(&bound) = pred.assoc_type_bindings(self.db).get(&assoc.name) {
                    return Some(bound.fold_with(&mut table));
                }
            }
        }

        // 3) Fall back to the general associated type search used by path resolution,
        //    but restrict results to the same trait as `assoc`.
        //    Search by the trait's self type: `SelfTy::assoc.name`.
        let self_ty = assoc.trait_.self_ty(self.db);
        let mut cands = find_associated_type(
            self.db,
            self.scope,
            Canonical::new(self.db, self_ty),
            assoc.name,
            self.assumptions,
        );

        // Keep only candidates from the same trait as `assoc`.
        cands.retain(|(inst, _)| inst.def(self.db) == assoc.trait_.def(self.db));
        match cands.as_slice() {
            [] => None,
            // Unique candidate: return it if it actually changes the type
            [(_, t)] if *t != ty => Some(*t),
            _ => None,
        }
    }
}
