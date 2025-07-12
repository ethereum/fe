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
    trait_def::impls_for_ty_with_constraints,
    trait_resolution::{constraint::collect_constraints, PredicateListId},
    ty_def::{AssocTy, TyData, TyId, TyParam},
    ty_lower::lower_hir_ty,
    unify::{self, UnificationTable},
};
use crate::{name_resolution::resolve_assoc_ty, HirAnalysisDb};

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
    if ty.pretty_print(db).contains("IntoIter::Item") {
        eprintln!("Normalizing: {}", ty.pretty_print(db));
        eprintln!("Assumptions: {}", assumptions.pretty_print(db));
    }

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
            if ty.pretty_print(db).contains("IntoIter::Item") {
                eprintln!("Final normalized result: {}", normalized.pretty_print(db));
            }
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
        eprintln!(
            "try_resolve_assoc_ty: {} with trait {}",
            ty.pretty_print(self.db),
            assoc.trait_.pretty_print(self.db, false)
        );

        // First check if the trait instance has a binding for this associated type
        if let Some(&bound_ty) = assoc.trait_.assoc_type_bindings(self.db).get(&assoc.name) {
            eprintln!(
                "Found binding: {} = {}",
                assoc.name.data(self.db),
                bound_ty.pretty_print(self.db)
            );
            return Some(bound_ty);
        }

        // Try to resolve using the full resolution logic
        let canonical_ty = Canonical::new(self.db, ty);
        let mut candidates = IndexSet::new();

        // Use the same resolution logic as path resolution
        resolve_assoc_ty(
            self.db,
            self.scope,
            canonical_ty,
            assoc.name,
            self.assumptions,
            &mut candidates,
            assoc,
        );

        // Also try to find implementations for concrete types
        if !assoc.trait_.self_ty(self.db).is_ty_var(self.db) {
            let ingot = self.scope.ingot(self.db);
            let self_ty = assoc.trait_.self_ty(self.db);
            let canonical_self_ty = Canonical::new(self.db, self_ty);
            for implementor in
                impls_for_ty_with_constraints(self.db, ingot, canonical_self_ty, self.assumptions)
            {
                let mut table = UnificationTable::new(self.db);
                let implementor_instance = table.instantiate_with_fresh_vars(implementor);

                // Check if this implementor is for the right trait
                if implementor_instance.trait_def(self.db) == assoc.trait_.def(self.db)
                    && table
                        .unify(self_ty, implementor_instance.self_ty(self.db))
                        .is_ok()
                {
                    if let Some(&ty) = implementor_instance.types(self.db).get(&assoc.name) {
                        let resolved_ty = ty.fold_with(&mut table);
                        candidates.insert((implementor_instance.trait_(self.db), resolved_ty));
                    }
                }
            }
        }

        // If we have exactly one candidate, use it
        if candidates.len() == 1 {
            candidates.first().map(|(_, ty)| *ty)
        } else {
            None
        }
    }
}

/// Helper function for normalizing types within a unification context.
/// This is used when we already have a unification table and want to normalize
/// types while preserving the current unification state.
pub fn normalize_ty_with_table<'db, U>(
    ty: TyId<'db>,
    table: &mut unify::UnificationTableBase<'db, U>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db>
where
    U: unify::UnificationStore<'db>,
{
    // First fold with the table to resolve any type variables
    let ty = ty.fold_with(table);

    // Then normalize associated types
    normalize_ty(table.db, ty, scope, assumptions)
}
