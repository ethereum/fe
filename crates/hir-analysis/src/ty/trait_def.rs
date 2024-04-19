//! This module contains all trait related types definitions.

use std::collections::{BTreeMap, BTreeSet};

use hir::{
    hir_def::{IdentId, ImplTrait, IngotId, Trait},
    span::DynLazySpan,
};
use rustc_hash::FxHashMap;

use super::{
    binder::Binder,
    canonical::Canonical,
    constraint::{collect_super_traits, AssumptionListId, ConstraintListId},
    constraint_solver::{check_trait_inst_wf, GoalSatisfiability},
    diagnostics::{TraitConstraintDiag, TyDiagCollection},
    func_def::FuncDef,
    trait_lower::collect_implementor_methods,
    ty_def::{Kind, TyId},
    ty_lower::GenericParamTypeSet,
    unify::UnificationTable,
};
use crate::{
    ty::{constraint::collect_implementor_constraints, trait_lower::collect_trait_impls},
    HirAnalysisDb,
};

/// Returns [`TraitEnv`] for the given ingot.
#[salsa::tracked(return_ref)]
pub(crate) fn ingot_trait_env(db: &dyn HirAnalysisDb, ingot: IngotId) -> TraitEnv {
    TraitEnv::collect(db, ingot)
}

/// Returns all [`Implementor`]s for the given trait inst.
#[salsa::tracked(return_ref)]
pub(crate) fn impls_of_trait(
    db: &dyn HirAnalysisDb,
    trait_: TraitInstId,
) -> Vec<Binder<Implementor>> {
    let env = ingot_trait_env(db, trait_.ingot(db));
    let Some(impls) = env.impls.get(&trait_.def(db)) else {
        return vec![];
    };

    impls
        .iter()
        .filter(|impl_| {
            let mut table = UnificationTable::new(db);
            let impl_ = table.instantiate_with_fresh_vars(**impl_);
            table.unify(impl_.trait_(db), trait_).is_ok()
        })
        .cloned()
        .collect()
}

#[salsa::tracked(return_ref)]
pub(crate) fn impls_of_ty(
    db: &dyn HirAnalysisDb,
    ingot: IngotId,
    ty: Canonical<TyId>,
) -> Vec<Binder<Implementor>> {
    let mut table = UnificationTable::new(db);
    let ty = ty.decanonicalize(&mut table);
    let env = ingot_trait_env(db, ingot);
    if ty.has_invalid(db) {
        return vec![];
    }

    let Some(cands) = env.ty_to_implementors.get(&ty.base_ty(db)) else {
        return vec![];
    };

    cands
        .iter()
        .copied()
        .filter(|impl_| {
            let snapshot = table.snapshot();

            let impl_ = table.instantiate_with_fresh_vars(*impl_);
            let impl_ty = table.instantiate_to_term(impl_.ty(db));
            let ty = table.instantiate_to_term(ty);
            let is_ok = table.unify(impl_ty, ty).is_ok();

            table.rollback_to(snapshot);

            is_ok
        })
        .collect()
}

/// Represents the trait environment of an ingot, which maintain all trait
/// implementors which can be used in the ingot.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct TraitEnv {
    pub(super) impls: FxHashMap<TraitDef, Vec<Binder<Implementor>>>,
    hir_to_implementor: FxHashMap<ImplTrait, Binder<Implementor>>,

    /// This maintains a mapping from the base type to the implementors.
    ty_to_implementors: FxHashMap<TyId, Vec<Binder<Implementor>>>,

    ingot: IngotId,
}

impl TraitEnv {
    fn collect(db: &dyn HirAnalysisDb, ingot: IngotId) -> Self {
        let mut impls: FxHashMap<_, Vec<Binder<Implementor>>> = FxHashMap::default();
        let mut hir_to_implementor: FxHashMap<ImplTrait, Binder<Implementor>> =
            FxHashMap::default();
        let mut ty_to_implementors: FxHashMap<TyId, Vec<Binder<Implementor>>> =
            FxHashMap::default();

        for impl_map in ingot
            .external_ingots(db.as_hir_db())
            .iter()
            .map(|(_, external)| collect_trait_impls(db, *external))
            .chain(std::iter::once(collect_trait_impls(db, ingot)))
        {
            // `collect_trait_impls` ensures that there are no conflicting impls, so we can
            // just extend the map.
            for (trait_def, implementors) in impl_map.iter() {
                impls
                    .entry(*trait_def)
                    .or_default()
                    .extend(implementors.iter().copied());

                hir_to_implementor.extend(implementors.iter().map(|implementor| {
                    (implementor.skip_binder().hir_impl_trait(db), *implementor)
                }));

                for implementor in implementors {
                    ty_to_implementors
                        .entry(implementor.instantiate_identity().ty(db).base_ty(db))
                        .or_default()
                        .push(*implementor);
                }
            }
        }

        Self {
            impls,
            hir_to_implementor,
            ty_to_implementors,
            ingot,
        }
    }

    /// Returns all implementors of the given instantiated trait.
    pub(crate) fn impls_of_trait<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        trait_: TraitInstId,
    ) -> &'db [Binder<Implementor>] {
        impls_of_trait(db, trait_)
    }

    /// Returns the corresponding implementor of the given `impl Trait` type.
    pub(crate) fn map_impl_trait(&self, trait_ref: ImplTrait) -> Option<Binder<Implementor>> {
        self.hir_to_implementor.get(&trait_ref).copied()
    }
}

/// Represents an implementor of a trait, which can be thought of as a lowered
/// `impl Trait`.
#[salsa::interned]
pub(crate) struct Implementor {
    /// The trait that this implementor implements.
    pub(crate) trait_: TraitInstId,

    /// The type that this implementor implements the trait for.
    pub(crate) ty: TyId,

    /// The type parameters of this implementor.
    #[return_ref]
    pub(crate) params: Vec<TyId>,

    /// The original hir.
    pub(crate) hir_impl_trait: ImplTrait,
}

impl Implementor {
    /// Returns the trait definition that this implementor implements.
    pub(crate) fn trait_def(self, db: &dyn HirAnalysisDb) -> TraitDef {
        self.trait_(db).def(db)
    }

    pub(crate) fn original_params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.params(db)
    }

    /// Returns the constraints that the implementor requires when the
    /// implementation is selected.
    pub(super) fn constraints(self, db: &dyn HirAnalysisDb) -> ConstraintListId {
        collect_implementor_constraints(db, self).instantiate(db, self.params(db))
    }

    pub(super) fn methods(self, db: &dyn HirAnalysisDb) -> &BTreeMap<IdentId, FuncDef> {
        collect_implementor_methods(db, self)
    }
}

/// Returns `true` if the given two implementor conflicts.
pub(super) fn does_impl_trait_conflict(
    db: &dyn HirAnalysisDb,
    a: Binder<Implementor>,
    b: Binder<Implementor>,
) -> bool {
    let mut table = UnificationTable::new(db);
    let a = table.instantiate_with_fresh_vars(a);
    let b = table.instantiate_with_fresh_vars(b);

    table.unify(a, b).is_ok()
}

/// Represents an instantiated trait, which can be thought of as a trait
/// reference from a HIR perspective.
#[salsa::interned]
pub struct TraitInstId {
    pub def: TraitDef,
    #[return_ref]
    pub args: Vec<TyId>,
}

impl TraitInstId {
    pub fn pretty_print(self, db: &dyn HirAnalysisDb) -> String {
        let mut s = self.def(db).name(db).unwrap_or("<unknown>").to_string();

        let mut args = self.args(db).iter().map(|ty| ty.pretty_print(db));
        // Skip the first type parameter since it's the implementor type.
        args.next();

        if let Some(first) = args.next() {
            s.push('<');
            s.push_str(first);
            for arg in args {
                s.push_str(", ");
                s.push_str(arg);
            }
            s.push('>');
        }

        s
    }

    pub fn self_ty(self, db: &dyn HirAnalysisDb) -> TyId {
        self.args(db)[0]
    }

    pub(super) fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        self.def(db).ingot(db)
    }

    pub(super) fn emit_sat_diag(
        self,
        db: &dyn HirAnalysisDb,
        assumptions: AssumptionListId,
        span: DynLazySpan,
    ) -> Option<TyDiagCollection> {
        match check_trait_inst_wf(db, self, assumptions) {
            GoalSatisfiability::Satisfied => None,
            GoalSatisfiability::NotSatisfied(goal) => {
                Some(TraitConstraintDiag::trait_bound_not_satisfied(db, span, goal).into())
            }

            GoalSatisfiability::InfiniteRecursion(goal) => {
                Some(TraitConstraintDiag::infinite_bound_recursion(db, span, goal).into())
            }
        }
    }
}

/// Represents a trait definition.
#[salsa::tracked]
pub struct TraitDef {
    pub trait_: Trait,
    #[return_ref]
    pub(crate) param_set: GenericParamTypeSet,
    #[return_ref]
    pub methods: BTreeMap<IdentId, TraitMethod>,
}

impl TraitDef {
    pub fn params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.param_set(db).params(db)
    }

    pub fn self_param(self, db: &dyn HirAnalysisDb) -> TyId {
        self.param_set(db).trait_self(db).unwrap()
    }

    pub fn original_params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.param_set(db).explicit_params(db)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct TraitMethod(pub FuncDef);

impl TraitMethod {
    pub fn has_default_impl(self, db: &dyn HirAnalysisDb) -> bool {
        self.0
            .hir_func_def(db)
            .unwrap()
            .body(db.as_hir_db())
            .is_some()
    }
}

impl TraitDef {
    /// Returns the type kind that implementor type must have.
    pub(crate) fn expected_implementor_kind(self, db: &dyn HirAnalysisDb) -> &Kind {
        self.self_param(db).kind(db)
    }

    /// Returns `ingot` in which this trait is defined.
    pub(crate) fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        let hir_db = db.as_hir_db();
        self.trait_(db).top_mod(hir_db).ingot(hir_db)
    }

    pub(super) fn super_traits(self, db: &dyn HirAnalysisDb) -> &BTreeSet<Binder<TraitInstId>> {
        const _EMPTY: &BTreeSet<Binder<TraitInstId>> = &BTreeSet::new();
        collect_super_traits(db, self).as_ref().unwrap_or(_EMPTY)
    }

    fn name(self, db: &dyn HirAnalysisDb) -> Option<&str> {
        self.trait_(db)
            .name(db.as_hir_db())
            .to_opt()
            .map(|name| name.data(db.as_hir_db()).as_str())
    }
}
