//! This module contains all trait related types definitions.

use std::collections::BTreeSet;

use hir::{
    hir_def::{ImplTrait, IngotId, Trait},
    span::DynLazySpan,
};
use rustc_hash::FxHashMap;

use crate::{
    ty::{constraint::collect_implementor_constraints, trait_lower::collect_trait_impls},
    HirAnalysisDb,
};

use super::{
    constraint::{
        collect_super_traits, collect_trait_constraints, trait_inst_constraints, AssumptionListId,
        ConstraintListId,
    },
    constraint_solver::{check_trait_inst_sat, GoalSatisfiability},
    diagnostics::{TraitConstraintDiag, TyDiagCollection},
    ty_def::{Kind, Subst, TyId},
    unify::UnificationTable,
};

/// Returns [`TraitEnv`] for the given ingot.
#[salsa::tracked(return_ref)]
pub(crate) fn ingot_trait_env(db: &dyn HirAnalysisDb, ingot: IngotId) -> TraitEnv {
    TraitEnv::collect(db, ingot)
}

/// Returns all [`Implementor`]s for the given trait inst.
#[salsa::tracked(return_ref)]
pub(crate) fn trait_implementors(db: &dyn HirAnalysisDb, trait_: TraitInstId) -> Vec<Implementor> {
    let env = ingot_trait_env(db, trait_.ingot(db));
    let Some(impls) = env.impls.get(&trait_.def(db)) else {
        return vec![];
    };

    impls
        .iter()
        .filter(|impl_| {
            let mut table = UnificationTable::new(db);
            let (gen_impl, _) = impl_.generalize(db, &mut table);
            table.unify(gen_impl.trait_(db), trait_)
        })
        .cloned()
        .collect()
}

/// Represents the trait environment of an ingot, which maintain all trait
/// implementors which can be used in the ingot.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct TraitEnv {
    impls: FxHashMap<TraitDef, Vec<Implementor>>,
    hir_to_implementor: FxHashMap<ImplTrait, Implementor>,
    ingot: IngotId,
}

impl TraitEnv {
    pub(super) fn new(db: &dyn HirAnalysisDb, ingot: IngotId) -> &Self {
        ingot_trait_env(db, ingot)
    }

    fn collect(db: &dyn HirAnalysisDb, ingot: IngotId) -> Self {
        let mut impls: FxHashMap<_, Vec<Implementor>> = FxHashMap::default();
        let mut hir_to_implementor: FxHashMap<ImplTrait, Implementor> = FxHashMap::default();

        for impl_map in ingot
            .external_ingots(db.as_hir_db())
            .iter()
            .map(|(_, external)| collect_trait_impls(db, *external))
            .chain(std::iter::once(collect_trait_impls(db, ingot)))
        {
            // `collect_trait_impls` ensure that there are no conflicting impls, so we can
            // just extend the map.
            for (trait_def, implementors) in impl_map.iter() {
                impls
                    .entry(*trait_def)
                    .or_default()
                    .extend(implementors.iter().copied());

                hir_to_implementor.extend(
                    implementors
                        .iter()
                        .map(|implementor| (implementor.impl_trait(db), *implementor)),
                );
            }
        }

        Self {
            impls,
            hir_to_implementor,
            ingot,
        }
    }

    /// Returns all implementors of the given instantiated trait.
    pub(crate) fn implementors_for<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        trait_: TraitInstId,
    ) -> &'db [Implementor] {
        trait_implementors(db, trait_)
    }

    /// Returns the corresponding implementor of the given `impl Trait` type.
    pub(crate) fn map_impl_trait(&self, trait_ref: ImplTrait) -> Option<Implementor> {
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
    pub(crate) impl_trait: ImplTrait,
}

impl Implementor {
    /// Returns the trait definition that this implementor implements.
    pub(crate) fn trait_def(self, db: &dyn HirAnalysisDb) -> TraitDef {
        self.trait_(db).def(db)
    }

    /// Generalizes the implementor by replacing all type parameters with fresh
    /// type variables.
    pub(super) fn generalize(
        self,
        db: &dyn HirAnalysisDb,
        table: &mut UnificationTable,
    ) -> (Self, impl Subst) {
        let mut subst = FxHashMap::default();
        for param in self.params(db) {
            let var = table.new_var(param.kind(db));
            subst.insert(*param, var);
        }

        let hir_impl = self.impl_trait(db);
        let trait_ = self.trait_(db).apply_subst(db, &mut subst);
        let ty = self.ty(db).apply_subst(db, &mut subst);
        let params = self
            .params(db)
            .iter()
            .map(|param| subst[param])
            .collect::<Vec<_>>();

        (Implementor::new(db, trait_, ty, params, hir_impl), subst)
    }

    /// Returns the constraints that the implementor requires when the
    /// implementation is selected.
    pub(super) fn constraints(self, db: &dyn HirAnalysisDb) -> ConstraintListId {
        collect_implementor_constraints(db, self)
    }

    /// Returns true if the implementor conflicts with the other implementor.
    pub(super) fn does_conflict(
        self,
        db: &dyn HirAnalysisDb,
        other: Self,
        table: &mut UnificationTable,
    ) -> bool {
        let (generalized_self, _) = self.generalize(db, table);
        let (generalized_other, _) = other.generalize(db, table);

        table.unify(generalized_self, generalized_other)
    }
}

/// Represents an instantiated trait, which can be thought of as a trait
/// reference from a HIR perspective.
#[salsa::interned]
pub struct TraitInstId {
    pub def: TraitDef,
    #[return_ref]
    pub substs: Vec<TyId>,

    pub(super) ingot: IngotId,
}

impl TraitInstId {
    pub fn pretty_print(self, db: &dyn HirAnalysisDb) -> String {
        let mut s = self.def(db).name(db).unwrap_or("<unknown>").to_string();

        let mut args = self.substs(db).iter().map(|ty| ty.pretty_print(db));
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

    /// Apply substitutions to this trait.
    /// Which is to say, replace all type parameters with their corresponding
    /// type in the `subst`.
    pub(super) fn apply_subst<S: Subst>(
        self,
        db: &dyn HirAnalysisDb,
        subst: &mut S,
    ) -> TraitInstId {
        TraitInstId::new(
            db,
            self.def(db),
            self.substs(db)
                .iter()
                .map(|ty| ty.apply_subst(db, subst))
                .collect(),
            self.ingot(db),
        )
    }

    pub(super) fn subst_table(self, db: &dyn HirAnalysisDb) -> FxHashMap<TyId, TyId> {
        let mut table = FxHashMap::default();
        for (from, to) in self.def(db).params(db).iter().zip(self.substs(db)) {
            table.insert(*from, *to);
        }

        table
    }

    pub(super) fn constraints(
        self,
        db: &dyn HirAnalysisDb,
    ) -> (AssumptionListId, ConstraintListId) {
        trait_inst_constraints(db, self)
    }

    pub(super) fn emit_sat_diag(
        self,
        db: &dyn HirAnalysisDb,
        assumptions: AssumptionListId,
        span: DynLazySpan,
    ) -> Option<TyDiagCollection> {
        match check_trait_inst_sat(db, self, assumptions) {
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
    pub params: Vec<TyId>,
    /// We collects self type here to know the expected kind of implementor
    /// type in `Implementor` lowering phase.
    pub self_param: TyId,
    // TODO: we need to collect associated method types here.
    // methods: Vec<FuncInst>
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

    pub(super) fn super_traits(self, db: &dyn HirAnalysisDb) -> &BTreeSet<TraitInstId> {
        const _EMPTY: &BTreeSet<TraitInstId> = &BTreeSet::new();
        collect_super_traits(db, self).as_ref().unwrap_or(_EMPTY)
    }

    pub(super) fn constraints(self, db: &dyn HirAnalysisDb) -> ConstraintListId {
        collect_trait_constraints(db, self)
    }

    fn name(self, db: &dyn HirAnalysisDb) -> Option<&str> {
        self.trait_(db)
            .name(db.as_hir_db())
            .to_opt()
            .map(|name| name.data(db.as_hir_db()).as_str())
    }
}
