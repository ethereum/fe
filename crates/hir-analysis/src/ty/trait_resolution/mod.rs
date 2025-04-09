use common::indexmap::IndexSet;
use hir::hir_def::IngotId;
use salsa::Update;

use super::{
    canonical::{Canonical, Canonicalized, Solution},
    trait_def::TraitInstId,
    ty_def::{TyFlags, TyId},
};
use crate::{
    ty::{
        trait_resolution::{
            constraint::{collect_trait_constraints, ty_constraints},
            proof_forest::ProofForest,
        },
        unify::UnificationTable,
        visitor::collect_flags,
    },
    HirAnalysisDb,
};

pub(crate) mod constraint;
mod proof_forest;

#[salsa::tracked(return_ref)]
pub fn is_goal_satisfiable<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
    goal: Canonical<TraitInstId<'db>>,
    assumptions: PredicateListId<'db>,
) -> GoalSatisfiability<'db> {
    let flags = collect_flags(db, goal.value);
    if flags.contains(TyFlags::HAS_INVALID) {
        return GoalSatisfiability::ContainsInvalid;
    };

    ProofForest::new(db, ingot, goal, assumptions).solve()
}

/// Checks if the given type is well-formed, i.e., the arguments of the given
/// type applications satisfies the constraints under the given assumptions.
#[salsa::tracked]
pub(crate) fn check_ty_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
    ty: TyId<'db>,
    assumptions: PredicateListId<'db>,
) -> WellFormedness<'db> {
    let (_, args) = ty.decompose_ty_app(db);

    for &arg in args {
        let wf = check_ty_wf(db, ingot, arg, assumptions);
        if !wf.is_wf() {
            return wf;
        }
    }

    let constraints = ty_constraints(db, ty);

    for &goal in constraints.list(db) {
        let mut table = UnificationTable::new(db);
        let canonical_goal = Canonicalized::new(db, goal);

        if let GoalSatisfiability::UnSat(subgoal) =
            is_goal_satisfiable(db, ingot, canonical_goal.value, assumptions)
        {
            let subgoal =
                subgoal.map(|subgoal| canonical_goal.extract_solution(&mut table, subgoal));
            return WellFormedness::IllFormed { goal, subgoal };
        }
    }

    WellFormedness::WellFormed
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
pub(crate) enum WellFormedness<'db> {
    WellFormed,
    IllFormed {
        goal: TraitInstId<'db>,
        subgoal: Option<TraitInstId<'db>>,
    },
}

impl WellFormedness<'_> {
    fn is_wf(self) -> bool {
        matches!(self, WellFormedness::WellFormed)
    }
}

/// Checks if the given trait instance are well-formed, i.e., the arguments of
/// the trait satisfies all constraints under the given assumptions.
#[salsa::tracked]
pub(crate) fn check_trait_inst_wf<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
    trait_inst: TraitInstId<'db>,
    assumptions: PredicateListId<'db>,
) -> WellFormedness<'db> {
    let constraints =
        collect_trait_constraints(db, trait_inst.def(db)).instantiate(db, trait_inst.args(db));

    for &goal in constraints.list(db) {
        let mut table = UnificationTable::new(db);
        let canonical_goal = Canonicalized::new(db, goal);
        if let GoalSatisfiability::UnSat(subgoal) =
            is_goal_satisfiable(db, ingot, canonical_goal.value, assumptions)
        {
            let subgoal =
                subgoal.map(|subgoal| canonical_goal.extract_solution(&mut table, subgoal));
            return WellFormedness::IllFormed { goal, subgoal };
        }
    }

    WellFormedness::WellFormed
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub enum GoalSatisfiability<'db> {
    /// Goal is satisfied with the unique solution.
    Satisfied(Solution<TraitInstId<'db>>),
    /// Goal might be satisfied, but needs more type information to determine
    /// satisfiability and uniqueness.
    NeedsConfirmation(IndexSet<Solution<TraitInstId<'db>>>),

    /// Goal contains invalid.
    ContainsInvalid,
    /// The gaol is not satisfied.
    /// It contains an unsatisfied subgoal if we can know the exact subgoal
    /// that makes the proof step stuck.
    UnSat(Option<Solution<TraitInstId<'db>>>),
}

impl GoalSatisfiability<'_> {
    pub fn is_satisfied(&self) -> bool {
        matches!(
            self,
            Self::Satisfied(_) | Self::NeedsConfirmation(_) | Self::ContainsInvalid
        )
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct PredicateListId<'db> {
    #[return_ref]
    pub list: Vec<TraitInstId<'db>>,
}

impl<'db> PredicateListId<'db> {
    pub(super) fn merge(self, db: &'db dyn HirAnalysisDb, other: Self) -> Self {
        let mut predicates = self.list(db).clone();
        predicates.extend(other.list(db));
        PredicateListId::new(db, predicates)
    }

    pub fn empty_list(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new())
    }

    fn extend_by_super(self, db: &'db dyn HirAnalysisDb) -> Self {
        let mut super_traits: IndexSet<_> = self.list(db).iter().copied().collect();
        for &pred in self.list(db) {
            for &super_trait in pred.def(db).super_traits(db).iter() {
                let super_trait = super_trait.instantiate(db, pred.args(db));
                super_traits.insert(super_trait);
            }
        }

        Self::new(db, super_traits.into_iter().collect::<Vec<_>>())
    }
}
