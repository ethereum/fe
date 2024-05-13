//! This module implements a constraint(trait bound) satisfiability solver.
//! The algorithm is based on the paper [Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/),
//! but the algorithm here is slightly extended to support multi parametrized
//! traits.

use rustc_hash::FxHashSet;

use super::{
    canonical::Canonical,
    constraint::{collect_trait_constraints, AssumptionListId, PredicateId},
    fold::TyFoldable,
    trait_def::{ingot_trait_env, TraitEnv, TraitInstId},
    ty_def::TyId,
    unify::UnificationTable,
};
use crate::{
    ty::{constraint::ty_constraints, ty_def::inference_keys},
    HirAnalysisDb,
};

type Goal = PredicateId;

/// Checks if the given type is well-formed, i.e., the arguments of the given
/// type applications satisfies the constraints under the given assumptions.
///
/// # Panics
/// This function panics if the given type contains free inference keys.
#[salsa::tracked]
pub(crate) fn check_ty_wf(
    db: &dyn HirAnalysisDb,
    ty: TyId,
    assumptions: AssumptionListId,
) -> GoalSatisfiability {
    assert!(inference_keys(db, ty).is_empty());

    let (_, args) = ty.decompose_ty_app(db);

    for &arg in args {
        match check_ty_wf(db, arg, assumptions) {
            GoalSatisfiability::Satisfied => {}
            err => return err,
        }
    }

    let constraints = ty_constraints(db, ty);

    for &goal in constraints.predicates(db) {
        let goal = Canonical::new(db, goal);
        match is_goal_satisfiable(db, assumptions, goal) {
            GoalSatisfiability::Satisfied => {}
            err => return err,
        }
    }

    GoalSatisfiability::Satisfied
}

/// Checks if the given trait instance are well-formed, i.e., the arguments of
/// the trait satisfies all constraints under the given assumptions.
#[salsa::tracked]
pub(crate) fn check_trait_inst_wf(
    db: &dyn HirAnalysisDb,
    trait_inst: TraitInstId,
    assumptions: AssumptionListId,
) -> GoalSatisfiability {
    let constraints =
        collect_trait_constraints(db, trait_inst.def(db)).instantiate(db, trait_inst.args(db));

    for &goal in constraints.predicates(db) {
        let goal = Canonical::new(db, goal);
        match is_goal_satisfiable(db, assumptions, goal) {
            GoalSatisfiability::Satisfied => {}
            err => return err,
        }
    }

    GoalSatisfiability::Satisfied
}

/// Checks if the given goal is satisfiable under the given assumptions(i.e.,
/// context of the goal).
#[salsa::tracked(recovery_fn= recover_is_goal_satisfiable)]
pub(crate) fn is_goal_satisfiable(
    db: &dyn HirAnalysisDb,
    assumptions: AssumptionListId,
    goal: Canonical<Goal>,
) -> GoalSatisfiability {
    ConstraintSolver::new(db, goal, assumptions).solve()
}

fn recover_is_goal_satisfiable(
    _db: &dyn HirAnalysisDb,
    _cycle: &salsa::Cycle,
    _assumptions: AssumptionListId,
    goal: Canonical<Goal>,
) -> GoalSatisfiability {
    GoalSatisfiability::InfiniteRecursion(goal.value)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum GoalSatisfiability {
    Satisfied,
    NotSatisfied(Goal),
    InfiniteRecursion(Goal),
}

struct ConstraintSolver<'db> {
    db: &'db dyn HirAnalysisDb,
    env: &'db TraitEnv,
    goal: Canonical<Goal>,
    assumptions: AssumptionListId,
}

impl<'db> ConstraintSolver<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        goal: Canonical<Goal>,
        assumptions: AssumptionListId,
    ) -> Self {
        let ingot = assumptions.ingot(db);
        let env = ingot_trait_env(db, ingot);

        Self {
            db,
            env,
            goal,
            assumptions,
        }
    }

    /// The main entry point of the constraint solver, which performs actual
    /// solving.
    fn solve(self) -> GoalSatisfiability {
        let mut table = UnificationTable::new(self.db);
        let goal = self.goal.extract_identity(&mut table);
        let goal_ty = goal.ty(self.db);
        let goal_trait = goal.trait_inst(self.db);

        // If the goal type is already invalid, we don't need to do anything.
        if goal_ty.has_invalid(self.db) {
            return GoalSatisfiability::Satisfied;
        }

        let derived_assumptions = self.derived_assumptions();
        if self.assumptions.predicates(self.db).contains(&goal)
            || derived_assumptions.contains(&goal)
        {
            return GoalSatisfiability::Satisfied;
        }

        // Find sub goals that need to be satisfied in order to satisfy the
        // current goal.
        let Some(sub_goals) = self
            .env
            .impls_of_trait(
                self.db,
                self.assumptions.ingot(self.db),
                Canonical::new(self.db, goal.trait_inst(self.db)),
            )
            .iter()
            .find_map(|impl_| {
                let snapshot = table.snapshot();
                let gen_impl = table.instantiate_with_fresh_vars(*impl_);

                // If the `impl` can matches the goal by unifying the goal type, then we can
                // obtain a subgoals which is specified by the `impl`.
                let res = if table.unify(gen_impl.ty(self.db), goal_ty).is_ok()
                    && table.unify(gen_impl.trait_(self.db), goal_trait).is_ok()
                {
                    let constraints = gen_impl.fold_with(&mut table).constraints(self.db);
                    Some(constraints.fold_with(&mut table))
                } else {
                    None
                };

                table.rollback_to(snapshot);

                res
            })
        else {
            return GoalSatisfiability::NotSatisfied(goal);
        };

        // Checks if the all sub goals are satisfied.
        for &sub_goal in sub_goals.predicates(self.db) {
            let sub_goal = Canonical::new(self.db, sub_goal);
            match is_goal_satisfiable(self.db, self.assumptions, sub_goal) {
                GoalSatisfiability::Satisfied => {}
                GoalSatisfiability::NotSatisfied(_) => {
                    return GoalSatisfiability::NotSatisfied(goal);
                }
                GoalSatisfiability::InfiniteRecursion(_) => {
                    return GoalSatisfiability::InfiniteRecursion(goal)
                }
            }
        }

        GoalSatisfiability::Satisfied
    }

    /// Returns the assumptions that are derived by sub trait relations.
    fn derived_assumptions(&self) -> FxHashSet<PredicateId> {
        let mut assumptions = FxHashSet::default();
        for &pred in self.assumptions.predicates(self.db) {
            let trait_ = pred.trait_inst(self.db);
            let ty = pred.ty(self.db);

            for &super_trait in trait_.def(self.db).super_traits(self.db).iter() {
                let super_trait = super_trait.instantiate(self.db, trait_.args(self.db));
                let super_pred = PredicateId::new(self.db, ty, super_trait);
                assumptions.insert(super_pred);
            }
        }

        assumptions
    }
}
