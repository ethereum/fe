use std::collections::BTreeMap;

use hir::visitor::prelude::LazyTySpan;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    ty::{constraint::super_trait_insts, visitor::TyVisitor},
    HirAnalysisDb,
};

use super::{
    constraint::{compute_super_assumptions, AssumptionListId, PredicateId, PredicateListId},
    diagnostics::TraitConstraintDiag,
    trait_::{TraitEnv, TraitInstId},
    ty_def::TyId,
    unify::UnificationTable,
};

type Goal = PredicateId;

/// Checks if the given goal is satisfiable under the given assumptions(i.e.,
/// context of the goal).
#[salsa::tracked(recovery_fn= recover_is_goal_satisfiable)]
pub(crate) fn is_goal_satisfiable(
    db: &dyn HirAnalysisDb,
    goal: Goal,
    assumptions: AssumptionListId,
) -> GoalSatisfiability {
    if goal.ty(db).is_invalid(db) {
        return GoalSatisfiability::Satisfied;
    }
    ConstraintSolver::new(db, goal, assumptions).solve()
}

fn recover_is_goal_satisfiable(
    db: &dyn HirAnalysisDb,
    _cycle: &salsa::Cycle,
    goal: Goal,
    assumptions: AssumptionListId,
) -> GoalSatisfiability {
    GoalSatisfiability::InfiniteRecursion
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum GoalSatisfiability {
    Satisfied,
    NonSatisfied,
    InfiniteRecursion,
}

struct ConstraintSolver<'db> {
    db: &'db dyn HirAnalysisDb,
    env: &'db TraitEnv,
    goal: Goal,
    assumptions: AssumptionListId,
}

impl<'db> ConstraintSolver<'db> {
    fn new(db: &'db dyn HirAnalysisDb, goal: Goal, assumptions: AssumptionListId) -> Self {
        let ingot = goal.trait_inst(db).ingot(db);
        let env = TraitEnv::new(db, ingot);

        Self {
            db,
            env,
            goal,
            assumptions,
        }
    }

    fn solve(mut self) -> GoalSatisfiability {
        let goal_ty = self.goal.ty(self.db);
        let goal_trait = self.goal.trait_inst(self.db);

        let super_assumptions = compute_super_assumptions(self.db, self.assumptions);
        if self.assumptions.does_satisfy(self.db, self.goal)
            || super_assumptions.does_satisfy(self.db, self.goal)
        {
            return GoalSatisfiability::Satisfied;
        }

        /// Find sub goals that need to be satisfied in order to satisfy the
        /// current goal.
        let Some(sub_goals) = self
            .env
            .implementors_for(self.db, goal_trait)
            .iter()
            .find_map(|impl_| {
                let mut table = UnificationTable::new(self.db);
                /// Generalize the implementor by lifting all type parameters to
                /// free type variables.
                let gen_impl = impl_.generalize(self.db, &mut table);

                if table.unify(gen_impl.ty(self.db), goal_ty)
                    && table.unify(gen_impl.trait_(self.db), goal_trait)
                {
                    // Specialize the implementor and obtains constraints for it.
                    let spec_impl = gen_impl.apply_subst(self.db, &mut table);
                    Some(spec_impl.constraints(self.db))
                } else {
                    None
                }
            })
        else {
            return GoalSatisfiability::NonSatisfied;
        };

        for &sub_goal in sub_goals.predicates(self.db) {
            match is_goal_satisfiable(self.db, sub_goal, self.assumptions) {
                GoalSatisfiability::Satisfied => {}
                failed => return failed,
            }
        }

        GoalSatisfiability::Satisfied
    }
}

impl PredicateListId {
    /// Returns `true` if the given predicate list satisfies the given goal.
    fn does_satisfy(self, db: &dyn HirAnalysisDb, goal: Goal) -> bool {
        let trait_ = goal.trait_inst(db);
        let ty = goal.ty(db);
        self.predicates(db).contains(&goal)
    }
}
