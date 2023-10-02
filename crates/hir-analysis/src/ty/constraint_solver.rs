use std::collections::BTreeMap;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{ty::constraint::super_trait_insts, HirAnalysisDb};

use super::{
    constraint::{compute_super_assumptions, AssumptionListId, PredicateId, PredicateListId},
    trait_::{TraitEnv, TraitInstId},
    ty_def::TyId,
    unify::UnificationTable,
};

type Goal = PredicateId;

#[salsa::tracked(recovery_fn= recover_is_goal_satisfiable)]
pub(crate) fn is_goal_satisfiable(
    db: &dyn HirAnalysisDb,
    goal: Goal,
    assumptions: AssumptionListId,
) -> GoalSatisfiability {
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
        let ingot = goal.trait_(db).ingot(db);
        let env = TraitEnv::new(db, ingot);

        let mut assumptions_by_super: FxHashMap<_, FxHashSet<_>> = FxHashMap::default();
        for (ty, insts) in assumptions.predicates(db) {
            let super_insts = insts
                .iter()
                .flat_map(|inst| super_trait_insts(db, *inst).iter().copied());
            assumptions_by_super.insert(ty, super_insts.collect());
        }

        Self {
            db,
            env,
            goal,
            assumptions,
        }
    }

    fn solve(mut self) -> GoalSatisfiability {
        let goal_ty = self.goal.ty(self.db);
        let goal_trait = self.goal.trait_(self.db);

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
                    let spec_impl = gen_impl.apply_subst(self.db, &mut table);
                    Some(spec_impl.constraints(self.db))
                } else {
                    None
                }
            })
        else {
            return GoalSatisfiability::NonSatisfied;
        };

        for (ty, insts) in sub_goals.predicates(self.db).iter() {
            for inst in insts {
                let sub_goal = PredicateId::new(self.db, *ty, *inst);
                match is_goal_satisfiable(self.db, sub_goal, self.assumptions) {
                    GoalSatisfiability::Satisfied => {}
                    failed => return failed,
                }
            }
        }

        GoalSatisfiability::Satisfied
    }
}

impl PredicateListId {
    /// Returns `true` if the given predicate list satisfies the given goal.
    fn does_satisfy(self, db: &dyn HirAnalysisDb, goal: Goal) -> bool {
        let trait_ = goal.trait_(db);
        let ty = goal.ty(db);

        let Some(insts) = self.predicates(db).get(&ty) else {
            return false;
        };

        insts.contains(&trait_)
    }
}
