use crate::{ty::constraint::ty_constraints, HirAnalysisDb};

use super::{
    constraint::{compute_super_assumptions, AssumptionListId, PredicateId, PredicateListId},
    trait_::{TraitEnv, TraitInstId},
    ty_def::{Subst, TyId},
    unify::UnificationTable,
};

type Goal = PredicateId;

#[salsa::tracked]
pub(crate) fn check_ty_sat(
    db: &dyn HirAnalysisDb,
    ty: TyId,
    assumptions: AssumptionListId,
) -> GoalSatisfiability {
    assert!(ty.free_inference_keys(db).is_empty());

    let (_, args) = ty.decompose_ty_app(db);

    for arg in args {
        match check_ty_sat(db, arg, assumptions) {
            GoalSatisfiability::Satisfied => {}
            err => return err,
        }
    }

    let (new_assumptions, constraints) = ty_constraints(db, ty);

    let new_assumptions = assumptions.merge(db, new_assumptions);
    for &goal in constraints.predicates(db) {
        match is_goal_satisfiable(db, goal, new_assumptions) {
            GoalSatisfiability::Satisfied => {}
            err => return err,
        }
    }

    GoalSatisfiability::Satisfied
}

#[salsa::tracked]
pub(crate) fn check_trait_inst_sat(
    db: &dyn HirAnalysisDb,
    trait_inst: TraitInstId,
    assumptions: AssumptionListId,
) -> GoalSatisfiability {
    let (new_assumptions, constraints) = trait_inst.constraints(db);
    let new_assumptions = assumptions.merge(db, new_assumptions);

    for &goal in constraints.predicates(db) {
        match is_goal_satisfiable(db, goal, new_assumptions) {
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
    goal: Goal,
    assumptions: AssumptionListId,
) -> GoalSatisfiability {
    if goal.ty(db).is_invalid(db) {
        return GoalSatisfiability::Satisfied;
    }
    ConstraintSolver::new(db, goal, assumptions).solve()
}

fn recover_is_goal_satisfiable(
    _db: &dyn HirAnalysisDb,
    _cycle: &salsa::Cycle,
    goal: Goal,
    _assumptions: AssumptionListId,
) -> GoalSatisfiability {
    GoalSatisfiability::InfiniteRecursion(goal)
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

    fn solve(self) -> GoalSatisfiability {
        let goal_ty = self.goal.ty(self.db);
        let goal_trait = self.goal.trait_inst(self.db);

        // If the goal type is already invalid, we don't need to do anything.
        if goal_ty.contains_invalid(self.db) {
            return GoalSatisfiability::Satisfied;
        }

        let super_assumptions = compute_super_assumptions(self.db, self.assumptions);
        if self.assumptions.does_satisfy(self.db, self.goal)
            || super_assumptions.does_satisfy(self.db, self.goal)
        {
            return GoalSatisfiability::Satisfied;
        }

        // Find sub goals that need to be satisfied in order to satisfy the
        // current goal.
        let Some(sub_goals) = self
            .env
            .implementors_for(self.db, goal_trait)
            .iter()
            .find_map(|impl_| {
                let mut table = UnificationTable::new(self.db);
                // Generalize the implementor by lifting all type parameters to
                // free type variables.
                let (gen_impl, mut gen_param_map) = impl_.generalize(self.db, &mut table);

                // If the `impl` can matches the goal by unifying the goal type, then we can
                // obtain a subgaols which is specified by the `impl`.
                if table.unify(gen_impl.ty(self.db), goal_ty)
                    && table.unify(gen_impl.trait_(self.db), goal_trait)
                {
                    let mut subst = ChainedSubst::chain(&mut gen_param_map, &mut table);
                    let constraints = impl_.constraints(self.db);
                    Some(constraints.apply_subst(self.db, &mut subst))
                } else {
                    None
                }
            })
        else {
            return GoalSatisfiability::NotSatisfied(self.goal);
        };

        for &sub_goal in sub_goals.predicates(self.db) {
            match is_goal_satisfiable(self.db, sub_goal, self.assumptions) {
                GoalSatisfiability::Satisfied => {}
                GoalSatisfiability::NotSatisfied(_) => {
                    return GoalSatisfiability::NotSatisfied(self.goal)
                }
                GoalSatisfiability::InfiniteRecursion(_) => {
                    return GoalSatisfiability::InfiniteRecursion(self.goal)
                }
            }
        }

        GoalSatisfiability::Satisfied
    }
}

impl PredicateListId {
    /// Returns `true` if the given predicate list satisfies the given goal.
    fn does_satisfy(self, db: &dyn HirAnalysisDb, goal: Goal) -> bool {
        self.predicates(db).contains(&goal)
    }
}

struct ChainedSubst<'a, 'b, S1, S2> {
    first: &'a mut S1,
    second: &'b mut S2,
}

impl<'a, 'b, S1, S2> ChainedSubst<'a, 'b, S1, S2> {
    fn chain(first: &'a mut S1, second: &'b mut S2) -> Self {
        Self { first, second }
    }
}

impl<'a, 'b, S1, S2> Subst for ChainedSubst<'a, 'b, S1, S2>
where
    S1: Subst,
    S2: Subst,
{
    fn get(&mut self, from: TyId) -> Option<TyId> {
        self.second.get(self.first.get(from)?)
    }

    fn apply(&mut self, db: &dyn HirAnalysisDb, ty: TyId) -> TyId {
        self.second.apply(db, self.first.apply(db, ty))
    }
}
