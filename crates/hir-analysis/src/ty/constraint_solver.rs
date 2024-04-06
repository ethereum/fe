//! This module implements a constraint(trait bound) satisfiability solver.
//! The algorithm is based on the paper [Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/),
//! but the algorithm here is slightly extended to support multi parametrized
//! traits.

use super::{
    constraint::{compute_super_assumptions, AssumptionListId, PredicateId, PredicateListId},
    trait_def::{ingot_trait_env, TraitEnv, TraitInstId},
    ty_def::{Subst, TyId},
    unify::UnificationTable,
};
use crate::{ty::constraint::ty_constraints, HirAnalysisDb};

type Goal = PredicateId;

/// Checks if the arguments of the given type applications satisfies the
/// constraints under the given assumptions.
///
/// # Panics
/// This function panics if the given type contains free inference keys.
#[salsa::tracked]
pub(crate) fn check_ty_app_sat(
    db: &dyn HirAnalysisDb,
    ty: TyId,
    assumptions: AssumptionListId,
) -> GoalSatisfiability {
    assert!(ty.free_inference_keys(db).is_empty());

    let (_, args) = ty.decompose_ty_app(db);

    for &arg in args {
        match check_ty_app_sat(db, arg, assumptions) {
            GoalSatisfiability::Satisfied => {}
            err => return err,
        }
    }

    let constraints = ty_constraints(db, ty);

    for &goal in constraints.predicates(db) {
        match is_goal_satisfiable(db, goal, assumptions) {
            GoalSatisfiability::Satisfied => {}
            err => return err,
        }
    }

    GoalSatisfiability::Satisfied
}

/// Checks if the given argument of the given trait instantiation satisfies the
/// constraints under the given assumptions.
#[salsa::tracked]
pub(crate) fn check_trait_inst_sat(
    db: &dyn HirAnalysisDb,
    trait_inst: TraitInstId,
    assumptions: AssumptionListId,
) -> GoalSatisfiability {
    let constraints = trait_inst.constraints(db);
    for &goal in constraints.predicates(db) {
        match is_goal_satisfiable(db, goal, assumptions) {
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
        let ingot = assumptions.ingot(db);
        let env = ingot_trait_env(db, ingot);

        Self {
            db,
            env,
            goal,
            assumptions,
        }
    }

    /// The main entry point of the constraint solver, whici performs actual
    /// solving.
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
            .impls_of_trait(self.db, goal_trait)
            .iter()
            .find_map(|impl_| {
                let mut table = UnificationTable::new(self.db);
                // Generalize the implementor by lifting all type parameters to
                // free type variables.
                let (gen_impl, mut gen_param_map) = impl_.generalize(self.db, &mut table);

                // If the `impl` can matches the goal by unifying the goal type, then we can
                // obtain a subgaols which is specified by the `impl`.
                if table.unify(gen_impl.ty(self.db), goal_ty).is_ok()
                    && table.unify(gen_impl.trait_(self.db), goal_trait).is_ok()
                {
                    let mut subst = SubstComposition::compose(&mut gen_param_map, &mut table);
                    let constraints = impl_.constraints(self.db);
                    Some(constraints.apply_subst(self.db, &mut subst))
                } else {
                    None
                }
            })
        else {
            return GoalSatisfiability::NotSatisfied(self.goal);
        };

        // Checks if the all sub goals are satisfied.
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

/// A substitution that composes multiple substitutions.
struct SubstComposition<'a, 'b, S1, S2> {
    first: &'a mut S1,
    second: &'b mut S2,
}

impl<'a, 'b, S1, S2> SubstComposition<'a, 'b, S1, S2> {
    /// Creates a new `SubstComposition` from two substitutions.
    fn compose(first: &'a mut S1, second: &'b mut S2) -> Self {
        Self { first, second }
    }
}

impl<'a, 'b, S1, S2> Subst for SubstComposition<'a, 'b, S1, S2>
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
