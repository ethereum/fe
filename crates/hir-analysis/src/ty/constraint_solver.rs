use super::{
    canonical::Canonical, trait_def::TraitInstId, trait_resolution::PredicateListId, ty_def::TyId,
};
use crate::{
    ty::{
        constraint::{collect_trait_constraints, ty_constraints},
        trait_resolution::is_goal_satisfiable,
        ty_def::inference_keys,
    },
    HirAnalysisDb,
};

/// Checks if the given type is well-formed, i.e., the arguments of the given
/// type applications satisfies the constraints under the given assumptions.
///
/// # Panics
/// This function panics if the given type contains free inference keys.
#[salsa::tracked]
pub(crate) fn check_ty_wf(
    db: &dyn HirAnalysisDb,
    ty: TyId,
    assumptions: PredicateListId,
) -> Option<TraitInstId> {
    assert!(inference_keys(db, &ty).is_empty());

    let (_, args) = ty.decompose_ty_app(db);

    for &arg in args {
        if let Some(unsat_goal) = check_ty_wf(db, arg, assumptions) {
            return Some(unsat_goal);
        }
    }

    let constraints = ty_constraints(db, ty);

    for &goal in constraints.list(db) {
        let goal = Canonical::new(db, goal);
        if !is_goal_satisfiable(db, assumptions, goal).is_satisfied() {
            return Some(goal.value);
        }
    }

    None
}

/// Checks if the given trait instance are well-formed, i.e., the arguments of
/// the trait satisfies all constraints under the given assumptions.
#[salsa::tracked]
pub(crate) fn check_trait_inst_wf(
    db: &dyn HirAnalysisDb,
    trait_inst: TraitInstId,
    assumptions: PredicateListId,
) -> Option<TraitInstId> {
    let constraints =
        collect_trait_constraints(db, trait_inst.def(db)).instantiate(db, trait_inst.args(db));

    for &goal in constraints.list(db) {
        let goal = Canonical::new(db, goal);
        if !is_goal_satisfiable(db, assumptions, goal).is_satisfied() {
            return Some(goal.value);
        }
    }

    None
}
