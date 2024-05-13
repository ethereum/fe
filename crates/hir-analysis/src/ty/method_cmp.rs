use super::{
    canonical::Canonical,
    constraint::collect_func_def_constraints,
    constraint_solver::{is_goal_satisfiable, GoalSatisfiability},
    diagnostics::{ImplDiag, TyDiagCollection},
    func_def::FuncDef,
    trait_def::{TraitInstId, TraitMethod},
    ty_def::TyId,
};
use crate::HirAnalysisDb;

pub(super) fn compare_impl_method(
    db: &dyn HirAnalysisDb,
    impl_m: FuncDef,
    trait_m: TraitMethod,
    trait_inst: TraitInstId,
    sink: &mut Vec<TyDiagCollection>,
) {
    if !compare_generic_param_num(db, impl_m, trait_m.0, sink) {
        return;
    }

    if !compare_generic_param_kind(db, impl_m, trait_m.0, sink) {
        return;
    }

    if !compare_arity(db, impl_m, trait_m.0, sink) {
        return;
    }

    // Compare the argument labels, argument types, and return type of the impl
    // method with the trait method.
    let mut err = !compare_arg_label(db, impl_m, trait_m.0, sink);

    let map_to_impl: Vec<_> = trait_inst
        .args(db)
        .iter()
        .chain(impl_m.explicit_params(db).iter())
        .copied()
        .collect();
    err |= !compare_ty(db, impl_m, trait_m.0, &map_to_impl, sink);
    if err {
        return;
    }

    compare_constraints(db, impl_m, trait_m.0, &map_to_impl, sink);
}

/// Checks if the number of generic parameters of the implemented method is the
/// same as the number of generic parameters of the trait method.
/// Returns `false` if the comparison fails.
fn compare_generic_param_num(
    db: &dyn HirAnalysisDb,
    impl_m: FuncDef,
    trait_m: FuncDef,
    sink: &mut Vec<TyDiagCollection>,
) -> bool {
    let impl_params = impl_m.explicit_params(db);
    let trait_params = trait_m.explicit_params(db);

    if impl_params.len() == trait_params.len() {
        true
    } else {
        sink.push(
            ImplDiag::method_param_num_mismatch(
                impl_m.name_span(db),
                trait_params.len(),
                impl_params.len(),
            )
            .into(),
        );
        false
    }
}

/// Checks if the generic parameter kinds are the same.
/// Returns `false` if the comparison fails.
fn compare_generic_param_kind(
    db: &dyn HirAnalysisDb,
    impl_m: FuncDef,
    trait_m: FuncDef,
    sink: &mut Vec<TyDiagCollection>,
) -> bool {
    let mut err = false;
    for (idx, (&trait_m_param, &impl_m_param)) in trait_m
        .explicit_params(db)
        .iter()
        .zip(impl_m.explicit_params(db))
        .enumerate()
    {
        let trait_m_kind = trait_m_param.kind(db);
        let impl_m_kind = impl_m_param.kind(db);

        if !trait_m_kind.does_match(impl_m_kind) {
            let span = impl_m
                .hir_func_def(db)
                .unwrap()
                .lazy_span()
                .generic_params_moved()
                .param_moved(idx)
                .into();
            sink.push(ImplDiag::method_param_kind_mismatch(span, trait_m_kind, impl_m_kind).into());
            err = true;
        }
    }

    !err
}

/// Checks if the arity of the implemented method is the same as the arity of
/// the trait method.
/// Returns `false` if the comparison fails.
fn compare_arity(
    db: &dyn HirAnalysisDb,
    impl_m: FuncDef,
    trait_m: FuncDef,
    sink: &mut Vec<TyDiagCollection>,
) -> bool {
    let impl_m_arity = impl_m.arg_tys(db).len();
    let trait_m_arity = trait_m.arg_tys(db).len();

    // Checks if the arity are the same.
    if impl_m_arity == trait_m_arity {
        true
    } else {
        sink.push(
            ImplDiag::method_arg_num_mismatch(
                impl_m.param_list_span(db),
                trait_m_arity,
                impl_m_arity,
            )
            .into(),
        );
        false
    }
}

/// Checks if the argument labels of the implemented method are the same as the
/// argument labels of the trait method.
/// Returns `false` if the comparison fails.
fn compare_arg_label(
    db: &dyn HirAnalysisDb,
    impl_m: FuncDef,
    trait_m: FuncDef,
    sink: &mut Vec<TyDiagCollection>,
) -> bool {
    let hir_db = db.as_hir_db();

    let mut err = false;
    let hir_impl_m = impl_m.hir_func_def(db).unwrap();
    let hir_trait_m = trait_m.hir_func_def(db).unwrap();

    let (Some(impl_m_params), Some(trait_m_params)) = (
        hir_impl_m.params(hir_db).to_opt(),
        hir_trait_m.params(hir_db).to_opt(),
    ) else {
        return true;
    };

    for (idx, (expected_param, method_param)) in trait_m_params
        .data(hir_db)
        .iter()
        .zip(impl_m_params.data(hir_db))
        .enumerate()
    {
        let Some(expected_label) = expected_param
            .label
            .or_else(|| expected_param.name.to_opt())
        else {
            continue;
        };

        let Some(method_label) = method_param.label.or_else(|| method_param.name.to_opt()) else {
            continue;
        };

        if expected_label != method_label {
            let primary = hir_impl_m.lazy_span().params_moved().param(idx).into();
            let sub = hir_trait_m.lazy_span().params_moved().param(idx).into();

            sink.push(
                ImplDiag::method_arg_label_mismatch(db, primary, sub, expected_label, method_label)
                    .into(),
            );
            err = true;
        }
    }

    !err
}

/// Checks if the argument types and return type of the implemented method are
/// the same as the argument types and return type of the trait method.
/// Returns `false` if the comparison fails.
fn compare_ty(
    db: &dyn HirAnalysisDb,
    impl_m: FuncDef,
    trait_m: FuncDef,
    map_to_impl: &[TyId],
    sink: &mut Vec<TyDiagCollection>,
) -> bool {
    let mut err = false;
    let impl_m_arg_tys = impl_m.arg_tys(db);
    let trait_m_arg_tys = trait_m.arg_tys(db);

    for (idx, (&trait_m_ty, &impl_m_ty)) in trait_m_arg_tys.iter().zip(impl_m_arg_tys).enumerate() {
        let trait_m_ty = trait_m_ty.instantiate(db, map_to_impl);
        if trait_m_ty.has_invalid(db) {
            continue;
        }
        let impl_m_ty = impl_m_ty.instantiate_identity();
        if !impl_m_ty.has_invalid(db) && trait_m_ty != impl_m_ty {
            let span = impl_m.param_span(db, idx);
            sink.push(ImplDiag::method_arg_ty_mismatch(db, span, trait_m_ty, impl_m_ty).into());
            err = true;
        }
    }

    let impl_m_ret_ty = impl_m.ret_ty(db).instantiate_identity();
    let trait_m_ret_ty = trait_m.ret_ty(db).instantiate(db, map_to_impl);
    if !impl_m_ret_ty.has_invalid(db) && trait_m_ret_ty != impl_m_ret_ty {
        sink.push(
            ImplDiag::method_ret_type_mismatch(
                db,
                impl_m.hir_func_def(db).unwrap().lazy_span().ret_ty().into(),
                trait_m_ret_ty,
                impl_m_ret_ty,
            )
            .into(),
        );

        err = true;
    }

    !err
}

/// Checks if the method constraints are stricter than the trait constraints.
/// This check is performed by checking if the `impl_method` constraints are
/// satisfied under the assumptions that is obtained from the `expected_method`
/// constraints.
/// Returns `false` if the comparison fails.
fn compare_constraints(
    db: &dyn HirAnalysisDb,
    impl_m: FuncDef,
    trait_m: FuncDef,
    map_to_impl: &[TyId],
    sink: &mut Vec<TyDiagCollection>,
) -> bool {
    let impl_m_constraints = collect_func_def_constraints(db, impl_m, false).instantiate_identity();
    let trait_m_constraints =
        collect_func_def_constraints(db, trait_m, false).instantiate(db, map_to_impl);
    let mut unsatisfied_goals = vec![];
    for &goal in impl_m_constraints.predicates(db) {
        let goal = Canonical::new(db, goal);
        if !matches!(
            is_goal_satisfiable(db, trait_m_constraints, goal),
            GoalSatisfiability::Satisfied
        ) {
            unsatisfied_goals.push(goal.value);
        }
    }

    if unsatisfied_goals.is_empty() {
        true
    } else {
        unsatisfied_goals.sort_by_key(|goal| goal.ty(db).pretty_print(db));
        sink.push(
            ImplDiag::method_stricter_bound(db, impl_m.name_span(db), &unsatisfied_goals).into(),
        );
        false
    }
}
