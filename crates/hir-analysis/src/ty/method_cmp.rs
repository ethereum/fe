use common::indexmap::IndexMap;
use hir::hir_def::IdentId;
use thin_vec::ThinVec;

use super::{
    binder::Binder,
    canonical::Canonical,
    const_ty::ConstTyData,
    diagnostics::{ImplDiag, TyDiagCollection},
    fold::{TyFoldable, TyFolder},
    func_def::FuncDef,
    normalize::normalize_ty,
    trait_def::{impls_for_ty, TraitInstId, TraitMethod},
    trait_resolution::{
        constraint::collect_func_def_constraints, is_goal_satisfiable, GoalSatisfiability,
    },
    ty_def::{TyData, TyId},
    unify::UnificationTable,
};
use crate::HirAnalysisDb;

/// Compares the implementation method with the trait method to ensure they
/// match.
///
/// This function performs the following checks:
///
/// 1. Number of generic parameters.
/// 2. Kinds of generic parameters.
/// 3. Arity (number of arguments).
/// 4. Argument labels.
/// 5. Argument types and return type.
/// 6. Method constraints.
///
/// If any of these checks fail, the function will record the appropriate
/// diagnostics.
///
/// # Arguments
///
/// * `db` - Reference to the database implementing the `HirAnalysisDb` trait.
/// * `impl_m` - The implementation method to compare.
/// * `trait_m` - The trait method to compare against.
/// * `trait_inst` - The instance of the trait being checked.
/// * `implementor` - The implementor that contains associated type bindings.
/// * `sink` - A mutable reference to a vector where diagnostic messages will be
///   collected.
pub(super) fn compare_impl_method<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: FuncDef<'db>,
    trait_m: TraitMethod<'db>,
    trait_inst: TraitInstId<'db>,
    implementor: super::trait_def::Implementor<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
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
    err |= !compare_ty(
        db,
        impl_m,
        trait_m.0,
        &map_to_impl,
        implementor.types(db),
        implementor,
        sink,
    );
    if err {
        return;
    }

    compare_constraints(db, impl_m, trait_m.0, &map_to_impl, sink);
}

/// Checks if the number of generic parameters of the implemented method is the
/// same as the number of generic parameters of the trait method.
/// Returns `false` if the comparison fails.
fn compare_generic_param_num<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: FuncDef<'db>,
    trait_m: FuncDef<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let impl_params = impl_m.explicit_params(db);
    let trait_params = trait_m.explicit_params(db);

    if impl_params.len() == trait_params.len() {
        true
    } else {
        sink.push(ImplDiag::MethodTypeParamNumMismatch { trait_m, impl_m }.into());
        false
    }
}

/// Checks if the generic parameter kinds are the same.
/// Returns `false` if the comparison fails.
fn compare_generic_param_kind<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: FuncDef<'db>,
    trait_m: FuncDef<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
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
            sink.push(
                ImplDiag::MethodTypeParamKindMismatch {
                    trait_m,
                    impl_m,
                    param_idx: idx,
                }
                .into(),
            );
            err = true;
        }
    }

    !err
}

/// Checks if the arity of the implemented method is the same as the arity of
/// the trait method.
/// Returns `false` if the comparison fails.
fn compare_arity<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: FuncDef<'db>,
    trait_m: FuncDef<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let impl_m_arity = impl_m.arg_tys(db).len();
    let trait_m_arity = trait_m.arg_tys(db).len();

    // Checks if the arity are the same.
    if impl_m_arity == trait_m_arity {
        true
    } else {
        sink.push(ImplDiag::MethodArgNumMismatch { impl_m, trait_m }.into());
        false
    }
}

/// Checks if the argument labels of the implemented method are the same as the
/// argument labels of the trait method.
/// Returns `false` if the comparison fails.
fn compare_arg_label<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: FuncDef<'db>,
    trait_m: FuncDef<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let mut err = false;
    let hir_impl_m = impl_m.hir_func_def(db).unwrap();
    let hir_trait_m = trait_m.hir_func_def(db).unwrap();

    let (Some(impl_m_params), Some(trait_m_params)) = (
        hir_impl_m.params(db).to_opt(),
        hir_trait_m.params(db).to_opt(),
    ) else {
        return true;
    };

    for (idx, (expected_param, method_param)) in trait_m_params
        .data(db)
        .iter()
        .zip(impl_m_params.data(db))
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
            sink.push(
                ImplDiag::MethodArgLabelMismatch {
                    trait_m,
                    impl_m,
                    param_idx: idx,
                }
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
fn compare_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: FuncDef<'db>,
    trait_m: FuncDef<'db>,
    map_to_impl: &[TyId<'db>],
    assoc_type_bindings: &IndexMap<IdentId<'db>, TyId<'db>>,
    implementor: super::trait_def::Implementor<'db>,
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let mut err = false;
    let impl_m_arg_tys = impl_m.arg_tys(db);
    let trait_m_arg_tys = trait_m.arg_tys(db);

    for (idx, (&trait_m_ty, &impl_m_ty)) in trait_m_arg_tys.iter().zip(impl_m_arg_tys).enumerate() {
        let trait_m_ty =
            instantiate_with_assoc_types(db, trait_m_ty, map_to_impl, assoc_type_bindings);
        if trait_m_ty.has_invalid(db) {
            continue;
        }
        let impl_m_ty = impl_m_ty.instantiate_identity();

        // Normalize both types to resolve any associated types
        let trait_m_ty_normalized = {
            let assumptions =
                collect_func_def_constraints(db, trait_m.hir_def(db), true).instantiate_identity();
            normalize_ty(db, trait_m_ty, impl_m.scope(db), assumptions)
        };
        let impl_m_ty_normalized = {
            let assumptions =
                collect_func_def_constraints(db, impl_m.hir_def(db), true).instantiate_identity();
            normalize_ty(db, impl_m_ty, impl_m.scope(db), assumptions)
        };
        if !impl_m_ty.has_invalid(db)
            && !types_match_with_assoc_resolution(
                db,
                trait_m_ty_normalized,
                impl_m_ty_normalized,
                impl_m,
                implementor,
            )
        {
            sink.push(
                ImplDiag::MethodArgTyMismatch {
                    trait_m,
                    impl_m,
                    trait_m_ty,
                    impl_m_ty,
                    param_idx: idx,
                }
                .into(),
            );
            err = true;
        }
    }

    let impl_m_ret_ty = impl_m.ret_ty(db).instantiate_identity();
    let trait_m_ret_ty =
        instantiate_with_assoc_types(db, trait_m.ret_ty(db), map_to_impl, assoc_type_bindings);

    // Normalize return types
    let trait_m_ret_ty_normalized = {
        let assumptions =
            collect_func_def_constraints(db, trait_m.hir_def(db), true).instantiate_identity();
        normalize_ty(db, trait_m_ret_ty, impl_m.scope(db), assumptions)
    };
    let impl_m_ret_ty_normalized = {
        let assumptions =
            collect_func_def_constraints(db, impl_m.hir_def(db), true).instantiate_identity();
        normalize_ty(db, impl_m_ret_ty, impl_m.scope(db), assumptions)
    };

    if !impl_m_ret_ty.has_invalid(db)
        && !trait_m_ret_ty.has_invalid(db)
        && !types_match_with_assoc_resolution(
            db,
            trait_m_ret_ty_normalized,
            impl_m_ret_ty_normalized,
            impl_m,
            implementor,
        )
    {
        sink.push(
            ImplDiag::MethodRetTyMismatch {
                trait_m,
                impl_m,
                trait_ty: trait_m_ret_ty,
                impl_ty: impl_m_ret_ty,
            }
            .into(),
        );

        err = true;
    }

    !err
}

/// Compares two types, handling associated type resolution when the expected type
/// is still an unresolved associated type after instantiation.
fn types_match_with_assoc_resolution<'db>(
    db: &'db dyn HirAnalysisDb,
    expected_ty: TyId<'db>,
    actual_ty: TyId<'db>,
    impl_m: FuncDef<'db>,
    implementor: super::trait_def::Implementor<'db>,
) -> bool {
    // Direct equality check first
    if expected_ty == actual_ty {
        return true;
    }

    // Handle unresolved associated types
    match expected_ty.data(db) {
        TyData::AssocTy(expected_assoc) => {
            let trait_def = expected_assoc.trait_.def(db);

            // First check if this is from the trait we're implementing
            if trait_def == implementor.trait_def(db) {
                // Directly look up the binding from the implementor we already have
                if let Some(&assoc_ty_value) = implementor.types(db).get(&expected_assoc.name) {
                    // The associated type value might itself need substitution
                    // For example, if the associated type is defined as `type Output = T`
                    // we need to substitute T with the actual type parameter
                    return assoc_ty_value == actual_ty;
                }
            }

            // Fall back to searching for other implementations
            // (needed for associated types from other traits)
            let self_ty = expected_assoc.trait_.self_ty(db);
            let ingot = impl_m.ingot(db);
            let implementors = impls_for_ty(db, ingot, Canonical::new(db, self_ty));

            for implementor in implementors {
                let impl_inst = implementor.skip_binder();
                if impl_inst.trait_def(db) == trait_def {
                    if let Some(&assoc_ty_value) = impl_inst.types(db).get(&expected_assoc.name) {
                        if assoc_ty_value == actual_ty {
                            return true;
                        }
                    }
                }
            }
            false
        }
        _ => false, // Types don't match and expected is not an associated type
    }
}

/// Checks if the method constraints are stricter than the trait constraints.
/// This check is performed by checking if the `impl_method` constraints are
/// satisfied under the assumptions that is obtained from the `expected_method`
/// constraints.
/// Returns `false` if the comparison fails.
fn compare_constraints<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_m: FuncDef<'db>,
    trait_m: FuncDef<'db>,
    map_to_impl: &[TyId<'db>],
    sink: &mut Vec<TyDiagCollection<'db>>,
) -> bool {
    let impl_m_constraints =
        collect_func_def_constraints(db, impl_m.hir_def(db), false).instantiate_identity();
    let trait_m_constraints =
        collect_func_def_constraints(db, trait_m.hir_def(db), false).instantiate(db, map_to_impl);
    let mut unsatisfied_goals = ThinVec::new();
    for &goal in impl_m_constraints.list(db) {
        let canonical_goal = Canonical::new(db, goal);
        let ingot = trait_m.ingot(db);
        match is_goal_satisfiable(db, ingot, canonical_goal, trait_m_constraints) {
            GoalSatisfiability::Satisfied(_) | GoalSatisfiability::ContainsInvalid => {}
            GoalSatisfiability::NeedsConfirmation(_) => unreachable!(),
            GoalSatisfiability::UnSat(_) => {
                unsatisfied_goals.push(goal);
            }
        }
    }

    if unsatisfied_goals.is_empty() {
        true
    } else {
        sink.push(
            ImplDiag::MethodStricterBound {
                span: impl_m.name_span(db),
                stricter_bounds: unsatisfied_goals,
            }
            .into(),
        );
        false
    }
}

/// Instantiates a type with both type parameters and associated type bindings.
fn instantiate_with_assoc_types<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Binder<TyId<'db>>,
    map_to_impl: &[TyId<'db>],
    assoc_type_bindings: &IndexMap<IdentId<'db>, TyId<'db>>,
) -> TyId<'db> {
    struct InstantiateWithAssocFolder<'db, 'a> {
        db: &'db dyn HirAnalysisDb,
        args: &'a [TyId<'db>],
        assoc_type_bindings: &'a IndexMap<IdentId<'db>, TyId<'db>>,
    }

    impl<'db> TyFolder<'db> for InstantiateWithAssocFolder<'db, '_> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn fold_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
            match ty.data(self.db) {
                TyData::TyParam(param) => {
                    return self.args[param.idx];
                }
                TyData::AssocTy(assoc_ty) => {
                    // First check if we have a direct binding
                    if let Some(&bound_ty) = self.assoc_type_bindings.get(&assoc_ty.name) {
                        return bound_ty;
                    }

                    // If the trait's self type is a type parameter that we're substituting,
                    // we need to first substitute the trait instance's type parameters
                    let trait_inst = assoc_ty.trait_;
                    let mut new_args = vec![];
                    for &arg in trait_inst.args(self.db) {
                        new_args.push(self.fold_ty(arg));
                    }

                    // If the args changed, create a new trait instance
                    if &new_args != trait_inst.args(self.db) {
                        let new_trait_inst = super::trait_def::TraitInstId::new(
                            self.db,
                            trait_inst.def(self.db),
                            new_args,
                            trait_inst.assoc_type_bindings(self.db).clone(),
                        );

                        // If the self type is now concrete (not a type parameter),
                        // try to resolve the associated type from implementations
                        let self_ty = new_trait_inst.self_ty(self.db);
                        if !matches!(self_ty.data(self.db), TyData::TyParam(_)) {
                            // Look for implementations of this trait for the concrete type
                            let ingot = new_trait_inst.ingot(self.db);
                            let canonical_self_ty = Canonical::new(self.db, self_ty);
                            let implementors = impls_for_ty(self.db, ingot, canonical_self_ty);

                            // We need to properly map type parameters from the implementation
                            // to our concrete type arguments
                            let mut table = UnificationTable::new(self.db);
                            let extracted_self_ty = canonical_self_ty.extract_identity(&mut table);

                            for implementor in implementors {
                                let snapshot = table.snapshot();
                                let impl_inst = table.instantiate_with_fresh_vars(*implementor);

                                if impl_inst.trait_def(self.db) == new_trait_inst.def(self.db) {
                                    // Unify to establish type parameter mappings
                                    if table
                                        .unify(extracted_self_ty, impl_inst.self_ty(self.db))
                                        .is_ok()
                                    {
                                        if let Some(&assoc_ty_value) =
                                            impl_inst.types(self.db).get(&assoc_ty.name)
                                        {
                                            // Apply the unification substitutions to the associated type value
                                            let substituted = assoc_ty_value.fold_with(&mut table);
                                            return substituted;
                                        }
                                    }
                                }
                                table.rollback_to(snapshot);
                            }
                        }

                        // Create a new AssocTy with the updated trait instance
                        let new_assoc_ty = super::ty_def::AssocTy {
                            trait_: new_trait_inst,
                            name: assoc_ty.name,
                        };
                        return TyId::new(self.db, TyData::AssocTy(new_assoc_ty));
                    }
                }
                TyData::ConstTy(const_ty) => {
                    if let ConstTyData::TyParam(param, _) = const_ty.data(self.db) {
                        return self.args[param.idx];
                    }
                }
                _ => {}
            }
            ty.super_fold_with(self)
        }
    }

    let mut folder = InstantiateWithAssocFolder {
        db,
        args: map_to_impl,
        assoc_type_bindings,
    };
    ty.instantiate_identity().fold_with(&mut folder)
}
