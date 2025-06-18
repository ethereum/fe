//! This module implements the trait and impl trait lowering process.

use common::indexmap::IndexMap;
use hir::hir_def::{
    params::GenericArg, scope_graph::ScopeId, AssocTypeGenericArg, IdentId, ImplTrait, IngotId,
    Partial, Trait, TraitRefId, TypeId,
};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    const_ty::ConstTyId,
    func_def::FuncDef,
    trait_def::{does_impl_trait_conflict, Implementor, TraitDef, TraitInstId},
    trait_resolution::PredicateListId,
    ty_def::{InvalidCause, Kind, TyId},
    ty_lower::{collect_generic_params, lower_hir_ty},
};
use crate::{
    name_resolution::{resolve_path, PathRes, PathResError},
    ty::{const_ty::ConstTyData, func_def::lower_func, ty_def::TyData, ty_lower::lower_opt_hir_ty},
    HirAnalysisDb,
};

type TraitImplTable<'db> = FxHashMap<TraitDef<'db>, Vec<Binder<Implementor<'db>>>>;

#[salsa::tracked]
pub(crate) fn lower_trait<'db>(db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> TraitDef<'db> {
    TraitDef::new(db, trait_)
}

/// Collect all trait implementors in the ingot.
/// The returned table doesn't contain the const(external) ingot
/// implementors. If you need to obtain the environment that contains all
/// available implementors in the ingot, please use
/// [`TraitEnv`](super::trait_def::TraitEnv).
#[salsa::tracked(return_ref)]
pub(crate) fn collect_trait_impls<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: IngotId<'db>,
) -> TraitImplTable<'db> {
    let const_impls = ingot
        .external_ingots(db)
        .iter()
        .map(|(_, external)| collect_trait_impls(db, *external))
        .collect();

    let impl_traits = ingot.all_impl_traits(db);
    ImplementorCollector::new(db, const_impls).collect(impl_traits)
}

/// Returns the corresponding implementors for the given [`ImplTrait`].
/// If the implementor type or the trait reference is ill-formed, returns
/// `None`.
#[salsa::tracked]
pub(crate) fn lower_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<Binder<Implementor<'db>>> {
    let scope = impl_trait.scope();

    let assumptions = PredicateListId::empty_list(db);

    let hir_ty = impl_trait.ty(db).to_opt()?;
    let ty = lower_hir_ty(db, hir_ty, scope, assumptions);
    if ty.has_invalid(db) {
        return None;
    }

    let trait_ = lower_trait_ref(
        db,
        ty,
        impl_trait.trait_ref(db).to_opt()?,
        impl_trait.scope(),
        assumptions,
    )
    .ok()?;

    let impl_trait_ingot = impl_trait.top_mod(db).ingot(db);

    if Some(impl_trait_ingot) != ty.ingot(db) && impl_trait_ingot != trait_.def(db).ingot(db) {
        return None;
    }

    let params = collect_generic_params(db, impl_trait.into())
        .params(db)
        .to_vec();

    let mut types: IndexMap<_, _> = impl_trait
        .types(db)
        .iter()
        .filter_map(|t| match (t.name.to_opt(), t.ty.to_opt()) {
            (Some(name), Some(ty)) => Some((
                name,
                lower_hir_ty(db, ty, scope, PredicateListId::empty_list(db)),
            )),
            _ => None,
        })
        .collect();

    for t in trait_.def(db).trait_(db).types(db).iter() {
        let (Some(name), Some(default)) = (t.name.to_opt(), t.default) else {
            continue;
        };
        types
            .entry(name)
            .or_insert_with(|| lower_hir_ty(db, default, scope, PredicateListId::empty_list(db)));
    }
    let implementor = Implementor::new(db, trait_, params, types, impl_trait);

    Some(Binder::bind(implementor))
}

/// Lower a trait reference to a trait instance.
#[salsa::tracked]
pub(crate) fn lower_trait_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    self_ty: TyId<'db>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    let Partial::Present(path) = trait_ref.path(db) else {
        // Path is syntactically absent, should be caught by parser
        return Err(TraitRefLowerError::Other);
    };

    let trait_def = match resolve_path(db, path, scope, assumptions, false) {
        Ok(PathRes::Trait(t)) => t.def(db),
        Ok(res) => return Err(TraitRefLowerError::InvalidDomain(res)),
        Err(e) => return Err(TraitRefLowerError::PathResError(e)),
    };

    let Some(args) = trait_ref.generic_args(db) else {
        // No generic args provided, but we need to check if the trait expects any
        let trait_params: &[TyId<'db>] = trait_def.params(db);
        let expected = trait_params
            .iter()
            .skip(1)
            .take_while(|param| match param.data(db) {
                TyData::TyParam(p) if p.is_normal() => true,
                TyData::ConstTy(_) => true,
                _ => false,
            })
            .count();

        if expected > 0 {
            return Err(TraitRefLowerError::ArgNumMismatch { expected, given: 0 });
        }

        return Ok(TraitInstId::new(
            db,
            trait_def,
            vec![self_ty],
            IndexMap::new(),
        ));
    };
    let args = args.data(db);

    // trait_params is [Self, ExplicitTypeParam1, ..., ExplicitConstParamN, ..., AssocTypeParam1, ...]
    let trait_params: &[TyId<'db>] = trait_def.params(db);
    let mut final_args: Vec<TyId<'db>> = Vec::with_capacity(trait_params.len());

    let mut args_iter = args
        .iter()
        .filter(|arg| matches!(arg, GenericArg::Type(_) | GenericArg::Const(_)));

    // 1. Add Self type
    final_args.push(self_ty);

    let mut expected = 0;
    let mut given = 0;

    // 2. Process explicit generic parameters (type and const)
    for param in trait_params.iter().skip(1) {
        match param.data(db) {
            TyData::TyParam(p) if p.is_normal() => {}
            TyData::ConstTy(_) => {}
            _ => break,
        };
        expected += 1;

        if let Some(user_arg) = args_iter.next() {
            given += 1;
            let lowered_user_arg = match user_arg {
                GenericArg::Type(ty_arg) => {
                    lower_opt_hir_ty(db, scope, ty_arg.ty, PredicateListId::empty_list(db))
                }
                GenericArg::Const(const_arg) => {
                    let const_ty_id = ConstTyId::from_opt_body(db, const_arg.body);
                    TyId::const_ty(db, const_ty_id)
                }
                _ => unreachable!(),
            };
            final_args.push(lowered_user_arg);
        } else {
            // Not enough explicit generic arguments provided.
            return Err(TraitRefLowerError::ArgNumMismatch { expected, given });
        }
    }

    // Check if there were more explicit arguments provided by user than expected
    if args_iter.next().is_some() {
        let given = args
            .iter()
            .filter(|arg| matches!(arg, GenericArg::Type(_) | GenericArg::Const(_)))
            .count();
        return Err(TraitRefLowerError::ArgNumMismatch { expected, given });
    }

    // 3. Perform kind checking and const type evaluation for regular arguments (Self + explicit params)
    // Skip Self (index 0)
    for i in 1..final_args.len() {
        let expected_param_ty = trait_params[i];
        let actual_arg_ty = &mut final_args[i]; // Get a mutable reference to update

        // Kind Check
        if !expected_param_ty
            .kind(db)
            .does_match(actual_arg_ty.kind(db))
        {
            return Err(TraitRefLowerError::ArgKindMisMatch {
                expected: expected_param_ty.kind(db).clone(),
                given: *actual_arg_ty, // Use the value before potential modification
            });
        }

        // Const Type Evaluation/Check (if expected_param_ty is a const generic parameter)
        let expected_const_value_type = match expected_param_ty.data(db) {
            TyData::ConstTy(cty_id) => match cty_id.data(db) {
                // This is the type *of* the const generic value, e.g., u32 for `const N: u32`
                ConstTyData::TyParam(_, ty_of_const_val) => Some(*ty_of_const_val),
                _ => None,
            },
            _ => None,
        };

        match actual_arg_ty.evaluate_const_ty(db, expected_const_value_type) {
            Ok(evaluated_ty) => {
                *actual_arg_ty = evaluated_ty; // Update in place
            }
            Err(cause) => {
                let err_kind = match cause {
                    InvalidCause::ConstTyMismatch { expected, given } => {
                        TraitRefLowerError::ArgTypeMismatch {
                            expected: Some(expected),
                            given: Some(given),
                        }
                    }
                    InvalidCause::ConstTyExpected { expected } => {
                        TraitRefLowerError::ArgTypeMismatch {
                            expected: Some(expected),
                            given: None,
                        }
                    }
                    InvalidCause::NormalTypeExpected { given } => {
                        TraitRefLowerError::ArgTypeMismatch {
                            expected: None,
                            given: Some(given),
                        }
                    }
                    _ => TraitRefLowerError::Other,
                };
                return Err(err_kind);
            }
        }
    }

    // 4. Process associated type parameters
    let user_assoc_type_bindings: FxHashMap<IdentId<'db>, Partial<TypeId<'db>>> = args
        .iter()
        .filter_map(|arg| match arg {
            GenericArg::AssocType(AssocTypeGenericArg { name, ty }) if name.is_present() => {
                Some((*name.unwrap(), *ty))
            }
            _ => None,
        })
        .collect();

    let mut assoc_bindings = IndexMap::new();
    // Process associated types from the trait definition
    let associated_types = trait_def.trait_(db).types(db);
    for trait_type in associated_types.iter() {
        if let Some(assoc_name) = trait_type.name.to_opt() {
            if let Some(hir_assoc_ty_val) = user_assoc_type_bindings.get(&assoc_name) {
                // User provided an explicit binding for this associated type
                let bound_ty = lower_opt_hir_ty(
                    db,
                    scope,
                    *hir_assoc_ty_val,
                    PredicateListId::empty_list(db),
                );
                assoc_bindings.insert(assoc_name, bound_ty);
            }
        }
    }

    Ok(TraitInstId::new(db, trait_def, final_args, assoc_bindings))
}

#[salsa::tracked(return_ref)]
pub(crate) fn collect_implementor_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: Implementor<'db>,
) -> IndexMap<IdentId<'db>, FuncDef<'db>> {
    let mut methods = IndexMap::default();

    for method in implementor.hir_impl_trait(db).methods(db) {
        if let Some(func) = lower_func(db, method) {
            methods.insert(func.name(db), func);
        }
    }

    methods
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub(crate) enum TraitRefLowerError<'db> {
    /// The number of arguments doesn't match the number of parameters.
    ArgNumMismatch {
        expected: usize,
        given: usize,
    },

    /// The kind of the argument doesn't match the kind of the parameter of the
    /// trait.
    ArgKindMisMatch {
        expected: Kind,
        given: TyId<'db>,
    },

    /// The argument type doesn't match the const parameter type.
    ArgTypeMismatch {
        expected: Option<TyId<'db>>,
        given: Option<TyId<'db>>,
    },

    PathResError(PathResError<'db>),

    InvalidDomain(PathRes<'db>),

    /// Other errors, which is reported by another pass. So we don't need to
    /// report this error kind.
    Other,
}

/// Collect all implementors in an ingot.
struct ImplementorCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    impl_table: TraitImplTable<'db>,
    const_impl_maps: Vec<&'db TraitImplTable<'db>>,
}

impl<'db> ImplementorCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, const_impl_maps: Vec<&'db TraitImplTable>) -> Self {
        Self {
            db,
            impl_table: TraitImplTable::default(),
            const_impl_maps,
        }
    }

    fn collect(mut self, impl_traits: &[ImplTrait<'db>]) -> TraitImplTable<'db> {
        for &impl_ in impl_traits {
            let Some(implementor) = lower_impl_trait(self.db, impl_) else {
                continue;
            };

            if !self.does_conflict(implementor) {
                self.impl_table
                    .entry(implementor.instantiate_identity().trait_def(self.db))
                    .or_default()
                    .push(implementor);
            }
        }

        self.impl_table
    }

    /// Returns `true` if `implementor` conflicts with any existing implementor.
    fn does_conflict(&mut self, implementor: Binder<Implementor>) -> bool {
        let def = implementor.instantiate_identity().trait_def(self.db);
        for impl_map in self
            .const_impl_maps
            .iter()
            .chain(std::iter::once(&&self.impl_table))
        {
            let Some(impls) = impl_map.get(&def) else {
                continue;
            };
            for already_implemented in impls {
                if does_impl_trait_conflict(self.db, *already_implemented, implementor) {
                    return true;
                }
            }
        }

        false
    }
}
