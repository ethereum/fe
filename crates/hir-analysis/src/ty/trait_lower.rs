//! This module implements the trait and impl trait lowering process.

use common::indexmap::IndexMap;
use hir::hir_def::{scope_graph::ScopeId, IdentId, ImplTrait, IngotId, Partial, Trait, TraitRefId};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    func_def::FuncDef,
    trait_def::{does_impl_trait_conflict, Implementor, TraitDef, TraitInstId, TraitMethod},
    ty_def::{InvalidCause, Kind, TyId},
    ty_lower::{collect_generic_params, lower_generic_arg_list, GenericParamTypeSet},
};
use crate::{
    name_resolution::{resolve_path, PathRes},
    ty::{func_def::lower_func, ty_def::TyData, ty_lower::lower_hir_ty},
    HirAnalysisDb,
};

type TraitImplTable<'db> = FxHashMap<TraitDef<'db>, Vec<Binder<Implementor<'db>>>>;

#[salsa::tracked]
pub(crate) fn lower_trait<'db>(db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> TraitDef<'db> {
    TraitBuilder::new(db, trait_).build()
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
    let hir_db = db;
    let scope = impl_trait.scope();

    let hir_ty = impl_trait.ty(hir_db).to_opt()?;
    let ty = lower_hir_ty(db, hir_ty, scope);
    if ty.has_invalid(db) {
        return None;
    }

    let trait_ = lower_trait_ref(
        db,
        ty,
        impl_trait.trait_ref(hir_db).to_opt()?,
        impl_trait.scope(),
    )
    .ok()?;

    let impl_trait_ingot = impl_trait.top_mod(hir_db).ingot(hir_db);

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
            (Some(name), Some(ty)) => Some((name, lower_hir_ty(db, ty, scope))),
            _ => None,
        })
        .collect();

    for t in trait_.def(db).trait_(db).types(db).iter() {
        let (Some(name), Some(default)) = (t.name.to_opt(), t.default) else {
            continue;
        };
        types
            .entry(name)
            .or_insert_with(|| lower_hir_ty(db, default, scope));
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
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    let hir_db = db;

    let mut args = vec![self_ty];
    if let Some(generic_args) = trait_ref.generic_args(hir_db) {
        args.extend(lower_generic_arg_list(db, generic_args, scope));
    };

    let Partial::Present(path) = trait_ref.path(hir_db) else {
        return Err(TraitRefLowerError::Other);
    };

    let trait_def = match resolve_path(db, path, scope, None, false) {
        Ok(PathRes::Trait(t)) => t,
        _ => return Err(TraitRefLowerError::Other),
    };

    // The first parameter of the trait is the self type, so we need to skip it.
    if trait_def.params(db).len() != args.len() {
        return Err(TraitRefLowerError::ArgNumMismatch {
            expected: trait_def.params(db).len() - 1,
            given: args.len() - 1,
        });
    }

    for (param, arg) in trait_def
        .params(db)
        .iter()
        .skip(1)
        .zip(args.iter_mut().skip(1))
    {
        if !param.kind(db).does_match(arg.kind(db)) {
            return Err(TraitRefLowerError::ArgKindMisMatch {
                expected: param.kind(db).clone(),
                given: *arg,
            });
        }

        let expected_const_ty = match param.data(db) {
            TyData::ConstTy(expected_ty) => expected_ty.ty(db).into(),
            _ => None,
        };

        match arg.evaluate_const_ty(db, expected_const_ty) {
            Ok(ty) => *arg = ty,

            Err(InvalidCause::ConstTyMismatch { expected, given }) => {
                return Err(TraitRefLowerError::ArgTypeMismatch {
                    expected: Some(expected),
                    given: Some(given),
                });
            }

            Err(InvalidCause::ConstTyExpected { expected }) => {
                return Err(TraitRefLowerError::ArgTypeMismatch {
                    expected: Some(expected),
                    given: None,
                });
            }

            Err(InvalidCause::NormalTypeExpected { given }) => {
                return Err(TraitRefLowerError::ArgTypeMismatch {
                    expected: None,
                    given: Some(given),
                })
            }

            _ => return Err(TraitRefLowerError::Other),
        }
    }

    Ok(TraitInstId::new(db, trait_def, args))
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
    ArgNumMismatch { expected: usize, given: usize },

    /// The kind of the argument doesn't match the kind of the parameter of the
    /// trait.
    ArgKindMisMatch { expected: Kind, given: TyId<'db> },

    /// The argument type doesn't match the const parameter type.
    ArgTypeMismatch {
        expected: Option<TyId<'db>>,
        given: Option<TyId<'db>>,
    },

    /// Other errors, which is reported by another pass. So we don't need to
    /// report this error kind.
    Other,
}

struct TraitBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
    param_set: GenericParamTypeSet<'db>,
    methods: IndexMap<IdentId<'db>, TraitMethod<'db>>,
}

impl<'db> TraitBuilder<'db> {
    fn new(db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> Self {
        let param_set = collect_generic_params(db, trait_.into());

        Self {
            db,
            trait_,
            param_set,
            methods: IndexMap::default(),
        }
    }

    fn build(mut self) -> TraitDef<'db> {
        self.collect_params();
        self.collect_methods();

        TraitDef::new(self.db, self.trait_, self.param_set, self.methods)
    }

    fn collect_params(&mut self) {
        self.param_set = collect_generic_params(self.db, self.trait_.into());
    }

    fn collect_methods(&mut self) {
        let hir_db = self.db;
        for method in self.trait_.methods(hir_db) {
            let Some(func) = lower_func(self.db, method) else {
                continue;
            };

            let name = func.name(self.db);
            let trait_method = TraitMethod(func);
            // We can simply ignore the conflict here because it's already handled by the
            // name resolution.
            self.methods.entry(name).or_insert(trait_method);
        }
    }
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
