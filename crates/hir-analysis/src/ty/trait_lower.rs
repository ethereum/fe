use hir::hir_def::{
    scope_graph::ScopeId, ImplTrait, IngotId, ItemKind, Partial, PathId, Trait, TraitRefId,
};
use rustc_hash::FxHashMap;

use crate::{
    name_resolution::{resolve_path_early, EarlyResolvedPath, NameDomain, NameResKind},
    ty::ty_lower::lower_hir_ty,
    HirAnalysisDb,
};

use super::{
    trait_::{Implementor, TraitDef, TraitInstId},
    ty_def::{Kind, TyId},
    ty_lower::{collect_generic_params, lower_generic_arg_list, GenericParamOwnerId},
    unify::UnificationTable,
};

type TraitImplTable = FxHashMap<TraitDef, Vec<Implementor>>;

#[salsa::tracked]
pub(crate) fn lower_trait(db: &dyn HirAnalysisDb, trait_: Trait) -> TraitDef {
    TraitBuilder::new(db, trait_).build()
}

#[salsa::tracked(return_ref)]
pub(crate) fn collect_trait_impls(db: &dyn HirAnalysisDb, ingot: IngotId) -> TraitImplTable {
    let dependent_impls = ingot
        .external_ingots(db.as_hir_db())
        .iter()
        .map(|(_, external)| collect_trait_impls(db, *external))
        .collect();

    let mut collector = ImplementorCollector::new(db, dependent_impls);
    collector.collect_impls(ingot.all_impl_trait(db.as_hir_db()));
    collector.finalize()
}

#[salsa::tracked]
pub(crate) fn lower_impl_trait(
    db: &dyn HirAnalysisDb,
    impl_trait: ImplTrait,
) -> Option<Implementor> {
    let hir_db = db.as_hir_db();
    let scope = impl_trait.scope();

    let hir_ty = impl_trait.ty(hir_db).to_opt()?;
    let ty = lower_hir_ty(db, hir_ty, scope);
    if ty.contains_invalid(db) {
        return None;
    }

    let trait_ = lower_trait_ref(
        db,
        impl_trait.trait_ref(hir_db).to_opt()?,
        impl_trait.scope(),
    )
    .ok()?;

    let impl_trait_ingot = impl_trait.top_mod(hir_db).ingot(hir_db);

    if Some(impl_trait_ingot) != ty.ingot(db) && impl_trait_ingot != trait_.def(db).ingot(db) {
        return None;
    }

    let param_owner = GenericParamOwnerId::new(db, impl_trait.into());
    let params = collect_generic_params(db, param_owner);

    Some(Implementor::new(
        db,
        trait_,
        ty,
        params.params.clone(),
        impl_trait,
    ))
}

#[salsa::tracked]
pub(crate) fn lower_trait_ref(
    db: &dyn HirAnalysisDb,
    trait_ref: TraitRefId,
    scope: ScopeId,
) -> Result<TraitInstId, TraitRefLowerError> {
    let hir_db = db.as_hir_db();
    let args = if let Some(args) = trait_ref.generic_args(hir_db) {
        lower_generic_arg_list(db, args, scope)
    } else {
        vec![]
    };

    let Partial::Present(path) = trait_ref.path(hir_db) else {
        return Err(TraitRefLowerError::TraitNotFound);
    };

    let trait_def = match resolve_path_early(db, path, scope) {
        EarlyResolvedPath::Full(bucket) => match bucket.pick(NameDomain::Type) {
            Ok(res) => {
                let NameResKind::Scope(ScopeId::Item(ItemKind::Trait(trait_))) = res.kind else {
                    return Err(TraitRefLowerError::TraitNotFound);
                };
                lower_trait(db, trait_)
            }

            Err(_) => return Err(TraitRefLowerError::TraitNotFound),
        },

        EarlyResolvedPath::Partial { .. } => {
            return Err(TraitRefLowerError::AssocTy(path));
        }
    };

    if trait_def.params(db).len() != args.len() {
        return Err(TraitRefLowerError::ArgNumMismatch {
            expected: trait_def.params(db).len(),
            given: args.len(),
        });
    }

    for (param, arg) in trait_def.params(db).iter().zip(args.iter()) {
        if !param.kind(db).does_match(arg.kind(db)) {
            return Err(TraitRefLowerError::ArgumentKindMisMatch {
                expected: param.kind(db).clone(),
                given: *arg,
            });
        }
    }

    let ingot = scope.ingot(hir_db);
    Ok(TraitInstId::new(db, trait_def, args, ingot))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TraitRefLowerError {
    /// The trait reference is not a valid trait reference. This error is
    /// reported by the name resolution and no need to report it again.
    TraitNotFound,

    AssocTy(PathId),

    ArgNumMismatch {
        expected: usize,
        given: usize,
    },

    ArgumentKindMisMatch {
        expected: Kind,
        given: TyId,
    },
}

struct TraitBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    trait_: Trait,
    params: Vec<TyId>,
    self_arg: TyId,
    // TODO: We need to lower associated methods here.
    // methods: Vec
}

impl<'db> TraitBuilder<'db> {
    fn new(db: &'db dyn HirAnalysisDb, trait_: Trait) -> Self {
        let params_owner_id = GenericParamOwnerId::new(db, trait_.into());
        let params_set = collect_generic_params(db, params_owner_id);
        Self {
            db,
            trait_,
            params: params_set.params.clone(),
            self_arg: params_set.trait_self.unwrap(),
        }
    }

    fn build(self) -> TraitDef {
        TraitDef::new(self.db, self.trait_, self.params, self.self_arg)
    }
}

/// Collect all implementors in an ingot.
struct ImplementorCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    impl_table: TraitImplTable,
    dependent_impl_maps: Vec<&'db TraitImplTable>,
}

impl<'db> ImplementorCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, dependent_impl_maps: Vec<&'db TraitImplTable>) -> Self {
        Self {
            db,
            impl_table: TraitImplTable::default(),
            dependent_impl_maps,
        }
    }

    fn finalize(self) -> TraitImplTable {
        self.impl_table
    }

    fn collect_impls(&mut self, impls: &[ImplTrait]) {
        for &impl_ in impls {
            let Some(implementor) = lower_impl_trait(self.db, impl_) else {
                continue;
            };

            if !self.does_conflict(implementor) {
                self.impl_table
                    .entry(implementor.trait_def(self.db))
                    .or_default()
                    .push(implementor);
            }
        }
    }

    /// Returns `true` if `implementor` conflicts with any existing implementor.
    fn does_conflict(&mut self, implementor: Implementor) -> bool {
        let def = implementor.trait_def(self.db);
        for impl_map in self
            .dependent_impl_maps
            .iter()
            .chain(std::iter::once(&&self.impl_table))
        {
            let Some(impls) = impl_map.get(&def) else {
                continue;
            };
            for already_implemented in impls {
                let mut table = UnificationTable::new(self.db);
                if already_implemented.does_conflict(self.db, implementor, &mut table) {
                    return true;
                }
            }
        }

        false
    }
}
