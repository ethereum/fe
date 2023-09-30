use std::collections::BTreeMap;

use hir::{
    hir_def::{
        scope_graph::ScopeId, ImplTrait, IngotId, ItemKind, Partial, TopLevelMod, Trait, TraitRef,
    },
    visitor::prelude::{LazyPathTypeSpan, LazyTraitRefSpan},
};
use rustc_hash::FxHashMap;

use crate::{
    name_resolution::{resolve_path_early, EarlyResolvedPath, NameDomain, NameResKind},
    ty::ty_lower::{lower_generic_arg_list_with_diag, lower_generic_arg_with_diag, lower_hir_ty},
    HirAnalysisDb,
};

use super::{
    diagnostics::{TraitConstraintDiag, TraitLowerDiag, TyLowerDiag},
    trait_::{Implementor, TraitDef, TraitEnv, TraitInstId},
    ty_def::TyId,
    ty_lower::{collect_generic_params, lower_hir_ty_with_diag, GenericParamOwnerId},
    unify::UnificationTable,
    visitor::TyDiagCollector,
};

type TraitImplMap = FxHashMap<TraitDef, Vec<Implementor>>;

#[salsa::tracked]
pub(crate) fn lower_trait(db: &dyn HirAnalysisDb, trait_: Trait) -> TraitDef {
    TraitBuilder::new(db, trait_).build()
}

#[salsa::tracked(return_ref)]
pub(crate) fn collect_trait_impls(
    db: &dyn HirAnalysisDb,
    ingot: IngotId,
) -> (
    TraitImplMap,
    FxHashMap<TopLevelMod, Vec<LowerDiagCollection>>,
) {
    let dependent_impls = ingot
        .external_ingots(db.as_hir_db())
        .iter()
        .map(|(_, external)| &collect_trait_impls(db, *external).0)
        .collect();

    let mut collector = ImplementorCollector::new(db, dependent_impls);
    collector.collect_impls(ingot.all_impl_trait(db.as_hir_db()));
    collector.finalize()
}

pub(super) fn lower_trait_ref(
    db: &dyn HirAnalysisDb,
    trait_ref: TraitRef,
    ref_span: LazyTraitRefSpan,
    scope: ScopeId,
) -> (Option<TraitInstId>, Vec<LowerDiagCollection>) {
    let hir_db = db.as_hir_db();
    let (args, diags) = if let Some(args) = trait_ref.generic_args {
        lower_generic_arg_list_with_diag(db, args, ref_span.generic_args(), scope)
    } else {
        (vec![], vec![])
    };

    let mut diags = diags
        .into_iter()
        .map(LowerDiagCollection::Ty)
        .collect::<Vec<_>>();

    let Partial::Present(path) = trait_ref.path else {
        return (None, diags);
    };

    let trait_def = match resolve_path_early(db, path, scope) {
        EarlyResolvedPath::Full(bucket) => match bucket.pick(NameDomain::Type) {
            Ok(res) => {
                let NameResKind::Scope(ScopeId::Item(ItemKind::Trait(trait_))) = res.kind else {
                    return (None, diags);
                };
                lower_trait(db, trait_)
            }

            Err(_) => return (None, diags),
        },

        EarlyResolvedPath::Partial { .. } => {
            diags.push(TyLowerDiag::AssocTy(ref_span.path().into()).into());
            return (None, diags);
        }
    };

    if trait_def.args(db).len() != args.len() {
        diags.push(
            TraitConstraintDiag::TraitArgNumMismatch {
                span: ref_span.into(),
                trait_: trait_def.trait_(db),
                n_given_arg: args.len(),
            }
            .into(),
        );
        return (None, diags);
    }

    let mut has_error = false;
    for (i, (expected, given)) in trait_def.args(db).iter().zip(&args).enumerate() {
        if !expected.kind(db).can_unify(given.kind(db)) {
            let span = ref_span.generic_args().arg_moved(i).into();
            let diag = TraitConstraintDiag::trait_arg_kind_mismatch(
                span,
                expected.kind(db),
                given.kind(db),
            );
            diags.push(diag.into());
            has_error = true;
        }
    }

    if !has_error {
        let ingot = scope.ingot(hir_db);
        (Some(TraitInstId::new(db, trait_def, args, ingot)), diags)
    } else {
        (None, diags)
    }
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
    impl_table: TraitImplMap,
    dependent_impl_maps: Vec<&'db TraitImplMap>,
    diags: FxHashMap<TopLevelMod, Vec<LowerDiagCollection>>,
}

impl<'db> ImplementorCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, dependent_impl_maps: Vec<&'db TraitImplMap>) -> Self {
        Self {
            db,
            impl_table: TraitImplMap::default(),
            dependent_impl_maps,
            diags: FxHashMap::default(),
        }
    }

    fn finalize(
        self,
    ) -> (
        TraitImplMap,
        FxHashMap<TopLevelMod, Vec<LowerDiagCollection>>,
    ) {
        (self.impl_table, self.diags)
    }

    fn collect_impls(&mut self, impls: &[ImplTrait]) {
        for &impl_ in impls {
            let Some(implementor) = self.lower_impl(impl_) else {
                continue;
            };

            if let Some(conflict_with) = self.does_conflict(implementor) {
                let diag = TraitLowerDiag::conflict_impl(
                    implementor.impl_def(self.db),
                    conflict_with.impl_def(self.db),
                );
                self.push_diag(impl_, diag);
            } else {
                self.impl_table
                    .entry(implementor.trait_def(self.db))
                    .or_default()
                    .push(implementor);
            }
        }
    }

    fn lower_impl(&mut self, impl_: ImplTrait) -> Option<Implementor> {
        let ty = self.lower_implementor_ty(impl_)?;
        let trait_ = self.instantiate_trait(impl_, ty)?;
        let impl_trait_ingot = impl_
            .top_mod(self.db.as_hir_db())
            .ingot(self.db.as_hir_db());

        if Some(impl_trait_ingot) != ty.ingot(self.db)
            && impl_trait_ingot != trait_.def(self.db).ingot(self.db)
        {
            let diag = TraitLowerDiag::external_trait_for_external_type(impl_);
            self.push_diag(impl_, diag);
            return None;
        }

        let param_owner = GenericParamOwnerId::new(self.db, impl_.into());
        let params = collect_generic_params(self.db, param_owner);
        Some(Implementor::new(
            self.db,
            impl_,
            trait_,
            ty,
            params.params.clone(),
        ))
    }

    fn lower_implementor_ty(&mut self, impl_: ImplTrait) -> Option<TyId> {
        let hir_ty = impl_.ty(self.db.as_hir_db()).to_opt()?;
        let scope = impl_.scope();
        let (ty, diags) = lower_hir_ty_with_diag(self.db, hir_ty, impl_.lazy_span().ty(), scope);
        if diags.is_empty() {
            Some(ty)
        } else {
            for diag in diags {
                self.push_diag(impl_, diag);
            }
            None
        }
    }

    fn instantiate_trait(
        &mut self,
        impl_trait: ImplTrait,
        implementor_ty: TyId,
    ) -> Option<TraitInstId> {
        let trait_ref = impl_trait.trait_ref(self.db.as_hir_db()).to_opt()?;
        let (trait_inst, diags) = lower_trait_ref(
            self.db,
            trait_ref,
            impl_trait.lazy_span().trait_ref_moved(),
            impl_trait.scope(),
        );
        for diag in diags {
            self.push_diag(impl_trait, diag);
        }

        let trait_inst = trait_inst?;
        if implementor_ty
            .kind(self.db)
            .can_unify(trait_inst.def(self.db).expected_implementor_kind(self.db))
        {
            Some(trait_inst)
        } else {
            let diag = TraitConstraintDiag::KindMismatch {
                primary: impl_trait.lazy_span().ty_moved().into(),
                trait_def: trait_inst.def(self.db).trait_(self.db),
            };
            self.push_diag(impl_trait, diag);
            None
        }
    }

    fn does_conflict(&mut self, implementor: Implementor) -> Option<Implementor> {
        let def = implementor.trait_def(self.db);
        for &already_implemented in self.impl_table.get(&def)? {
            let mut table = UnificationTable::new(self.db);
            if already_implemented.does_conflict(self.db, implementor, &mut table) {
                return Some(already_implemented);
            }
        }

        None
    }

    fn get_implementors_for(&mut self, def: TraitDef) -> impl Iterator<Item = Implementor> + '_ {
        self.dependent_impl_maps
            .iter()
            .filter_map(move |table| table.get(&def).map(|impls| impls.iter().copied()))
            .flatten()
            .chain(self.impl_table.get(&def).into_iter().flatten().copied())
    }

    fn push_diag(&mut self, impl_: ImplTrait, diag: impl Into<LowerDiagCollection>) {
        let top_mod = impl_.top_mod(self.db.as_hir_db());
        self.diags.entry(top_mod).or_default().push(diag.into());
    }
}

impl Implementor {
    fn does_conflict(
        self,
        db: &dyn HirAnalysisDb,
        other: Self,
        table: &mut UnificationTable,
    ) -> bool {
        let generalized_self = self.generalize(db, table);
        let generalized_other = other.generalize(db, table);
        if !generalized_self
            .trait_(db)
            .can_unify(db, generalized_other.trait_(db), table)
        {
            return false;
        }

        table.unify(generalized_self.ty(db), generalized_other.ty(db))
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, derive_more::From)]
pub(crate) enum LowerDiagCollection {
    Ty(TyLowerDiag),
    Satisfaction(TraitConstraintDiag),
    TraitLower(TraitLowerDiag),
}

impl LowerDiagCollection {
    pub(super) fn to_voucher(&self) -> Box<dyn hir::diagnostics::DiagnosticVoucher> {
        match self.clone() {
            LowerDiagCollection::Ty(diag) => Box::new(diag) as _,
            LowerDiagCollection::Satisfaction(diag) => Box::new(diag) as _,
            LowerDiagCollection::TraitLower(diag) => Box::new(diag) as _,
        }
    }
}
