#![allow(unused)]
use crate::HirAnalysisDb;
use hir::{analysis_pass::ModuleAnalysisPass, hir_def::TopLevelMod};
use rustc_hash::FxHashSet;

use self::{
    adt_analysis::analyze_adt,
    diagnostics::{
        AdtDefDiagAccumulator, GenericParamDiagAccumulator, TraitSatisfactionDiag, TyLowerDiag,
        TypeAliasDefDiagAccumulator,
    },
    trait_lower::{collect_trait_impl, TraitImplDiag},
    ty::AdtRefId,
    ty_lower::{collect_generic_params, lower_type_alias, GenericParamOwnerId},
};

pub mod adt_analysis;
pub mod diagnostics;
pub mod trait_;
pub mod trait_lower;
pub mod ty;
pub mod ty_lower;
pub mod visitor;

mod unify;

pub struct TypeDefAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> TypeDefAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass for TypeDefAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod,
    ) -> Vec<Box<dyn hir::diagnostics::DiagnosticVoucher>> {
        let hir_db = self.db.as_hir_db();
        let adts = top_mod
            .all_structs(hir_db)
            .iter()
            .map(|s| AdtRefId::from_struct(self.db, *s))
            .chain(
                top_mod
                    .all_enums(hir_db)
                    .iter()
                    .map(|e| AdtRefId::from_enum(self.db, *e)),
            )
            .chain(
                top_mod
                    .all_contracts(hir_db)
                    .iter()
                    .map(|c| AdtRefId::from_contract(self.db, *c)),
            );

        adts.map(|adt| {
            analyze_adt::accumulated::<AdtDefDiagAccumulator>(self.db, adt)
                .into_iter()
                .chain(
                    if let Some(owner_id) = adt.generic_owner_id(self.db) {
                        collect_generic_params::accumulated::<GenericParamDiagAccumulator>(
                            self.db, owner_id,
                        )
                    } else {
                        Vec::new()
                    }
                    .into_iter(),
                )
        })
        .flatten()
        .map(|diag| Box::new(diag) as _)
        .collect()
    }
}

pub struct TypeAliasAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> TypeAliasAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass for TypeAliasAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod,
    ) -> Vec<Box<dyn hir::diagnostics::DiagnosticVoucher>> {
        let diags: FxHashSet<TyLowerDiag> = top_mod
            .all_type_aliases(self.db.as_hir_db())
            .iter()
            .map(|&alias| {
                lower_type_alias::accumulated::<TypeAliasDefDiagAccumulator>(self.db, alias)
                    .into_iter()
                    .chain(
                        lower_type_alias::accumulated::<GenericParamDiagAccumulator>(
                            self.db, alias,
                        )
                        .into_iter(),
                    )
            })
            .flatten()
            .collect();

        diags.into_iter().map(|diag| Box::new(diag) as _).collect()
    }
}

pub struct TraitAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}
impl<'db> TraitAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass for TraitAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod,
    ) -> Vec<Box<dyn hir::diagnostics::DiagnosticVoucher>> {
        top_mod
            .all_traits(self.db.as_hir_db())
            .iter()
            .map(|&trait_| {
                let owner_id = GenericParamOwnerId::new(self.db, trait_.into());
                collect_generic_params::accumulated::<GenericParamDiagAccumulator>(
                    self.db, owner_id,
                )
            })
            .flatten()
            .map(|diag| Box::new(diag) as _)
            .collect()
    }
}

pub struct ImplTraitAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> ImplTraitAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass for ImplTraitAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod,
    ) -> Vec<Box<dyn hir::diagnostics::DiagnosticVoucher>> {
        let ingot = top_mod.ingot(self.db.as_hir_db());
        let (_, diags) = collect_trait_impl(self.db, ingot);
        let Some(diags) = diags.get(&top_mod) else {
            return Vec::new();
        };

        diags
            .iter()
            .map(|diag| match diag {
                TraitImplDiag::Ty(diag) => Box::new(diag.clone()) as _,
                TraitImplDiag::Satisfaction(diag) => Box::new(diag.clone()) as _,
                TraitImplDiag::TraitImplLower(diag) => Box::new(diag.clone()) as _,
            })
            .chain(
                top_mod
                    .all_impl_traits(self.db.as_hir_db())
                    .iter()
                    .copied()
                    .map(|impl_trait| {
                        let owner_id = GenericParamOwnerId::new(self.db, impl_trait.into());
                        collect_generic_params::accumulated::<GenericParamDiagAccumulator>(
                            self.db, owner_id,
                        )
                    })
                    .flatten()
                    .map(|diag| Box::new(diag) as _),
            )
            .collect()
    }
}
