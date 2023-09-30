#![allow(unused)]
use crate::HirAnalysisDb;
use hir::{analysis_pass::ModuleAnalysisPass, hir_def::TopLevelMod};
use rustc_hash::FxHashSet;

use self::{
    adt_analysis::analyze_adt,
    constraint::collect_super_traits,
    diagnostics::{
        AdtDefDiagAccumulator, GenericParamDiagAccumulator, TraitConstraintDiag, TyLowerDiag,
        TypeAliasDefDiagAccumulator,
    },
    trait_lower::{collect_trait_impls, lower_trait, LowerDiagCollection},
    ty_def::AdtRefId,
    ty_lower::{collect_generic_params, lower_type_alias, GenericParamOwnerId},
};

pub mod adt_analysis;
pub mod diagnostics;
pub mod trait_;
pub mod trait_lower;
pub mod ty_def;
pub mod ty_lower;
pub mod visitor;

pub(crate) mod constraint;

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

        adts.flat_map(|adt| {
            analyze_adt::accumulated::<AdtDefDiagAccumulator>(self.db, adt)
                .into_iter()
                .chain(if let Some(owner_id) = adt.generic_owner_id(self.db) {
                    collect_generic_params::accumulated::<GenericParamDiagAccumulator>(
                        self.db, owner_id,
                    )
                } else {
                    Vec::new()
                })
        })
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
            .flat_map(|&alias| {
                lower_type_alias::accumulated::<TypeAliasDefDiagAccumulator>(self.db, alias)
                    .into_iter()
                    .chain(
                        lower_type_alias::accumulated::<GenericParamDiagAccumulator>(
                            self.db, alias,
                        ),
                    )
            })
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
        let mut diags = Vec::new();

        for &trait_ in top_mod.all_traits(self.db.as_hir_db()) {
            let owner_id = GenericParamOwnerId::new(self.db, trait_.into());
            diags.extend(
                collect_generic_params::accumulated::<GenericParamDiagAccumulator>(
                    self.db, owner_id,
                )
                .into_iter()
                .map(|diag| Box::new(diag) as _),
            );

            let def = lower_trait(self.db, trait_);
            let (_, super_traits_diags) = collect_super_traits(self.db, def);
            diags.extend(super_traits_diags.iter().map(|diag| diag.to_voucher()));
        }

        diags
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
        let (_, diags) = collect_trait_impls(self.db, ingot);
        let Some(diags) = diags.get(&top_mod) else {
            return Vec::new();
        };

        diags
            .iter()
            .map(|diag| diag.to_voucher())
            .chain(
                top_mod
                    .all_impl_traits(self.db.as_hir_db())
                    .iter()
                    .copied()
                    .flat_map(|impl_trait| {
                        let owner_id = GenericParamOwnerId::new(self.db, impl_trait.into());
                        collect_generic_params::accumulated::<GenericParamDiagAccumulator>(
                            self.db, owner_id,
                        )
                    })
                    .map(|diag| Box::new(diag) as _),
            )
            .collect()
    }
}
