use hir::{analysis_pass::ModuleAnalysisPass, hir_def::TopLevelMod};

use self::{
    adt_def::AdtRefId,
    def_analysis::{
        analyze_adt, analyze_func, analyze_impl, analyze_impl_trait, analyze_trait,
        analyze_type_alias,
    },
    diagnostics::{
        AdtDefDiagAccumulator, FuncBodyDiagAccumulator, FuncDefDiagAccumulator,
        ImplDefDiagAccumulator, ImplTraitDefDiagAccumulator, TraitDefDiagAccumulator,
        TypeAliasDefDiagAccumulator,
    },
};
use crate::HirAnalysisDb;

pub mod adt_def;
pub mod binder;
pub mod const_ty;
pub mod constraint_solver;
pub mod def_analysis;
pub mod diagnostics;
pub mod fold;
pub mod func_def;
pub mod method_table;
pub mod trait_def;
pub mod trait_lower;
pub mod ty_check;
pub mod ty_def;
pub mod ty_lower;
pub mod visitor;

pub(crate) mod constraint;

mod canonical;
mod method_cmp;
mod unify;

/// An analysis pass for type definitions.
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
            analyze_adt::accumulated::<AdtDefDiagAccumulator>(self.db, adt).into_iter()
        })
        .map(|diag| diag.to_voucher())
        .collect()
    }
}

pub struct BodyAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}
impl<'db> BodyAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}
impl<'db> ModuleAnalysisPass for BodyAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod,
    ) -> Vec<Box<dyn hir::diagnostics::DiagnosticVoucher>> {
        top_mod
            .all_funcs(self.db.as_hir_db())
            .iter()
            .flat_map(|func| {
                ty_check::check_func_body::accumulated::<FuncBodyDiagAccumulator>(self.db, *func)
                    .into_iter()
            })
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for trait definitions.
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
            .flat_map(|trait_| {
                analyze_trait::accumulated::<TraitDefDiagAccumulator>(self.db, *trait_)
            })
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

pub struct ImplAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> ImplAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass for ImplAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod,
    ) -> Vec<Box<dyn hir::diagnostics::DiagnosticVoucher>> {
        top_mod
            .all_impls(self.db.as_hir_db())
            .iter()
            .flat_map(|impl_| analyze_impl::accumulated::<ImplDefDiagAccumulator>(self.db, *impl_))
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for `ImplTrait'.
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
        top_mod
            .all_impl_traits(self.db.as_hir_db())
            .iter()
            .flat_map(|trait_| {
                analyze_impl_trait::accumulated::<ImplTraitDefDiagAccumulator>(self.db, *trait_)
            })
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for `ImplTrait'.
pub struct FuncAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> FuncAnalysisPass<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
}

impl<'db> ModuleAnalysisPass for FuncAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: TopLevelMod,
    ) -> Vec<Box<dyn hir::diagnostics::DiagnosticVoucher>> {
        top_mod
            .all_funcs(self.db.as_hir_db())
            .iter()
            .flat_map(|func| analyze_func::accumulated::<FuncDefDiagAccumulator>(self.db, *func))
            .map(|diag| diag.to_voucher())
            .collect()
    }
}

/// An analysis pass for type aliases.
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
        top_mod
            .all_type_aliases(self.db.as_hir_db())
            .iter()
            .flat_map(|alias| {
                analyze_type_alias::accumulated::<TypeAliasDefDiagAccumulator>(self.db, *alias)
                    .into_iter()
            })
            .map(|diag| diag.to_voucher())
            .collect()
    }
}
