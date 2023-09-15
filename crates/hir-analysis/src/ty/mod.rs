use crate::HirAnalysisDb;
use hir::analysis_pass::ModuleAnalysisPass;
use rustc_hash::FxHashSet;

use self::{
    diagnostics::{
        AdtDefDiagAccumulator, GenericParamDiagAccumulator, TyLowerDiag,
        TypeAliasDefDiagAccumulator,
    },
    ty::AdtRefId,
};

pub mod adt_analysis;
pub mod diagnostics;
pub mod lower;
pub mod trait_;
pub mod ty;
pub mod visitor;

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
        top_mod: hir::hir_def::TopLevelMod,
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
            adt_analysis::analyze_adt::accumulated::<AdtDefDiagAccumulator>(self.db, adt)
                .into_iter()
                .chain(
                    adt_analysis::analyze_adt::accumulated::<GenericParamDiagAccumulator>(
                        self.db, adt,
                    )
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
        top_mod: hir::hir_def::TopLevelMod,
    ) -> Vec<Box<dyn hir::diagnostics::DiagnosticVoucher>> {
        let diags: FxHashSet<TyLowerDiag> = top_mod
            .all_type_aliases(self.db.as_hir_db())
            .iter()
            .map(|&alias| {
                lower::lower_type_alias::accumulated::<TypeAliasDefDiagAccumulator>(self.db, alias)
                    .into_iter()
                    .chain(
                        lower::lower_type_alias::accumulated::<GenericParamDiagAccumulator>(
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
