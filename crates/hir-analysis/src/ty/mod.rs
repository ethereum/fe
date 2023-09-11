use hir::analysis_pass::ModuleAnalysisPass;

use crate::HirAnalysisDb;

use self::{diagnostics::AdtDefDiagAccumulator, ty::AdtRefId};

pub mod diagnostics;
pub mod lower;
pub mod trait_;
pub mod ty;

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
            lower::analyze_adt::accumulated::<AdtDefDiagAccumulator>(self.db, adt)
                .into_iter()
                .map(|diag| Box::new(diag) as _)
        })
        .flatten()
        .collect()
    }
}
