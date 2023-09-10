use hir::analysis_pass::ModuleAnalysisPass;

use crate::HirAnalysisDb;

pub mod diagnostics;
pub mod lower;
pub mod trait_;
pub mod ty;

pub struct TypeDefAnalysisPass<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> ModuleAnalysisPass for TypeDefAnalysisPass<'db> {
    fn run_on_module(
        &mut self,
        top_mod: hir::hir_def::TopLevelMod,
    ) -> Vec<Box<dyn hir::diagnostics::DiagnosticVoucher>> {
        let mut diags = vec![];
        let hir_db = self.db.as_hir_db();
        for struct_ in top_mod.all_structs(hir_db) {
            lower::lower_struct(self.db, *struct_);
            diags.extend(
                lower::lower_struct::accumulated::<diagnostics::StructDefDiagAccumulator>(
                    self.db, *struct_,
                )
                .into_iter()
                .map(|diag| Box::new(diag) as _),
            )
        }

        for enum_ in top_mod.all_enums(hir_db) {
            lower::lower_enum(self.db, *enum_);
            diags.extend(
                lower::lower_enum::accumulated::<diagnostics::EnumDefDiagAccumulator>(
                    self.db, *enum_,
                )
                .into_iter()
                .map(|diag| Box::new(diag) as _),
            )
        }

        for contract in top_mod.all_contracts(hir_db) {
            lower::lower_contract(self.db, *contract);
            diags.extend(
                lower::lower_contract::accumulated::<diagnostics::EnumDefDiagAccumulator>(
                    self.db, *contract,
                )
                .into_iter()
                .map(|diag| Box::new(diag) as _),
            )
        }

        diags
    }
}
