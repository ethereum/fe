use common::{diagnostics::CompleteDiagnostic, InputDb};
use hir::{
    analysis_pass::AnalysisPassManager, diagnostics::DiagnosticVoucher, hir_def::TopLevelMod,
    HirDb, LowerHirDb, ParsingPass, SpannedHirDb,
};
use hir_analysis::{
    name_resolution::{DefConflictAnalysisPass, ImportAnalysisPass, PathAnalysisPass},
    HirAnalysisDb,
};
use salsa::{ParallelDatabase, Snapshot};

#[salsa::jar(db = LanguageServerDb)]
pub struct Jar(crate::functionality::diagnostics::file_line_starts);

pub trait LanguageServerDb:
    salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

impl<DB> LanguageServerDb for DB where
    DB: Sized + salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

#[salsa::db(
    common::Jar,
    hir::Jar,
    hir::LowerJar,
    hir::SpannedJar,
    hir_analysis::Jar,
    Jar
)]
pub struct LanguageServerDatabase {
    storage: salsa::Storage<Self>,
}

impl LanguageServerDatabase {
    pub fn analyze_top_mod(&self, top_mod: TopLevelMod) -> Vec<Box<dyn DiagnosticVoucher>> {
        let mut pass_manager = initialize_analysis_pass(self);
        pass_manager.run_on_module(top_mod)
    }

    pub fn finalize_diags(&self, diags: &[Box<dyn DiagnosticVoucher>]) -> Vec<CompleteDiagnostic> {
        let mut diags: Vec<_> = diags.iter().map(|d| d.to_complete(self)).collect();
        diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
            std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
            ord => ord,
        });
        diags
    }
    pub fn as_language_server_db(&self) -> &dyn LanguageServerDb {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db::<'_>(self)
    }
}

impl salsa::Database for LanguageServerDatabase {
    fn salsa_event(&self, _: salsa::Event) {}
}

impl Default for LanguageServerDatabase {
    fn default() -> Self {
        let db = Self {
            storage: Default::default(),
        };
        db.prefill();
        db
    }
}

impl ParallelDatabase for LanguageServerDatabase {
    fn snapshot(&self) -> Snapshot<Self> {
        Snapshot::new(LanguageServerDatabase {
            storage: self.storage.snapshot(),
        })
    }
}

fn initialize_analysis_pass(db: &LanguageServerDatabase) -> AnalysisPassManager<'_> {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass(Box::new(ParsingPass::new(db)));
    pass_manager.add_module_pass(Box::new(DefConflictAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(ImportAnalysisPass::new(db)));
    pass_manager.add_module_pass(Box::new(PathAnalysisPass::new(db)));
    pass_manager
}
