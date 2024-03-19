use common::{diagnostics::CompleteDiagnostic, InputDb};
use hir::{
    analysis_pass::AnalysisPassManager,
    diagnostics::DiagnosticVoucher,
    hir_def::{ItemKind, TopLevelMod},
    span::{DynLazySpan, LazySpan},
    HirDb, LowerHirDb, ParsingPass, SpannedHirDb,
};
use hir_analysis::{
    name_resolution::{DefConflictAnalysisPass, ImportAnalysisPass, PathAnalysisPass},
    HirAnalysisDb,
};
use salsa::{ParallelDatabase, Snapshot};

use crate::goto::Cursor;

#[salsa::jar(db = LanguageServerDb)]
pub struct Jar(crate::diagnostics::file_line_starts);

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

    pub fn find_enclosing_item(&self, top_mod: TopLevelMod, cursor: Cursor) -> Option<ItemKind> {
        let items = top_mod
            .scope_graph(self.as_hir_db())
            .items_dfs(self.as_hir_db());

        let mut smallest_enclosing_item = None;
        let mut smallest_range_size = None;

        for item in items {
            let lazy_item_span = DynLazySpan::from(item.lazy_span());
            let item_span = lazy_item_span
                .resolve(SpannedHirDb::as_spanned_hir_db(self))
                .unwrap();

            if item_span.range.contains(cursor) {
                let range_size = item_span.range.end() - item_span.range.start();
                if smallest_range_size.is_none() || range_size < smallest_range_size.unwrap() {
                    smallest_enclosing_item = Some(item);
                    smallest_range_size = Some(range_size);
                }
            }
        }

        smallest_enclosing_item
    }

    pub fn finalize_diags(
        &self,
        diags: &[Box<dyn DiagnosticVoucher>],
    ) -> Vec<CompleteDiagnostic> {
        let mut diags: Vec<_> = diags.iter().map(|d| d.to_complete(self)).collect();
        diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
            std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
            ord => ord,
        });
        diags
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
