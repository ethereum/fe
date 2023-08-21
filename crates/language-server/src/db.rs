use std::{collections::BTreeSet, path};

use common::{
    diagnostics::CompleteDiagnostic,
    input::{IngotKind, Version},
    InputDb, InputFile, InputIngot,
};
use hir::{
    analysis_pass::AnalysisPassManager, diagnostics::DiagnosticVoucher, hir_def::{TopLevelMod, ItemKind},
    lower::map_file_to_mod, HirDb, LowerHirDb, ParsingPass, SpannedHirDb, span::{DynLazySpan, LazySpan},
};
use hir_analysis::{
    name_resolution::{DefConflictAnalysisPass, ImportAnalysisPass, PathAnalysisPass},
    HirAnalysisDb,
};

use crate::goto::Cursor;

#[salsa::jar(db = LanguageServerDb)]
pub struct Jar(crate::diagnostics::file_line_starts);

pub trait LanguageServerDb:
    salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{ }

impl<DB> LanguageServerDb for DB where
    DB: Sized + salsa::DbWithJar<Jar> + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{ }


#[salsa::db(common::Jar, hir::Jar, hir::LowerJar, hir::SpannedJar, hir_analysis::Jar, Jar)]
pub struct LanguageServerDatabase {
    storage: salsa::Storage<Self>,
    diags: Vec<Box<dyn DiagnosticVoucher>>,
}

impl LanguageServerDatabase {
    pub fn run_on_top_mod(&mut self, top_mod: TopLevelMod) {
        self.run_on_file_with_pass_manager(top_mod, initialize_analysis_pass);
    }

    pub fn run_on_file_with_pass_manager<F>(&mut self, top_mod: TopLevelMod, pm_builder: F)
    where
        F: FnOnce(&LanguageServerDatabase) -> AnalysisPassManager<'_>,
    {
        self.diags.clear();
        self.diags = {
            let mut pass_manager = pm_builder(self);
            pass_manager.run_on_module(top_mod)
        };
    }

    pub fn top_mod_from_file(&mut self, file_path: &path::Path, source: &str) -> TopLevelMod {
        let kind = IngotKind::StandAlone;

        // We set the ingot version to 0.0.0 for stand-alone file.
        let version = Version::new(0, 0, 0);
        let root_file = file_path;
        let ingot = InputIngot::new(
            self,
            file_path.parent().unwrap().as_os_str().to_str().unwrap(),
            kind,
            version,
            BTreeSet::new(),
        );

        let file_name = root_file.file_name().unwrap().to_str().unwrap();
        let file = InputFile::new(self, ingot, file_name.into(), source.to_string());
        ingot.set_root_file(self, file);
        ingot.set_files(self, [file].into());

        map_file_to_mod(self, file)
    }

    pub fn find_enclosing_item(&mut self, top_mod: TopLevelMod, cursor: Cursor) -> Option<ItemKind> {
        let items = top_mod.scope_graph(self.as_hir_db()).items_dfs(self.as_hir_db());

        let mut smallest_enclosing_item = None;
        let mut smallest_range_size = None;

        for item in items {
            let lazy_item_span = DynLazySpan::from(item.lazy_span());
            let item_span = lazy_item_span.resolve(SpannedHirDb::as_spanned_hir_db(self)).unwrap();

            if item_span.range.contains(cursor) {
                let range_size = item_span.range.end() - item_span.range.start();
                if smallest_range_size.is_none() || range_size < smallest_range_size.unwrap() {
                    smallest_enclosing_item = Some(item);
                    smallest_range_size = Some(range_size);
                }
            }
        }

        return smallest_enclosing_item;
    }
    
    pub fn finalize_diags(&self) -> Vec<CompleteDiagnostic> {
        let mut diags: Vec<_> = self.diags.iter().map(|d| d.to_complete(self)).collect();
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
            diags: Vec::new(),
        };
        db.prefill();
        db
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
