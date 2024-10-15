use std::ops::Range;

use camino::Utf8Path;

use codespan_reporting as cs;
use cs::files as cs_files;

use common::{diagnostics::CompleteDiagnostic, InputDb, InputFile};

use fxhash::FxHashMap;
use hir::{
    analysis_pass::AnalysisPassManager, diagnostics::DiagnosticVoucher, hir_def::TopLevelMod,
    lower::map_file_to_mod, ParsingPass,
};
use hir_analysis::name_resolution::{
    DefConflictAnalysisPass, ImportAnalysisPass, PathAnalysisPass,
};

use crate::{
    backend::db::{LanguageServerDatabase, LanguageServerDb},
    util::diag_to_lsp,
};

#[salsa::tracked(return_ref)]
pub fn file_line_starts(db: &dyn LanguageServerDb, file: InputFile) -> Vec<usize> {
    cs::files::line_starts(file.text(db.as_input_db())).collect()
}

impl<'a> cs_files::Files<'a> for LanguageServerDatabase {
    type FileId = InputFile;
    type Name = &'a Utf8Path;
    type Source = &'a str;

    fn name(&'a self, file_id: Self::FileId) -> Result<Self::Name, cs_files::Error> {
        Ok(file_id.path(self).as_path())
    }

    fn source(&'a self, file_id: Self::FileId) -> Result<Self::Source, cs_files::Error> {
        Ok(file_id.text(self))
    }

    fn line_index(
        &'a self,
        file_id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, cs_files::Error> {
        let starts = file_line_starts(self, file_id);
        Ok(starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'a self,
        file_id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, cs_files::Error> {
        let line_starts = file_line_starts(self, file_id);

        let start = *line_starts
            .get(line_index)
            .ok_or(cs_files::Error::LineTooLarge {
                given: line_index,
                max: line_starts.len() - 1,
            })?;

        let end = if line_index == line_starts.len() - 1 {
            file_id.text(self).len()
        } else {
            *line_starts
                .get(line_index + 1)
                .ok_or(cs_files::Error::LineTooLarge {
                    given: line_index,
                    max: line_starts.len() - 1,
                })?
        };

        Ok(Range { start, end })
    }
}

impl LanguageServerDatabase {
    pub fn analyze_top_mod<'a>(
        &'a self,
        top_mod: TopLevelMod<'a>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'a>> {
        let mut pass_manager = initialize_analysis_pass(self);
        pass_manager.run_on_module(top_mod)
    }

    pub fn get_lsp_diagnostics(
        &self,
        files: Vec<InputFile>,
    ) -> FxHashMap<async_lsp::lsp_types::Url, Vec<async_lsp::lsp_types::Diagnostic>> {
        let mut result =
            FxHashMap::<async_lsp::lsp_types::Url, Vec<async_lsp::lsp_types::Diagnostic>>::default(
            );
        for file in files.iter() {
            let top_mod = map_file_to_mod(self, *file);
            let diagnostics = self.analyze_top_mod(top_mod);
            let mut finalized_diags: Vec<CompleteDiagnostic> =
                diagnostics.iter().map(|d| d.to_complete(self)).collect();
            finalized_diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
                std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
                ord => ord,
            });
            for diag in finalized_diags {
                let lsp_diags = diag_to_lsp(diag, self.as_input_db()).clone();
                for (uri, more_diags) in lsp_diags {
                    let diags = result.entry(uri.clone()).or_insert_with(Vec::new);
                    diags.extend(more_diags);
                }
            }
        }
        result
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
