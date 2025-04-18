use std::ops::Range;

use camino::Utf8Path;
use codespan_reporting as cs;
use common::{
    diagnostics::CompleteDiagnostic,
    file::File,
    ingot::{IngotBaseUrl, IngotDescription},
};
use cs::files as cs_files;
use hir::lower::map_file_to_mod;
use hir_analysis::{
    analysis_pass::{AnalysisPassManager, ParsingPass},
    name_resolution::ImportAnalysisPass,
    ty::{
        AdtDefAnalysisPass, BodyAnalysisPass, DefConflictAnalysisPass, FuncAnalysisPass,
        ImplAnalysisPass, ImplTraitAnalysisPass, TraitAnalysisPass, TypeAliasAnalysisPass,
    },
};
use rustc_hash::FxHashMap;
use url::Url;

use crate::{
    backend::db::{LanguageServerDatabase, LanguageServerDb},
    util::diag_to_lsp,
};

#[salsa::tracked(return_ref)]
pub fn file_line_starts(db: &dyn LanguageServerDb, file: File) -> Vec<usize> {
    cs::files::line_starts(file.text(db)).collect()
}

impl<'a> cs_files::Files<'a> for LanguageServerDatabase {
    type FileId = File;
    type Name = &'a Utf8Path;
    type Source = &'a str;

    fn name(&'a self, file_id: Self::FileId) -> Result<Self::Name, cs_files::Error> {
        Ok(file_id
            .path(self)
            .as_ref()
            .expect("File path should be valid"))
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
    pub fn diagnostics_for_ingot(
        &self,
        ingot: IngotDescription,
    ) -> FxHashMap<async_lsp::lsp_types::Url, Vec<async_lsp::lsp_types::Diagnostic>> {
        let mut result =
            FxHashMap::<async_lsp::lsp_types::Url, Vec<async_lsp::lsp_types::Diagnostic>>::default(
            );
        let mut pass_manager = initialize_analysis_pass(self);
        let ingot_files = ingot.files(self);

        for (url, file) in ingot_files.iter() {
            // initialize an empty diagnostic list for this file
            // (to clear any previous diagnostics)
            result.entry(url).or_default();

            let top_mod = map_file_to_mod(self, file);
            let diagnostics = pass_manager.run_on_module(top_mod);
            let mut finalized_diags: Vec<CompleteDiagnostic> = diagnostics
                .iter()
                .map(|d| d.to_complete(self).clone())
                .collect();
            finalized_diags.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
                std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
                ord => ord,
            });
            for diag in finalized_diags {
                let lsp_diags = diag_to_lsp(self, ingot, diag).clone();
                for (uri, more_diags) in lsp_diags {
                    let diags = result.entry(uri.clone()).or_insert_with(Vec::new);
                    diags.extend(more_diags);
                }
            }
        }

        result
    }
}

fn initialize_analysis_pass() -> AnalysisPassManager {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass(Box::new(ParsingPass {}));
    pass_manager.add_module_pass(Box::new(DefConflictAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(ImportAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(AdtDefAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(TypeAliasAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(TraitAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(ImplAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(ImplTraitAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(FuncAnalysisPass {}));
    pass_manager.add_module_pass(Box::new(BodyAnalysisPass {}));

    pass_manager
}
