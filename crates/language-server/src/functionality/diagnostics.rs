use std::ops::Range;

use camino::Utf8Path;

use codespan_reporting as cs;
use cs::{diagnostic as cs_diag, files as cs_files};

use common::{
    diagnostics::{CompleteDiagnostic, LabelStyle, Severity},
    InputDb, InputFile,
};

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

pub trait ToCsDiag {
    fn to_cs(&self, db: &LanguageServerDatabase) -> cs_diag::Diagnostic<InputFile>;
}

impl<T> ToCsDiag for T
where
    T: DiagnosticVoucher + Sync,
{
    fn to_cs(&self, db: &LanguageServerDatabase) -> cs_diag::Diagnostic<InputFile> {
        let complete = self.to_complete(db);

        let severity = convert_severity(complete.severity);
        let code = Some(complete.error_code.to_string());
        let message = complete.message;

        let labels = complete
            .sub_diagnostics
            .into_iter()
            .filter_map(|sub_diag| {
                let span = sub_diag.span?;
                match sub_diag.style {
                    LabelStyle::Primary => {
                        cs_diag::Label::new(cs_diag::LabelStyle::Primary, span.file, span.range)
                    }
                    LabelStyle::Secondary => {
                        cs_diag::Label::new(cs_diag::LabelStyle::Secondary, span.file, span.range)
                    }
                }
                .with_message(sub_diag.message)
                .into()
            })
            .collect();

        cs_diag::Diagnostic {
            severity,
            code,
            message,
            labels,
            notes: vec![],
        }
    }
}

fn convert_severity(severity: Severity) -> cs_diag::Severity {
    match severity {
        Severity::Error => cs_diag::Severity::Error,
        Severity::Warning => cs_diag::Severity::Warning,
        Severity::Note => cs_diag::Severity::Note,
    }
}

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

    pub fn get_lsp_diagnostics(
        &self,
        files: Vec<InputFile>,
    ) -> FxHashMap<async_lsp::lsp_types::Url, Vec<async_lsp::lsp_types::Diagnostic>> {
        let mut result =
            FxHashMap::<async_lsp::lsp_types::Url, Vec<async_lsp::lsp_types::Diagnostic>>::default(
            );
        files
            .iter()
            .flat_map(|file| {
                let top_mod = map_file_to_mod(self, *file);
                let diagnostics = self.analyze_top_mod(top_mod);
                self.finalize_diags(&diagnostics)
                    .into_iter()
                    .flat_map(|diag| diag_to_lsp(diag, self.as_input_db()).clone())
            })
            .for_each(|(uri, more_diags)| {
                let _ = result.entry(uri.clone()).or_insert_with(Vec::new);
                let diags = result.entry(uri).or_insert_with(Vec::new);
                diags.extend(more_diags);
            });
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
