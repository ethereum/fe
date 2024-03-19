use std::ops::Range;

use camino::Utf8Path;
use clap::Error;
use codespan_reporting as cs;
use cs::{diagnostic as cs_diag, files as cs_files};

use common::{
    diagnostics::{LabelStyle, Severity},
    InputDb, InputFile,
};
use fxhash::FxHashMap;
use hir::{diagnostics::DiagnosticVoucher, LowerHirDb};
use salsa::Snapshot;

use crate::{
    db::{LanguageServerDatabase, LanguageServerDb},
    util::diag_to_lsp,
    workspace::{IngotFileContext, Workspace},
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

fn run_diagnostics(
    db: &Snapshot<LanguageServerDatabase>,
    workspace: &Workspace,
    path: &str,
) -> Vec<common::diagnostics::CompleteDiagnostic> {
    let file_path = path;
    let top_mod = workspace.top_mod_from_file_path(db.as_lower_hir_db(), file_path).unwrap();
    let diags = &db.analyze_top_mod(top_mod);
    db.finalize_diags(diags)
}

pub fn get_diagnostics(
    db: &Snapshot<LanguageServerDatabase>,
    workspace: &Workspace,
    uri: lsp_types::Url,
) -> Result<FxHashMap<lsp_types::Url, Vec<lsp_types::Diagnostic>>, Error> {
    let diags = run_diagnostics(db, workspace, uri.to_file_path().unwrap().to_str().unwrap());

    let diagnostics = diags
        .into_iter()
        .flat_map(|diag| diag_to_lsp(diag, db.as_input_db()).clone());

    // we need to reduce the diagnostics to a map from URL to Vec<Diagnostic>
    let mut result = FxHashMap::<lsp_types::Url, Vec<lsp_types::Diagnostic>>::default();

    // add a null diagnostic to the result for the given URL
    let _ = result.entry(uri.clone()).or_insert_with(Vec::new);

    diagnostics.for_each(|(uri, more_diags)| {
        let diags = result.entry(uri).or_insert_with(Vec::new);
        diags.extend(more_diags);
    });

    Ok(result)
}
