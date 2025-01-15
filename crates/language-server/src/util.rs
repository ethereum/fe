use async_lsp::lsp_types::{DiagnosticSeverity, NumberOrString, Position};
use common::{
    diagnostics::{CompleteDiagnostic, Severity, Span},
    InputDb, InputFile, InputIngot,
};
use fxhash::FxHashMap;
use hir::{hir_def::scope_graph::ScopeId, span::LazySpan, SpannedHirDb};
use tracing::error;
use url::Url;

pub fn calculate_line_offsets(text: &str) -> Vec<usize> {
    text.lines()
        .scan(0, |state, line| {
            let offset = *state;
            *state += line.len() + 1;
            Some(offset)
        })
        .collect()
}

pub fn to_offset_from_position(position: Position, text: &str) -> rowan::TextSize {
    let line_offsets: Vec<usize> = calculate_line_offsets(text);
    let line_offset = line_offsets[position.line as usize];
    let character_offset = position.character as usize;

    rowan::TextSize::from((line_offset + character_offset) as u32)
}

pub fn to_lsp_range_from_span(
    span: Span,
    db: &dyn InputDb,
) -> Result<async_lsp::lsp_types::Range, Box<dyn std::error::Error>> {
    let text = span.file.text(db);
    let line_offsets = calculate_line_offsets(text);
    let start = span.range.start();
    let end = span.range.end();

    let start_line = line_offsets
        .binary_search(&start.into())
        .unwrap_or_else(|x| x - 1);

    let end_line = line_offsets
        .binary_search(&end.into())
        .unwrap_or_else(|x| x - 1);

    let start_character: usize = usize::from(span.range.start()) - line_offsets[start_line];
    let end_character: usize = usize::from(span.range.end()) - line_offsets[end_line];

    Ok(async_lsp::lsp_types::Range {
        start: Position::new(start_line as u32, start_character as u32),
        end: Position::new(end_line as u32, end_character as u32),
    })
}

pub fn to_lsp_location_from_scope(
    db: &dyn SpannedHirDb,
    ingot: InputIngot,
    file: InputFile,
    scope: ScopeId,
) -> Result<async_lsp::lsp_types::Location, Box<dyn std::error::Error>> {
    let lazy_span = scope
        .name_span(db.as_hir_db())
        .ok_or("Failed to get name span")?;
    let span = lazy_span
        .resolve(db.as_spanned_hir_db())
        .ok_or("Failed to resolve span")?;
    let uri = file.abs_path(db.as_input_db(), ingot);
    let range = to_lsp_range_from_span(span, db.as_input_db())?;
    let uri = async_lsp::lsp_types::Url::from_file_path(uri)
        .map_err(|()| "Failed to convert path to URL")?;
    Ok(async_lsp::lsp_types::Location { uri, range })
}

pub fn severity_to_lsp(is_primary: bool, severity: Severity) -> DiagnosticSeverity {
    // We set the severity to `HINT` for a secondary diags.
    if !is_primary {
        return DiagnosticSeverity::HINT;
    };

    match severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Note => DiagnosticSeverity::INFORMATION,
    }
}

pub fn diag_to_lsp(
    db: &dyn InputDb,
    ingot: InputIngot,
    diag: CompleteDiagnostic,
) -> FxHashMap<async_lsp::lsp_types::Url, Vec<async_lsp::lsp_types::Diagnostic>> {
    let mut result = FxHashMap::default();

    // TODO: this assumes that all sub_diagnostics point at files in the same ingot,
    // which might not be the case

    diag.sub_diagnostics.into_iter().for_each(|sub| {
        let is_primary = sub.is_primary();
        let Some(span) = sub.span else {
            if is_primary {
                error!("Diagnostic primary span is missing")
            }
            return;
        };

        let uri = span.file.abs_path(db, ingot);
        let uri = match Url::from_file_path(uri) {
            Ok(uri) => uri,
            Err(()) => {
                error!("Failed to convert path to URL");
                return;
            }
        };

        let range = match to_lsp_range_from_span(span, db) {
            Ok(range) => range,
            Err(e) => {
                error!("Error getting diagnostic: {} (ignoring diagnostic)", e);
                return;
            }
        };

        let code = is_primary.then(|| NumberOrString::String(diag.error_code.to_string()));
        let message = if is_primary {
            diag.message.to_string() + "\n" + &sub.message
        } else {
            sub.message
        };

        let diags = result.entry(uri).or_insert_with(Vec::new);
        diags.push(async_lsp::lsp_types::Diagnostic {
            range,
            // We set the severity to `HINT` for secondary diags.
            severity: Some(severity_to_lsp(is_primary, diag.severity)),
            code,
            source: None,
            message,
            related_information: None,
            tags: None,
            code_description: None,
            data: None,
        });
    });

    result
}

#[cfg(target_arch = "wasm32")]
use std::path::Path;

#[cfg(target_arch = "wasm32")]
pub trait DummyFilePathConversion {
    fn to_file_path(&self) -> Result<std::path::PathBuf, ()>;
    fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Url, ()>;
}

#[cfg(target_arch = "wasm32")]
impl DummyFilePathConversion for async_lsp::lsp_types::Url {
    fn to_file_path(&self) -> Result<std::path::PathBuf, ()> {
        // for now we don't support file paths on wasm
        Err(())
    }
    fn from_file_path<P: AsRef<Path>>(_path: P) -> Result<Url, ()> {
        // for now we don't support file paths on wasm
        Err(())
    }
}
