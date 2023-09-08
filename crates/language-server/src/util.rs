use common::{
    diagnostics::{CompleteDiagnostic, Severity, Span},
    InputDb,
};
use hir::{hir_def::scope_graph::ScopeId, span::LazySpan, SpannedHirDb};
use log::error;
use lsp_types::Position;

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
) -> Result<lsp_types::Range, Box<dyn std::error::Error>> {
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

    Ok(lsp_types::Range {
        start: Position::new(start_line as u32, start_character as u32),
        end: Position::new(end_line as u32, end_character as u32),
    })
}

pub fn to_lsp_location_from_scope(
    scope: ScopeId,
    db: &dyn SpannedHirDb,
) -> Result<lsp_types::Location, Box<dyn std::error::Error>> {
    let lazy_span = scope
        .name_span(db.as_hir_db())
        .ok_or("Failed to get name span")?;
    let span = lazy_span
        .resolve(db.as_spanned_hir_db())
        .ok_or("Failed to resolve span")?;
    let uri = span.file.abs_path(db.as_input_db());
    let range = to_lsp_range_from_span(span, db.as_input_db())?;
    let uri = lsp_types::Url::from_file_path(uri).map_err(|_| "Failed to convert path to URL")?;
    Ok(lsp_types::Location { uri, range })
}

pub fn severity_to_lsp(severity: Severity) -> lsp_types::DiagnosticSeverity {
    match severity {
        Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
        Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
        Severity::Note => lsp_types::DiagnosticSeverity::HINT,
    }
}

pub fn diag_to_lsp(diag: CompleteDiagnostic, db: &dyn InputDb) -> Vec<lsp_types::Diagnostic> {
    diag.sub_diagnostics
        .into_iter()
        .filter_map(|sub| {
            let lsp_range = to_lsp_range_from_span(sub.span.unwrap(), db);

            match lsp_range {
                Ok(range) => Some(lsp_types::Diagnostic {
                    range,
                    severity: Some(severity_to_lsp(diag.severity)),
                    code: None,
                    source: None,
                    message: sub.message,
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None, // for code actions
                }),
                Err(_) => {
                    error!("Failed to convert span to range");
                    None
                }
            }
        })
        .collect()
}
