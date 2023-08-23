use common::{diagnostics::{Severity, CompleteDiagnostic, Span}, InputDb};
use hir::{hir_def::scope_graph::ScopeId, span::LazySpan, SpannedHirDb};
use log::error;
use lsp_types::Position;


pub(crate) fn calculate_line_offsets(text: &str) -> Vec<usize> {
    text.lines()
        .scan(0, |state, line| {
            let offset = *state;
            *state += line.len() + 1;
            Some(offset)
        })
        .collect()
}

pub(crate) fn to_offset_from_position(position: Position, text: &str) -> rowan::TextSize {
    let line_offsets: Vec<usize> = calculate_line_offsets(text);
    let line_offset = line_offsets[position.line as usize];
    let character_offset = position.character as usize;

    rowan::TextSize::from((line_offset + character_offset) as u32)
}

pub(crate) fn to_lsp_range_from_span(span: Span, db: &dyn InputDb) -> Result<lsp_types::Range, Box<dyn std::error::Error>> {
    let text = span.file.text(db);
    let line_offsets = calculate_line_offsets(text);

    let start_line = line_offsets
        .binary_search(&span.range.start().into())
        .map_err(|_| "Failed to find start line")?;

    let end_line = line_offsets
        .binary_search(&span.range.end().into())
        .map_err(|_| "Failed to find end line")?;

    let start_character: usize = usize::from(span.range.start()) - line_offsets[start_line];
    let end_character: usize = usize::from(span.range.end()) - line_offsets[end_line];

    Ok(lsp_types::Range {
        start: Position::new(start_line as u32, start_character as u32),
        end: Position::new(end_line as u32, end_character as u32),
    })
}

pub(crate) fn to_lsp_location_from_scope(scope: ScopeId, db: &dyn SpannedHirDb) -> Result<lsp_types::Location, Box<dyn std::error::Error>> {
    let lazy_span = scope.name_span(db.as_hir_db()).ok_or("Failed to get name span")?;
    let span = lazy_span.resolve(db.as_spanned_hir_db()).ok_or("Failed to resolve span")?;
    let uri = span.file.abs_path(db.as_input_db());
    let range = to_lsp_range_from_span(span, db.as_input_db())?;
    let uri = lsp_types::Url::from_file_path(uri).map_err(|_| "Failed to convert path to URL")?;
    Ok(lsp_types::Location { uri, range })
}

pub(crate) fn severity_to_lsp(severity: Severity) -> lsp_types::DiagnosticSeverity {
    match severity {
        Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
        Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
        Severity::Note => lsp_types::DiagnosticSeverity::HINT,
    }
}

pub(crate) fn diag_to_lsp(diag: CompleteDiagnostic, db: &dyn InputDb) -> Vec<lsp_types::Diagnostic> {
    diag.sub_diagnostics
        .into_iter()
        .map(|sub| {
            let lsp_range = to_lsp_range_from_span(sub.span.unwrap(), db);
            
            match lsp_range {
                Ok(range) => 
                    Some(lsp_types::Diagnostic {
                        range,
                        severity: Some(severity_to_lsp(diag.severity)),
                        code: None,
                        source: None,
                        message: sub.message.clone(),
                        related_information: None,
                        tags: None,
                        code_description: None,
                        data: None // for code actions
                    }), 
                Err(_) => {
                    error!("Failed to convert span to range");
                    None 
                }
            }
        })
        .filter_map(|x| x)
        .collect()
    }