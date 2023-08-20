use common::diagnostics::{Severity, CompleteDiagnostic, Span};
use lsp_types::Position;

pub(crate) fn position_to_offset(position: Position, text: &str) -> rowan::TextSize {
    let line_offsets: Vec<usize> = text
        .lines()
        .scan(0, |state, line| {
            let offset = *state;
            *state += line.len() + 1;
            Some(offset)
        })
        .collect();

    let line_offset = line_offsets[position.line as usize];
    let character_offset = position.character as usize;

    rowan::TextSize::from((line_offset + character_offset) as u32)
}


pub(crate) fn span_to_range(span: Span, text: &str) -> lsp_types::Range {
    let line_offsets: Vec<usize> = text
        .lines()
        .scan(0, |state, line| {
            let offset = *state;
            *state += line.len() + 1;
            Some(offset)
        })
        .collect();

    let start_line = line_offsets
        .binary_search(&span.range.start().into())
        .unwrap_or_else(|x| x - 1);

    let end_line = line_offsets
        .binary_search(&span.range.end().into())
        .unwrap_or_else(|x| x - 1);

    let start_character: usize = usize::from(span.range.start()) - line_offsets[start_line];
    let end_character: usize = usize::from(span.range.end()) - line_offsets[end_line];

    lsp_types::Range {
        start: Position::new(start_line as u32, start_character as u32),
        end: Position::new(end_line as u32, end_character as u32),
    }
}

pub(crate) fn severity_to_lsp(severity: Severity) -> lsp_types::DiagnosticSeverity {
    match severity {
        // Severity::Bug => lsp_types::DiagnosticSeverity::ERROR,
        Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
        Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
        Severity::Note => lsp_types::DiagnosticSeverity::HINT,
        // Severity::Help => lsp_types::DiagnosticSeverity::INFORMATION,
    }
}

pub(crate) fn diag_to_lsp(diag: CompleteDiagnostic, text: &str) -> Vec<lsp_types::Diagnostic> {
        diag.sub_diagnostics
            .into_iter()
            .map(|sub| {
                let range = span_to_range(sub.span.unwrap(), text);
                lsp_types::Diagnostic {
                    range,
                    severity: Some(severity_to_lsp(diag.severity)),
                    code: None,
                    source: None,
                    message: diag.message.clone(),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None
                }
            })
            .collect()
    }