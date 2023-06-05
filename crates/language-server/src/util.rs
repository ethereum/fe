use fe_common::diagnostics::{Severity, Diagnostic};
use fe_common::Span;
use lsp_types::Position;

// TODO: these could potentially be moved into the common crate
// for more idiomatic use in the analyzer and the language server

pub(crate) fn span_to_range(span: Span, text: &str) -> lsp_types::Range {
    // we need to get line and character offsets from the text,
    // first we get the line offsets
    let line_offsets: Vec<usize> = text
        .lines()
        .scan(0, |state, line| {
            let offset = *state;
            *state += line.len() + 1;
            Some(offset)
        })
        .collect();

    // now we get the line and character offsets
    let start_line = line_offsets
        .binary_search(&span.start)
        .unwrap_or_else(|x| x - 1);

    let end_line = line_offsets
        .binary_search(&span.end)
        .unwrap_or_else(|x| x - 1);

    let start_character = span.start - line_offsets[start_line];
    let end_character = span.end - line_offsets[end_line];

    lsp_types::Range {
        start: Position::new(start_line as u32, start_character as u32),
        end: Position::new(end_line as u32, end_character as u32),
    }
}
pub(crate) fn severity_to_lsp(severity: Severity) -> lsp_types::DiagnosticSeverity {
    match severity {
        Severity::Bug => lsp_types::DiagnosticSeverity::ERROR,
        Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
        Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
        Severity::Note => lsp_types::DiagnosticSeverity::HINT,
        Severity::Help => lsp_types::DiagnosticSeverity::INFORMATION,
    }
}

pub(crate) fn diag_to_lsp(diag: Diagnostic, text: &str) -> Vec<lsp_types::Diagnostic> {
        diag.labels
            .into_iter()
            .map(|label| {
                let range = span_to_range(label.span, text);
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