use crate::files::{FileStore, SourceFileId};
use crate::Span;
pub use codespan_reporting::diagnostic::{
    Diagnostic as CsDiagnostic, Label as CsLabel, LabelStyle, Severity,
};
use codespan_reporting::term;
use term::termcolor::{BufferWriter, ColorChoice};

pub type Diagnostic = CsDiagnostic<SourceFileId>;

pub struct Label {
    pub style: LabelStyle,
    pub span: Span,
    pub message: String,
}
impl Label {
    /// Create a primary label with the given message. This will underline the
    /// given span with carets (`^^^^`).
    pub fn primary<S: Into<String>>(span: Span, message: S) -> Self {
        Label {
            style: LabelStyle::Primary,
            span,
            message: message.into(),
        }
    }

    /// Create a secondary label with the given message. This will underline the
    /// given span with hyphens (`----`).
    pub fn secondary<S: Into<String>>(span: Span, message: S) -> Self {
        Label {
            style: LabelStyle::Secondary,
            span,
            message: message.into(),
        }
    }

    /// Convert into a [`codespan_reporting::Diagnostic::Label`]
    pub fn into_cs_label(self, file_id: SourceFileId) -> CsLabel<SourceFileId> {
        CsLabel {
            style: self.style,
            file_id,
            range: self.span.into(),
            message: self.message,
        }
    }
}

/// Print the given diagnostics to stderr.
pub fn print_diagnostics(diagnostics: &[Diagnostic], files: &FileStore) {
    let writer = BufferWriter::stderr(ColorChoice::Auto);
    let mut buffer = writer.buffer();
    let config = term::Config::default();

    for diag in diagnostics {
        term::emit(&mut buffer, &config, files, &diag).unwrap();
    }
    // If we use `writer` here, the output won't be captured by rust's test system.
    eprintln!("{}", std::str::from_utf8(buffer.as_slice()).unwrap());
}

/// Format the given diagnostics as a string.
pub fn diagnostics_string(diagnostics: &[Diagnostic], files: &FileStore) -> String {
    let writer = BufferWriter::stderr(ColorChoice::Never);
    let mut buffer = writer.buffer();
    let config = term::Config::default();

    for diag in diagnostics {
        term::emit(&mut buffer, &config, files, &diag).expect("failed to emit diagnostic");
    }
    std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
}
