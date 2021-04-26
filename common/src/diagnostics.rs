use crate::files::{FileStore, SourceFileId};
pub use codespan_reporting::diagnostic::{Diagnostic as CsDiagnostic, Label, LabelStyle, Severity};
use codespan_reporting::term;
use term::termcolor::{BufferWriter, ColorChoice};

pub type Diagnostic = CsDiagnostic<SourceFileId>;

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
        term::emit(&mut buffer, &config, files, &diag).unwrap();
    }
    std::str::from_utf8(buffer.as_slice()).unwrap().to_string()
}
