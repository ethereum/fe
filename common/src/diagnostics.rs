use crate::files::{FileStore, SourceFileId};
pub use codespan_reporting::diagnostic::{Diagnostic as CsDiagnostic, Label, LabelStyle, Severity};
use codespan_reporting::term;
use term::termcolor::{BufferWriter, ColorChoice};

pub type Diagnostic = CsDiagnostic<SourceFileId>;

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
