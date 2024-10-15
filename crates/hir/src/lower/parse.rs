use common::{
    diagnostics::{
        CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, Span, SpanKind,
        SubDiagnostic,
    },
    InputFile,
};
use parser::GreenNode;

use crate::{diagnostics::DiagnosticVoucher, hir_def::TopLevelMod, HirDb, SpannedHirDb};

#[salsa::tracked]
pub(crate) fn parse_file_impl<'db>(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> GreenNode {
    let file = top_mod.file(db);
    let text = file.text(db.as_input_db());
    let (node, parse_errors) = parser::parse_source_file(text);

    for error in parse_errors {
        ParseErrorAccumulator::push(db, ParserError { file, error });
    }
    node
}

#[doc(hidden)]
#[salsa::accumulator]
pub struct ParseErrorAccumulator(ParserError);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParserError {
    file: InputFile,
    error: parser::ParseError,
}

// `ParseError` has span information, but this is not a problem because the
// parsing procedure itself depends on the file content, and thus span
// information.
impl<'db> DiagnosticVoucher<'db> for ParserError {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(DiagnosticPass::Parse, 1)
    }

    fn to_complete(&self, _db: &'db dyn SpannedHirDb) -> CompleteDiagnostic {
        let error_code = self.error_code();
        let span = Span::new(self.file, self.error.range(), SpanKind::Original);
        CompleteDiagnostic::new(
            Severity::Error,
            self.error.msg(),
            vec![SubDiagnostic::new(
                LabelStyle::Primary,
                self.error.label(),
                Some(span),
            )],
            vec![],
            error_code,
        )
    }

    // fn clone_box(&self) -> Box<dyn DiagnosticVoucher> {
    //     Box::new(self.clone())
    // }
}
