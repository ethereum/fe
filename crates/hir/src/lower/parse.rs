use common::{
    diagnostics::{
        AnalysisPass, CompleteDiagnostic, GlobalErrorCode, LabelStyle, Severity, Span, SpanKind,
        SubDiagnostic,
    },
    InputFile,
};
use parser::GreenNode;

use crate::{diagnostics::DiagnosticVoucher, hir_def::TopLevelMod, HirDb, SpannedHirDb};

#[salsa::tracked]
pub(crate) fn parse_file_impl(db: &dyn HirDb, top_mod: TopLevelMod) -> GreenNode {
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
impl DiagnosticVoucher for ParserError {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(AnalysisPass::Parse, 0)
    }

    fn to_complete(&self, _db: &dyn SpannedHirDb) -> CompleteDiagnostic {
        let error_code = self.error_code();
        let span = Span::new(self.file, self.error.range, SpanKind::Original);
        CompleteDiagnostic::new(
            Severity::Error,
            self.error.msg.clone(),
            vec![SubDiagnostic::new(
                LabelStyle::Primary,
                self.error.msg.clone(),
                Some(span),
            )],
            vec![],
            error_code,
        )
    }
}
