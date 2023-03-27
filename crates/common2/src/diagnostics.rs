use parser::ast::SyntaxNodePtr;

use crate::InputFile;

pub struct CompleteDiagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
    pub sub_diagnostics: Vec<SubDiagnostic>,
    pub error_code: GlobalErrorCode,
}

pub struct GlobalErrorCode {
    pub pass: AnalysisPass,
    pub local_code: u16,
}

pub struct SubDiagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file: InputFile,
    pub node: SyntaxNodePtr,
    pub origin: SpanOrigin,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpanOrigin {
    Raw,
    Expanded,
    Desugared,
}

#[repr(u16)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnalysisPass {
    Parse = 1,
    NameResolution,
    TyCheck,

    ExternalAnalysis(ExternalAnalysisKey) = u16::MAX,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternalAnalysisKey {
    name: String,
}
