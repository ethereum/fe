use parser::TextRange;

use crate::InputFile;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompleteDiagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
    pub sub_diagnostics: Vec<SubDiagnostic>,
    pub error_code: GlobalErrorCode,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlobalErrorCode {
    pub pass: AnalysisPass,
    pub local_code: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SubDiagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub file: InputFile,
    pub range: TextRange,
}

impl Span {
    pub fn new(file: InputFile, range: TextRange) -> Self {
        Self { file, range }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Severity {
    Error,
    Warning,
    Note,
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
