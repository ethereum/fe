use common::diagnostics::{
    AnalysisPass, CompleteDiagnostic, GlobalErrorCode, Severity, SubDiagnostic,
};
use hir::{
    diagnostics::DiagnosticVoucher,
    hir_def::IdentId,
    span::{DynLazySpan, LazySpan},
    HirDb,
};

use super::name_resolver::NameRes;

#[salsa::accumulator]
pub struct ImportErrorAccumulator(ImportError);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportError {
    span: DynLazySpan,
    kind: ImportErrorKind,
}

impl ImportError {
    pub fn new(span: DynLazySpan, kind: ImportErrorKind) -> Self {
        Self { span, kind }
    }

    pub fn conflict(span: DynLazySpan, conflict_with: DynLazySpan) -> Self {
        Self::new(span, ImportErrorKind::Conflict(conflict_with))
    }

    pub fn not_found(span: DynLazySpan, ident: IdentId) -> Self {
        Self::new(span, ImportErrorKind::NotFound(ident))
    }

    pub fn invisible(span: DynLazySpan, resolved: NameRes) -> Self {
        Self::new(span, ImportErrorKind::Invisible(resolved))
    }

    pub fn ambiguous(span: DynLazySpan, ident: IdentId) -> Self {
        Self::new(span, ImportErrorKind::Ambiguous(ident))
    }
}

impl DiagnosticVoucher for ImportError {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(AnalysisPass::ImportResolution, self.kind.local_code())
    }

    fn to_complete(self, db: &dyn hir::SpannedHirDb) -> CompleteDiagnostic {
        let span = self.span.resolve(db);
        let message = self.kind.message(db.as_hir_db());
        let sub_diags = self.kind.sub_diagnostics(db);

        CompleteDiagnostic::new(
            self.kind.severity(),
            message,
            span,
            sub_diags,
            self.error_code(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImportErrorKind {
    /// The import conflicts with another import.
    Conflict(DynLazySpan),
    /// The import path segment is not found.
    NotFound(IdentId),

    /// The import path segment is not visible.
    Invisible(NameRes),

    /// The import path segment is ambiguous.
    Ambiguous(IdentId),
}

impl ImportErrorKind {
    fn local_code(&self) -> u16 {
        match self {
            ImportErrorKind::Conflict(_) => 0,
            ImportErrorKind::NotFound(_) => 1,
            ImportErrorKind::Invisible(..) => 2,
            ImportErrorKind::Ambiguous(_) => 3,
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn message(&self, db: &dyn HirDb) -> String {
        match self {
            ImportErrorKind::Conflict(_) => "import conflicts with another import".to_string(),
            ImportErrorKind::NotFound(name) => format!("{} is not found", name.data(db)),
            ImportErrorKind::Invisible(resolved) => {
                format!(
                    "{} is not visible",
                    resolved.scope.name(db).unwrap().data(db)
                )
            }
            ImportErrorKind::Ambiguous(name) => format!("{} is ambiguous", name.data(db)),
        }
    }

    fn sub_diagnostics(&self, db: &dyn hir::SpannedHirDb) -> Vec<SubDiagnostic> {
        match self {
            ImportErrorKind::Conflict(conflict_with) => vec![SubDiagnostic::new(
                Severity::Note,
                "conflicts with this import".to_string(),
                conflict_with.resolve(db),
            )],

            ImportErrorKind::NotFound(_) | ImportErrorKind::Ambiguous(_) => vec![],

            ImportErrorKind::Invisible(resolved) => {
                let span = resolved.scope.name_span(db.as_hir_db()).unwrap();
                vec![SubDiagnostic::new(
                    Severity::Note,
                    "not visible because of this declaration".to_string(),
                    span.resolve(db),
                )]
            }
        }
    }
}
