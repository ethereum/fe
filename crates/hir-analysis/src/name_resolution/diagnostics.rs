use common::diagnostics::{
    AnalysisPass, CompleteDiagnostic, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::{
    diagnostics::DiagnosticVoucher,
    hir_def::IdentId,
    span::{DynLazySpan, LazySpan},
    HirDb,
};

use crate::HirAnalysisDb;

use super::name_resolver::NameRes;

#[salsa::accumulator]
pub struct NameResolutionDiagAccumulator(NameResolutionDiag);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameResolutionDiag {
    span: DynLazySpan,
    kind: ImportErrorKind,
}

impl NameResolutionDiag {
    pub fn new(span: DynLazySpan, kind: ImportErrorKind) -> Self {
        Self { span, kind }
    }

    pub fn conflict(span: DynLazySpan, name: IdentId, conflict_with: DynLazySpan) -> Self {
        Self::new(span, ImportErrorKind::Conflict(name, conflict_with))
    }

    pub fn not_found(span: DynLazySpan, ident: IdentId) -> Self {
        Self::new(span, ImportErrorKind::NotFound(ident))
    }

    pub fn invisible(db: &dyn HirAnalysisDb, span: DynLazySpan, resolved: NameRes) -> Self {
        let name = resolved.kind.name(db).unwrap();
        let name_span = resolved.kind.name_span(db);
        Self::new(span, ImportErrorKind::Invisible(name, name_span))
    }

    pub fn ambiguous(span: DynLazySpan, ident: IdentId, candidates: Vec<DynLazySpan>) -> Self {
        Self::new(span, ImportErrorKind::Ambiguous(ident, candidates))
    }
}

impl DiagnosticVoucher for NameResolutionDiag {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(AnalysisPass::NameResolution, self.kind.local_code())
    }

    fn to_complete(self, db: &dyn hir::SpannedHirDb) -> CompleteDiagnostic {
        let error_code = self.error_code();
        let message = self.kind.message(db.as_hir_db());
        let sub_diags = self.kind.sub_diagnostics(db, self.span);

        CompleteDiagnostic::new(self.kind.severity(), message, sub_diags, vec![], error_code)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImportErrorKind {
    /// The import conflicts with another import.
    Conflict(IdentId, DynLazySpan),
    /// The import path segment is not found.
    NotFound(IdentId),

    /// The import path segment is not visible.
    Invisible(IdentId, Option<DynLazySpan>),

    /// The import path segment is ambiguous.
    Ambiguous(IdentId, Vec<DynLazySpan>),
}

impl ImportErrorKind {
    fn local_code(&self) -> u16 {
        match self {
            ImportErrorKind::Conflict(..) => 0,
            ImportErrorKind::NotFound(_) => 1,
            ImportErrorKind::Invisible(..) => 2,
            ImportErrorKind::Ambiguous(..) => 3,
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn message(&self, db: &dyn HirDb) -> String {
        match self {
            ImportErrorKind::Conflict(name, _) => {
                format!("{} conflicts with other definitions", name.data(db))
            }
            ImportErrorKind::NotFound(name) => format!("{} is not found", name.data(db)),
            ImportErrorKind::Invisible(name, _) => {
                format!("{} is not visible", name.data(db),)
            }
            ImportErrorKind::Ambiguous(name, _) => format!("{} is ambiguous", name.data(db)),
        }
    }

    fn sub_diagnostics(
        &self,
        db: &dyn hir::SpannedHirDb,
        prim_span: DynLazySpan,
    ) -> Vec<SubDiagnostic> {
        match self {
            ImportErrorKind::Conflict(ident, conflict_with) => {
                let ident = ident.data(db.as_hir_db());
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        "`{ident}` conflicts with another definition".to_string(),
                        prim_span.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "{ident} redefined here ".to_string(),
                        conflict_with.resolve(db),
                    ),
                ]
            }

            ImportErrorKind::NotFound(ident) => {
                let ident = ident.data(db.as_hir_db());
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{ident}` is not found"),
                    prim_span.resolve(db),
                )]
            }

            ImportErrorKind::Invisible(ident, span) => {
                let ident = ident.data(db.as_hir_db());

                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{ident}` is not visible"),
                    prim_span.resolve(db),
                )];
                if let Some(span) = span {
                    diags.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("`{ident} is defined here"),
                        span.resolve(db),
                    ))
                }
                diags
            }

            ImportErrorKind::Ambiguous(ident, candidates) => {
                let ident = ident.data(db.as_hir_db());
                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{ident}` is ambiguous"),
                    prim_span.resolve(db),
                )];
                diags.extend(candidates.iter().enumerate().map(|(i, span)| {
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("candidate #{i}"),
                        span.resolve(db),
                    )
                }));

                diags
            }
        }
    }
}
