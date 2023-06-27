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
pub struct NameResolutionDiagAccumulator(pub(super) NameResDiag);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameResDiag {
    pub span: DynLazySpan,
    pub kind: NameResErrorKind,
}

impl NameResDiag {
    pub fn new(span: DynLazySpan, kind: NameResErrorKind) -> Self {
        Self { span, kind }
    }

    pub fn conflict(span: DynLazySpan, name: IdentId, conflict_with: DynLazySpan) -> Self {
        Self::new(span, NameResErrorKind::Conflict(name, conflict_with))
    }

    pub fn not_found(span: DynLazySpan, ident: IdentId) -> Self {
        Self::new(span, NameResErrorKind::NotFound(ident))
    }

    pub fn invisible(db: &dyn HirAnalysisDb, span: DynLazySpan, resolved: NameRes) -> Self {
        let name = resolved.kind.name(db).unwrap();
        let name_span = resolved.kind.name_span(db);
        Self::new(span, NameResErrorKind::Invisible(name, name_span))
    }

    pub fn ambiguous(span: DynLazySpan, ident: IdentId, candidates: Vec<DynLazySpan>) -> Self {
        Self::new(span, NameResErrorKind::Ambiguous(ident, candidates))
    }
}

impl DiagnosticVoucher for NameResDiag {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(AnalysisPass::NameResolution, self.kind.local_code())
    }

    fn to_complete(&self, db: &dyn hir::SpannedHirDb) -> CompleteDiagnostic {
        let error_code = self.error_code();
        let message = self.kind.message(db.as_hir_db());
        let sub_diags = self.kind.sub_diagnostics(db, self.span.clone());

        CompleteDiagnostic::new(self.kind.severity(), message, sub_diags, vec![], error_code)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NameResErrorKind {
    /// The import conflicts with another import.
    Conflict(IdentId, DynLazySpan),
    /// The import path segment is not found.
    NotFound(IdentId),

    /// The import path segment is not visible.
    Invisible(IdentId, Option<DynLazySpan>),

    /// The import path segment is ambiguous.
    Ambiguous(IdentId, Vec<DynLazySpan>),
}

impl NameResErrorKind {
    fn local_code(&self) -> u16 {
        match self {
            NameResErrorKind::Conflict(..) => 0,
            NameResErrorKind::NotFound(_) => 1,
            NameResErrorKind::Invisible(..) => 2,
            NameResErrorKind::Ambiguous(..) => 3,
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn message(&self, db: &dyn HirDb) -> String {
        match self {
            NameResErrorKind::Conflict(name, _) => {
                format!("{} conflicts with other definitions", name.data(db))
            }
            NameResErrorKind::NotFound(name) => format!("{} is not found", name.data(db)),
            NameResErrorKind::Invisible(name, _) => {
                format!("{} is not visible", name.data(db),)
            }
            NameResErrorKind::Ambiguous(name, _) => format!("{} is ambiguous", name.data(db)),
        }
    }

    fn sub_diagnostics(
        &self,
        db: &dyn hir::SpannedHirDb,
        prim_span: DynLazySpan,
    ) -> Vec<SubDiagnostic> {
        match self {
            NameResErrorKind::Conflict(ident, conflict_with) => {
                let ident = ident.data(db.as_hir_db());
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format! {"`{ident}` conflicts with another definition"},
                        prim_span.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format! {"{ident} redefined here "},
                        conflict_with.resolve(db),
                    ),
                ]
            }

            NameResErrorKind::NotFound(ident) => {
                let ident = ident.data(db.as_hir_db());
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{ident}` is not found"),
                    prim_span.resolve(db),
                )]
            }

            NameResErrorKind::Invisible(ident, span) => {
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

            NameResErrorKind::Ambiguous(ident, candidates) => {
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
