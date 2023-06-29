use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::{
    diagnostics::DiagnosticVoucher,
    hir_def::{IdentId, TopLevelMod},
    span::{DynLazySpan, LazySpan},
    HirDb,
};

use crate::HirAnalysisDb;

use super::name_resolver::NameRes;

#[salsa::accumulator]
pub struct NameResolutionDiagAccumulator(pub(super) NameResDiag);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NameResDiag {
    /// The definition conflicts with other definitions.
    Conflict(IdentId, Vec<DynLazySpan>),

    /// The name is not found.
    NotFound(DynLazySpan, IdentId),

    /// The resolved name is not visible.
    Invisible(DynLazySpan, IdentId, Option<DynLazySpan>),

    /// The resolved name is ambiguous.
    Ambiguous(DynLazySpan, IdentId, Vec<DynLazySpan>),
}

impl NameResDiag {
    pub fn conflict(name: IdentId, conflict_with: Vec<DynLazySpan>) -> Self {
        Self::Conflict(name, conflict_with)
    }

    pub fn not_found(span: DynLazySpan, ident: IdentId) -> Self {
        Self::NotFound(span, ident)
    }

    pub fn invisible(db: &dyn HirAnalysisDb, span: DynLazySpan, resolved: NameRes) -> Self {
        let name = resolved.kind.name(db).unwrap();
        let name_span = resolved.kind.name_span(db);
        Self::Invisible(span, name, name_span)
    }

    pub fn ambiguous(span: DynLazySpan, ident: IdentId, candidates: Vec<DynLazySpan>) -> Self {
        Self::Ambiguous(span, ident, candidates)
    }

    // Returns the top-level module where the diagnostic is located.
    pub fn top_mod(&self, db: &dyn HirAnalysisDb) -> TopLevelMod {
        match self {
            Self::Conflict(_, conflicts) => conflicts
                .iter()
                .filter_map(|span| span.top_mod(db.as_hir_db()))
                .min()
                .unwrap(),
            Self::NotFound(span, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::Invisible(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::Ambiguous(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
        }
    }

    fn local_code(&self) -> u16 {
        match self {
            Self::Conflict(..) => 1,
            Self::NotFound(..) => 2,
            Self::Invisible(..) => 3,
            Self::Ambiguous(..) => 4,
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn message(&self, db: &dyn HirDb) -> String {
        match self {
            Self::Conflict(name, _) => {
                format!("`{}` conflicts with other definitions", name.data(db))
            }
            Self::NotFound(_, name) => format!("`{}` is not found", name.data(db)),
            Self::Invisible(_, name, _) => {
                format!("`{}` is not visible", name.data(db),)
            }
            Self::Ambiguous(_, name, _) => format!("`{}` is ambiguous", name.data(db)),
        }
    }

    fn sub_diagnostics(&self, db: &dyn hir::SpannedHirDb) -> Vec<SubDiagnostic> {
        match self {
            Self::Conflict(ident, conflicts) => {
                let ident = ident.data(db.as_hir_db());
                let mut diags = Vec::with_capacity(conflicts.len());
                let mut spans: Vec<_> = conflicts
                    .iter()
                    .filter_map(|span| span.resolve(db))
                    .collect();
                spans.sort_unstable();
                let mut spans = spans.into_iter();

                diags.push(SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{ident}` is defined here"),
                    spans.next(),
                ));
                for sub_span in spans {
                    diags.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format! {"`{ident}` is redefined here"},
                        Some(sub_span),
                    ));
                }

                diags
            }

            Self::NotFound(prim_span, ident) => {
                let ident = ident.data(db.as_hir_db());
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{ident}` is not found"),
                    prim_span.resolve(db),
                )]
            }

            Self::Invisible(prim_span, ident, span) => {
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

            Self::Ambiguous(prim_span, ident, candidates) => {
                let ident = ident.data(db.as_hir_db());
                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{ident}` is ambiguous"),
                    prim_span.resolve(db),
                )];
                let mut cand_spans: Vec<_> = candidates
                    .iter()
                    .filter_map(|span| span.resolve(db))
                    .collect();
                cand_spans.sort_unstable();
                diags.extend(cand_spans.into_iter().enumerate().map(|(i, span)| {
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("candidate `#{i}`"),
                        Some(span),
                    )
                }));

                diags
            }
        }
    }
}

impl DiagnosticVoucher for NameResDiag {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(DiagnosticPass::NameResolution, self.local_code())
    }

    fn to_complete(&self, db: &dyn hir::SpannedHirDb) -> CompleteDiagnostic {
        let error_code = self.error_code();
        let message = self.message(db.as_hir_db());
        let sub_diags = self.sub_diagnostics(db);

        CompleteDiagnostic::new(self.severity(), message, sub_diags, vec![], error_code)
    }
}
