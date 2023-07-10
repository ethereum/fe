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

use super::NameRes;

#[salsa::accumulator]
pub struct NameResolutionDiagAccumulator(pub(super) NameResDiag);

#[salsa::accumulator]
pub struct ImportResolutionDiagAccumulator(pub(super) NameResDiag);

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

    /// The name is found but belongs to a different name domain other than the
    /// Type.
    ExpectedType(DynLazySpan, IdentId, NameRes),

    /// The name is found but belongs to a different name domain other than the
    /// trait.
    ExpectedTrait(DynLazySpan, IdentId, NameRes),

    /// The name is found but belongs to a different name domain other than the
    /// value.
    ExpectedValue(DynLazySpan, IdentId, NameRes),
}

impl NameResDiag {
    /// Returns the top-level module where the diagnostic is located.
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
            Self::ExpectedType(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::ExpectedTrait(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::ExpectedValue(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
        }
    }

    pub(super) fn conflict(name: IdentId, conflict_with: Vec<DynLazySpan>) -> Self {
        Self::Conflict(name, conflict_with)
    }

    pub(super) fn not_found(span: DynLazySpan, ident: IdentId) -> Self {
        Self::NotFound(span, ident)
    }

    pub(super) fn invisible(
        span: DynLazySpan,
        name: IdentId,
        invisible_span: Option<DynLazySpan>,
    ) -> Self {
        Self::Invisible(span, name, invisible_span)
    }

    pub(super) fn ambiguous(
        db: &dyn HirAnalysisDb,
        span: DynLazySpan,
        ident: IdentId,
        cands: Vec<NameRes>,
    ) -> Self {
        let cands = cands
            .into_iter()
            .filter_map(|name| name.kind.name_span(db))
            .collect();
        Self::Ambiguous(span, ident, cands)
    }

    fn local_code(&self) -> u16 {
        match self {
            Self::Conflict(..) => 1,
            Self::NotFound(..) => 2,
            Self::Invisible(..) => 3,
            Self::Ambiguous(..) => 4,
            Self::ExpectedType(..) => 5,
            Self::ExpectedTrait(..) => 6,
            Self::ExpectedValue(..) => 7,
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
            Self::ExpectedType(_, _, _) => "expected type item here".to_string(),
            Self::ExpectedTrait(_, _, _) => "expected trait item here".to_string(),
            Self::ExpectedValue(_, _, _) => "expected value here".to_string(),
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

            Self::ExpectedType(prim_span, name, res) => {
                let res_kind_name = res.kind_name();
                let name = name.data(db.as_hir_db());
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("expected type here, but found {} `{}`", res_kind_name, name),
                    prim_span.resolve(db),
                )]
            }

            Self::ExpectedTrait(prim_span, name, res) => {
                let res_kind_name = res.kind_name();
                let name = name.data(db.as_hir_db());
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!(
                        "expected trait here, but found {} `{}`",
                        res_kind_name, name
                    ),
                    prim_span.resolve(db),
                )]
            }

            Self::ExpectedValue(prim_span, name, res) => {
                let res_kind_name = res.kind_name();
                let name = name.data(db.as_hir_db());
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!(
                        "expected value here, but found {} `{}`",
                        res_kind_name, name
                    ),
                    prim_span.resolve(db),
                )]
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
