use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::{
    diagnostics::DiagnosticVoucher,
    span::{DynLazySpan, LazySpan},
};

use crate::HirAnalysisDb;

use super::ty::{AdtRefId, TyId};

#[salsa::accumulator]
pub struct AdtDefDiagAccumulator(pub(super) TyLowerDiag);
#[salsa::accumulator]
pub struct TypeAliasDefDiagAccumulator(pub(super) TyLowerDiag);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyLowerDiag {
    InvalidType(DynLazySpan),
    NotFullyAppliedType(DynLazySpan),
    KindMismatch(DynLazySpan, String),
    RecursiveType {
        primary_span: DynLazySpan,
        cycle_participants: Vec<DynLazySpan>,
    },
    AssocTy(DynLazySpan),
}

impl TyLowerDiag {
    pub(super) fn invalid_type(span: DynLazySpan) -> Self {
        Self::InvalidType(span)
    }

    pub fn not_fully_applied_type(span: DynLazySpan) -> Self {
        Self::NotFullyAppliedType(span)
    }

    pub fn kind_mismatch(db: &dyn HirAnalysisDb, abs: TyId, arg: TyId, span: DynLazySpan) -> Self {
        let k_abs = abs.kind(db);
        let k_arg = arg.kind(db);

        let msg = format!("can't apply `{}` kind to `{}` kind", k_arg, k_abs);
        Self::KindMismatch(span, msg.into())
    }

    pub(super) fn recursive_type(
        db: &dyn HirAnalysisDb,
        primary_span: DynLazySpan,
        participants: Vec<AdtRefId>,
    ) -> Self {
        let cycle_participants = participants.into_iter().map(|p| p.name_span(db)).collect();

        Self::RecursiveType {
            primary_span,
            cycle_participants,
        }
    }

    pub(super) fn assoc_ty(span: DynLazySpan) -> Self {
        Self::AssocTy(span)
    }

    fn local_code(&self) -> u16 {
        match self {
            Self::InvalidType(_) => 0,
            Self::NotFullyAppliedType(_) => 1,
            Self::KindMismatch(_, _) => 2,
            Self::RecursiveType { .. } => 3,
            Self::AssocTy(_) => 4,
        }
    }

    fn message(&self) -> String {
        match self {
            Self::InvalidType(_) => "expected type".to_string(),

            Self::NotFullyAppliedType(_) => "expected fully applied type".to_string(),

            Self::KindMismatch(_, _) => "kind mismatch in type application".to_string(),

            Self::RecursiveType { .. } => "recursive type is not allowed".to_string(),

            Self::AssocTy(_) => "associated type is not supported ".to_string(),
        }
    }

    fn sub_diags(&self, db: &dyn hir::SpannedHirDb) -> Vec<SubDiagnostic> {
        match self {
            Self::InvalidType(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "expected type here".to_string(),
                span.resolve(db),
            )],

            Self::NotFullyAppliedType(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "expected fully applied type here".to_string(),
                span.resolve(db),
            )],

            Self::KindMismatch(span, msg) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                msg.clone(),
                span.resolve(db),
            )],

            Self::RecursiveType {
                primary_span,
                cycle_participants,
            } => {
                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "causing cycle here".to_string(),
                    primary_span.resolve(db),
                )];

                diags.extend(cycle_participants.iter().map(|span| {
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("this type is part of the cycle"),
                        span.resolve(db),
                    )
                }));

                diags
            }

            Self::AssocTy(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "associated type is not implemented".to_string(),
                span.resolve(db),
            )],
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }
}

impl DiagnosticVoucher for TyLowerDiag {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(DiagnosticPass::TypeDefinition, self.local_code())
    }

    fn to_complete(&self, db: &dyn hir::SpannedHirDb) -> CompleteDiagnostic {
        let severity = self.severity();
        let error_code = self.error_code();
        let message = self.message();
        let sub_diags = self.sub_diags(db);

        CompleteDiagnostic::new(severity, message, sub_diags, vec![], error_code)
    }
}
