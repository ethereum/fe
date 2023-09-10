use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::{
    diagnostics::DiagnosticVoucher,
    span::{DynLazySpan, LazySpan},
};

use crate::HirAnalysisDb;

use super::ty::TyId;

#[salsa::accumulator]
pub struct StructDefDiagAccumulator(pub(super) TyLowerDiag);
#[salsa::accumulator]
pub struct EnumDefDiagAccumulator(pub(super) TyLowerDiag);
#[salsa::accumulator]
pub struct ContractDefDiagAccumulator(pub(super) TyLowerDiag);
#[salsa::accumulator]
pub struct TypeAliasDefDiagAccumulator(pub(super) TyLowerDiag);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyLowerDiag {
    InvalidType(DynLazySpan),
    NotFullyAppliedType(DynLazySpan),
    KindMismatch(DynLazySpan, String),
    AssocTy(DynLazySpan),
}

impl TyLowerDiag {
    pub(super) fn assoc_ty(span: DynLazySpan) -> Self {
        Self::AssocTy(span)
    }

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

    fn local_code(&self) -> u16 {
        match self {
            Self::InvalidType(_) => 0,
            Self::NotFullyAppliedType(_) => 1,
            Self::KindMismatch(_, _) => 2,
            Self::AssocTy(_) => 3,
        }
    }

    fn message(&self) -> String {
        match self {
            Self::InvalidType(_) => "expected type".to_string(),
            Self::NotFullyAppliedType(_) => "expected fully applied type".to_string(),
            Self::KindMismatch(_, _) => "kind mismatch in type application".to_string(),
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
