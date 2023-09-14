use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::{
    diagnostics::DiagnosticVoucher,
    hir_def::TypeAlias as HirTypeAlias,
    span::{DynLazySpan, LazySpan},
    HirDb,
};

use crate::HirAnalysisDb;

use super::ty::TyId;

#[salsa::accumulator]
pub struct AdtDefDiagAccumulator(pub(super) TyLowerDiag);
#[salsa::accumulator]
pub struct TypeAliasDefDiagAccumulator(pub(super) TyLowerDiag);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyLowerDiag {
    NotFullyAppliedType(DynLazySpan),
    TyAppFailed(DynLazySpan, String),
    KindMismatch(DynLazySpan, String),
    RecursiveType {
        primary_span: DynLazySpan,
        field_span: DynLazySpan,
    },

    TypeAliasArgumentMismatch {
        span: DynLazySpan,
        type_alias: HirTypeAlias,
        n_given_arg: usize,
    },
    TypeAliasCycle(DynLazySpan),

    AssocTy(DynLazySpan),
}

impl TyLowerDiag {
    pub fn not_fully_applied_type(span: DynLazySpan) -> Self {
        Self::NotFullyAppliedType(span)
    }

    pub fn ty_app_failed(db: &dyn HirAnalysisDb, span: DynLazySpan, abs: TyId, arg: TyId) -> Self {
        let k_abs = abs.kind(db);
        let k_arg = arg.kind(db);

        let msg = format!("can't apply `{}` kind to `{}` kind", k_arg, k_abs);
        Self::TyAppFailed(span, msg.into())
    }

    pub fn kind_mismatch(
        db: &dyn HirAnalysisDb,
        span: DynLazySpan,
        expected: TyId,
        actual: TyId,
    ) -> Self {
        debug_assert!(expected.kind(db) != actual.kind(db));

        let msg = format!(
            "expected `{}` kind, but found `{}` kind",
            expected.kind(db),
            actual.kind(db)
        );
        Self::KindMismatch(span, msg)
    }

    pub(super) fn recursive_type(primary_span: DynLazySpan, field_span: DynLazySpan) -> Self {
        Self::RecursiveType {
            primary_span,
            field_span,
        }
    }

    pub(super) fn type_alias_argument_mismatch(
        span: DynLazySpan,
        type_alias: HirTypeAlias,
        n_given_arg: usize,
    ) -> Self {
        Self::TypeAliasArgumentMismatch {
            span,
            type_alias,
            n_given_arg,
        }
    }

    pub(super) fn type_alias_cycle(span: DynLazySpan) -> Self {
        Self::TypeAliasCycle(span)
    }

    pub(super) fn assoc_ty(span: DynLazySpan) -> Self {
        Self::AssocTy(span)
    }

    fn local_code(&self) -> u16 {
        match self {
            Self::NotFullyAppliedType(_) => 0,
            Self::TyAppFailed(_, _) => 1,
            Self::KindMismatch(_, _) => 2,
            Self::RecursiveType { .. } => 3,
            Self::TypeAliasArgumentMismatch { .. } => 4,
            Self::TypeAliasCycle(_) => 5,
            Self::AssocTy(_) => 6,
        }
    }

    fn message(&self, db: &dyn HirDb) -> String {
        match self {
            Self::NotFullyAppliedType(_) => "expected fully applied type".to_string(),
            Self::TyAppFailed(_, _) => "kind mismatch in type application".to_string(),
            Self::KindMismatch(_, _) => "kind mismatch between two types".to_string(),
            Self::RecursiveType { .. } => "recursive type is not allowed".to_string(),

            Self::TypeAliasArgumentMismatch {
                type_alias,
                n_given_arg,
                ..
            } => format!(
                "type alias expects {} generic arguments, but {} given",
                type_alias.generic_params(db).len(db),
                n_given_arg
            ),
            Self::TypeAliasCycle(_) => "recursive type alias cycle is detected".to_string(),

            Self::AssocTy(_) => "associated type is not supported ".to_string(),
        }
    }

    fn sub_diags(&self, db: &dyn hir::SpannedHirDb) -> Vec<SubDiagnostic> {
        match self {
            Self::NotFullyAppliedType(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "expected fully applied type here".to_string(),
                span.resolve(db),
            )],

            Self::TyAppFailed(span, msg) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                msg.clone(),
                span.resolve(db),
            )],

            Self::KindMismatch(span, msg) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                msg.clone(),
                span.resolve(db),
            )],

            Self::RecursiveType {
                primary_span,
                field_span,
            } => {
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        "recursive type definition".to_string(),
                        primary_span.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "recursion occurs here".to_string(),
                        field_span.resolve(db),
                    ),
                ]
            }

            Self::TypeAliasArgumentMismatch {
                span: primary_span,
                type_alias,
                ..
            } => {
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!(
                            "expected {} arguments here",
                            type_alias
                                .generic_params(db.as_hir_db())
                                .len(db.as_hir_db())
                        ),
                        primary_span.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("type alias defined here"),
                        type_alias.lazy_span().resolve(db),
                    ),
                ]
            }

            Self::TypeAliasCycle(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "cycle happens here".to_string(),
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
        let message = self.message(db.as_hir_db());
        let sub_diags = self.sub_diags(db);

        CompleteDiagnostic::new(severity, message, sub_diags, vec![], error_code)
    }
}
