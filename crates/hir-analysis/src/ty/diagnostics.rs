use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::{
    diagnostics::DiagnosticVoucher,
    hir_def::{Trait, TypeAlias as HirTypeAlias},
    span::{DynLazySpan, LazySpan},
    HirDb,
};

use super::ty::Kind;

#[salsa::accumulator]
pub struct AdtDefDiagAccumulator(pub(super) TyLowerDiag);
#[salsa::accumulator]
pub struct TypeAliasDefDiagAccumulator(pub(super) TyLowerDiag);
#[salsa::accumulator]
pub struct GenericParamDiagAccumulator(pub(super) TyLowerDiag);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyLowerDiag {
    NotFullyAppliedType(DynLazySpan),
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

    DuplicateKindBound(DynLazySpan, DynLazySpan),

    KindBoundNotAllowed(DynLazySpan),

    AssocTy(DynLazySpan),
}

impl TyLowerDiag {
    pub fn not_fully_applied_type(span: DynLazySpan) -> Self {
        Self::NotFullyAppliedType(span)
    }

    pub fn kind_mismatch(span: DynLazySpan, expected: Option<Kind>, actual: Kind) -> Self {
        let msg = if let Some(expected) = expected {
            debug_assert!(expected != actual);

            format!("expected `{}` kind, but found `{}` kind", expected, actual,)
        } else {
            "too many generic arguments".to_string()
        };

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
            Self::KindMismatch(_, _) => 1,
            Self::RecursiveType { .. } => 2,
            Self::TypeAliasArgumentMismatch { .. } => 3,
            Self::TypeAliasCycle(_) => 4,
            Self::DuplicateKindBound(_, _) => 5,
            Self::KindBoundNotAllowed(_) => 6,
            Self::AssocTy(_) => 7,
        }
    }

    fn message(&self, db: &dyn HirDb) -> String {
        match self {
            Self::NotFullyAppliedType(_) => "expected fully applied type".to_string(),
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

            Self::DuplicateKindBound(_, _) => "duplicate type bound is not allowed.".to_string(),
            Self::KindBoundNotAllowed(_) => "kind bound is not allowed".to_string(),

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

            Self::DuplicateKindBound(primary, first_defined) => {
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        "duplicate type bound here".to_string(),
                        primary.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "first defined here".to_string(),
                        first_defined.resolve(db),
                    ),
                ]
            }
            Self::KindBoundNotAllowed(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "kind bound is not allowed here".to_string(),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitLowerDiag {
    ExternalTraitForExternalType(DynLazySpan),

    ConflictTraitImpl {
        primary: DynLazySpan,
        conflict_with: DynLazySpan,
    },

    KindMismatch {
        primary: DynLazySpan,
        trait_def: Trait,
    },

    TraitArgumentMismatch {
        span: DynLazySpan,
        trait_: Trait,
        n_given_arg: usize,
    },
}

impl TraitLowerDiag {
    fn local_code(&self) -> u16 {
        match self {
            Self::ExternalTraitForExternalType(_) => 0,
            Self::ConflictTraitImpl { .. } => 1,
            Self::KindMismatch { .. } => 2,
            Self::TraitArgumentMismatch { .. } => 3,
        }
    }

    fn message(&self, db: &dyn HirDb) -> String {
        match self {
            Self::ExternalTraitForExternalType(_) => {
                "external trait cannot be implemented for external type".to_string()
            }

            Self::ConflictTraitImpl { .. } => "conflict trait implementation".to_string(),

            Self::KindMismatch { .. } => "type doesn't satisfy required kind bound".to_string(),

            Self::TraitArgumentMismatch { .. } => {
                "given trait argument number mismatch".to_string()
            }
        }
    }

    fn sub_diags(&self, db: &dyn hir::SpannedHirDb) -> Vec<SubDiagnostic> {
        match self {
            Self::ExternalTraitForExternalType(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "external trait cannot be implemented for external type".to_string(),
                span.resolve(db),
            )],

            Self::ConflictTraitImpl {
                primary,
                conflict_with,
            } => vec![
                SubDiagnostic::new(
                    LabelStyle::Primary,
                    "conflict trait implementation".to_string(),
                    primary.resolve(db),
                ),
                SubDiagnostic::new(
                    LabelStyle::Secondary,
                    "conflict with this trait implementation".to_string(),
                    conflict_with.resolve(db),
                ),
            ],

            Self::KindMismatch { primary, trait_def } => vec![
                SubDiagnostic::new(
                    LabelStyle::Primary,
                    "type doesn't satisfy required kind bound here".to_string(),
                    primary.resolve(db),
                ),
                SubDiagnostic::new(
                    LabelStyle::Secondary,
                    "trait is defined here".to_string(),
                    trait_def.lazy_span().name().resolve(db),
                ),
            ],

            Self::TraitArgumentMismatch {
                span,
                trait_,
                n_given_arg,
            } => {
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!(
                            "expected {} arguments here, but {} given",
                            trait_.generic_params(db.as_hir_db()).len(db.as_hir_db()),
                            n_given_arg,
                        ),
                        span.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("trait defined here"),
                        trait_.lazy_span().name().resolve(db),
                    ),
                ]
            }
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }
}

impl DiagnosticVoucher for TraitLowerDiag {
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
