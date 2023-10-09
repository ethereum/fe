use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::{
    diagnostics::DiagnosticVoucher,
    hir_def::{ImplTrait, Trait, TypeAlias as HirTypeAlias},
    span::{DynLazySpan, LazySpan},
    SpannedHirDb,
};

use crate::HirAnalysisDb;

use super::{
    constraint::PredicateId,
    ty_def::{Kind, TyId},
};

#[salsa::accumulator]
pub struct AdtDefDiagAccumulator(pub(super) TyDiagCollection);
#[salsa::accumulator]
pub struct TraitDefDiagAccumulator(pub(super) TyDiagCollection);
#[salsa::accumulator]
pub struct ImplTraitDefDiagAccumulator(pub(super) TyDiagCollection);
#[salsa::accumulator]
pub struct TypeAliasDefDiagAccumulator(pub(super) TyDiagCollection);

#[derive(Debug, PartialEq, Eq, Hash, Clone, derive_more::From)]
pub enum TyDiagCollection {
    Ty(TyLowerDiag),
    Satisfaction(TraitConstraintDiag),
    TraitLower(TraitLowerDiag),
}

impl TyDiagCollection {
    pub(super) fn to_voucher(&self) -> Box<dyn hir::diagnostics::DiagnosticVoucher> {
        match self.clone() {
            TyDiagCollection::Ty(diag) => Box::new(diag) as _,
            TyDiagCollection::Satisfaction(diag) => Box::new(diag) as _,
            TyDiagCollection::TraitLower(diag) => Box::new(diag) as _,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyLowerDiag {
    NotFullyAppliedType(DynLazySpan),
    InvalidTypeArg(DynLazySpan, String),
    RecursiveType {
        primary_span: DynLazySpan,
        field_span: DynLazySpan,
    },

    UnboundTypeAliasParam {
        span: DynLazySpan,
        type_alias: HirTypeAlias,
        n_given_arg: usize,
    },
    TypeAliasCycle {
        primary: DynLazySpan,
        cycle: Vec<HirTypeAlias>,
    },

    KindBoundMismatch(DynLazySpan, String),

    KindBoundNotAllowed(DynLazySpan),

    AssocTy(DynLazySpan),
}

impl TyLowerDiag {
    pub fn not_fully_applied_type(span: DynLazySpan) -> Self {
        Self::NotFullyAppliedType(span)
    }

    pub fn invalid_type_arg(span: DynLazySpan, expected: Option<Kind>, actual: Kind) -> Self {
        let msg = if let Some(expected) = expected {
            debug_assert!(!expected.does_match(&actual));

            format!("expected `{}` kind, but found `{}` kind", expected, actual,)
        } else {
            "too many generic arguments".to_string()
        };

        Self::InvalidTypeArg(span, msg)
    }

    pub(super) fn recursive_type(primary_span: DynLazySpan, field_span: DynLazySpan) -> Self {
        Self::RecursiveType {
            primary_span,
            field_span,
        }
    }

    pub(super) fn unbound_type_alias_param(
        span: DynLazySpan,
        type_alias: HirTypeAlias,
        n_given_arg: usize,
    ) -> Self {
        Self::UnboundTypeAliasParam {
            span,
            type_alias,
            n_given_arg,
        }
    }

    pub(super) fn kind_bound_mismatch(
        db: &dyn HirAnalysisDb,
        span: DynLazySpan,
        ty: TyId,
        former_bound: &Kind,
        new_kind: &Kind,
    ) -> Self {
        let msg = format!(
            "`{}` is already declared with `{}` kind, but found `{}` kind here",
            ty.pretty_print(db),
            former_bound,
            new_kind
        );
        Self::KindBoundMismatch(span, msg)
    }

    pub(super) fn assoc_ty(span: DynLazySpan) -> Self {
        Self::AssocTy(span)
    }

    fn local_code(&self) -> u16 {
        match self {
            Self::NotFullyAppliedType(_) => 0,
            Self::InvalidTypeArg(_, _) => 1,
            Self::RecursiveType { .. } => 2,
            Self::UnboundTypeAliasParam { .. } => 3,
            Self::TypeAliasCycle { .. } => 4,
            Self::KindBoundMismatch(_, _) => 5,
            Self::KindBoundNotAllowed(_) => 6,
            Self::AssocTy(_) => 7,
        }
    }

    fn message(&self) -> String {
        match self {
            Self::NotFullyAppliedType(_) => "expected fully applied type".to_string(),
            Self::InvalidTypeArg(_, _) => "invalid type argument given".to_string(),
            Self::RecursiveType { .. } => "recursive type is not allowed".to_string(),

            Self::UnboundTypeAliasParam { .. } => {
                "all type parameters of type alias must be given".to_string()
            }
            Self::TypeAliasCycle { .. } => "recursive type alias cycle is detected".to_string(),

            Self::KindBoundMismatch(_, _) => "duplicate type bound is not allowed.".to_string(),
            Self::KindBoundNotAllowed(_) => "kind bound is not allowed".to_string(),

            Self::AssocTy(_) => "associated type is not supported ".to_string(),
        }
    }

    fn sub_diags(&self, db: &dyn SpannedHirDb) -> Vec<SubDiagnostic> {
        match self {
            Self::NotFullyAppliedType(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "expected fully applied type here".to_string(),
                span.resolve(db),
            )],

            Self::InvalidTypeArg(span, msg) => vec![SubDiagnostic::new(
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

            Self::UnboundTypeAliasParam {
                span: primary_span,
                type_alias,
                ..
            } => {
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!(
                            "expected at least {} arguments here",
                            type_alias
                                .generic_params(db.as_hir_db())
                                .len(db.as_hir_db())
                        ),
                        primary_span.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "type alias defined here".to_string(),
                        type_alias.lazy_span().resolve(db),
                    ),
                ]
            }

            Self::TypeAliasCycle { primary, cycle } => {
                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "cycle happens here".to_string(),
                    primary.resolve(db),
                )];
                diags.extend(cycle.iter().map(|type_alias| {
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "type alias defined here".to_string(),
                        type_alias.lazy_span().alias_moved().resolve(db),
                    )
                }));
                diags
            }

            Self::KindBoundMismatch(primary, msg) => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    msg.clone(),
                    primary.resolve(db),
                )]
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

    fn to_complete(&self, db: &dyn SpannedHirDb) -> CompleteDiagnostic {
        let severity = self.severity();
        let error_code = self.error_code();
        let message = self.message();
        let sub_diags = self.sub_diags(db);

        CompleteDiagnostic::new(severity, message, sub_diags, vec![], error_code)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitLowerDiag {
    ExternalTraitForExternalType(DynLazySpan),

    ConflictTraitImpl {
        primary: ImplTrait,
        conflict_with: ImplTrait,
    },

    CyclicSuperTraits(DynLazySpan),
}

impl TraitLowerDiag {
    pub(super) fn external_trait_for_external_type(impl_trait: ImplTrait) -> Self {
        Self::ExternalTraitForExternalType(impl_trait.lazy_span().trait_ref().into())
    }

    pub(super) fn conflict_impl(primary: ImplTrait, conflict_with: ImplTrait) -> Self {
        Self::ConflictTraitImpl {
            primary,
            conflict_with,
        }
    }

    fn local_code(&self) -> u16 {
        match self {
            Self::ExternalTraitForExternalType(_) => 0,
            Self::ConflictTraitImpl { .. } => 1,
            Self::CyclicSuperTraits { .. } => 2,
        }
    }

    fn message(&self) -> String {
        match self {
            Self::ExternalTraitForExternalType(_) => {
                "external trait cannot be implemented for external type".to_string()
            }

            Self::ConflictTraitImpl { .. } => "conflict trait implementation".to_string(),

            Self::CyclicSuperTraits { .. } => "cyclic super traits are not allowed".to_string(),
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
                    primary.lazy_span().ty().resolve(db),
                ),
                SubDiagnostic::new(
                    LabelStyle::Secondary,
                    "conflict with this trait implementation".to_string(),
                    conflict_with.lazy_span().ty().resolve(db),
                ),
            ],

            Self::CyclicSuperTraits(span) => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "super traits cycle is detected here".to_string(),
                    span.resolve(db),
                )]
            }
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }
}

impl DiagnosticVoucher for TraitLowerDiag {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(DiagnosticPass::ImplTraitDefinition, self.local_code())
    }

    fn to_complete(&self, db: &dyn hir::SpannedHirDb) -> CompleteDiagnostic {
        let severity = self.severity();
        let error_code = self.error_code();
        let message = self.message();
        let sub_diags = self.sub_diags(db);

        CompleteDiagnostic::new(severity, message, sub_diags, vec![], error_code)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitConstraintDiag {
    KindMismatch {
        primary: DynLazySpan,
        trait_def: Trait,
    },

    TraitArgNumMismatch {
        span: DynLazySpan,
        expected: usize,
        given: usize,
    },

    TraitArgKindMismatch(DynLazySpan, String),

    TraitBoundNotSat(DynLazySpan, String),

    InfiniteBoundRecursion(DynLazySpan, String),

    ConcreteTypeBound(DynLazySpan, String),
}

impl TraitConstraintDiag {
    pub(super) fn kind_mismatch(
        db: &dyn HirAnalysisDb,
        span: DynLazySpan,
        expected: &Kind,
        actual: TyId,
    ) -> Self {
        let actual_kind = actual.kind(db);
        let ty_display = actual.pretty_print(db);
        let msg = format!(
            "expected `{}` kind, but `{}` has `{}` kind",
            expected, ty_display, actual_kind,
        );
        Self::TraitArgKindMismatch(span, msg)
    }

    pub(super) fn trait_arg_num_mismatch(span: DynLazySpan, expected: usize, given: usize) -> Self {
        Self::TraitArgNumMismatch {
            span,
            expected,
            given,
        }
    }

    pub(super) fn trait_bound_not_satisfied(
        db: &dyn HirAnalysisDb,
        span: DynLazySpan,
        pred: PredicateId,
    ) -> Self {
        let ty = pred.ty(db);
        let goal = pred.trait_inst(db);
        let msg = format!(
            "`{}` doesn't implement `{}`",
            ty.pretty_print(db),
            goal.pretty_print(db)
        );
        Self::TraitBoundNotSat(span, msg)
    }

    pub(super) fn infinite_bound_recursion(
        db: &dyn HirAnalysisDb,
        span: DynLazySpan,
        pred: PredicateId,
    ) -> Self {
        let goal = pred.trait_inst(db);
        let ty = pred.ty(db);
        let msg = format!(
            "infinite evaluation recursion occurs when checking `{}: {}` ",
            ty.pretty_print(db),
            goal.pretty_print(db)
        );
        Self::InfiniteBoundRecursion(span, msg)
    }

    pub(super) fn concrete_type_bound(db: &dyn HirAnalysisDb, span: DynLazySpan, ty: TyId) -> Self {
        let msg = format!("`{}` is a concrete type", ty.pretty_print(db));
        Self::ConcreteTypeBound(span, msg)
    }

    fn local_code(&self) -> u16 {
        match self {
            Self::KindMismatch { .. } => 0,
            Self::TraitArgNumMismatch { .. } => 1,
            Self::TraitArgKindMismatch(_, _) => 2,
            Self::TraitBoundNotSat(_, _) => 3,
            Self::InfiniteBoundRecursion(_, _) => 4,
            Self::ConcreteTypeBound(_, _) => 5,
        }
    }

    fn message(&self) -> String {
        match self {
            Self::KindMismatch { .. } => "type doesn't satisfy required kind bound".to_string(),

            Self::TraitArgNumMismatch { .. } => "given trait argument number mismatch".to_string(),

            Self::TraitArgKindMismatch(_, _) => "given trait argument kind mismatch".to_string(),

            Self::TraitBoundNotSat(_, _) => "trait bound is not satisfied".to_string(),

            Self::InfiniteBoundRecursion(_, _) => "infinite trait bound recursion".to_string(),

            Self::ConcreteTypeBound(_, _) => {
                "trait bound for concrete type is not allowed".to_string()
            }
        }
    }

    fn sub_diags(&self, db: &dyn SpannedHirDb) -> Vec<SubDiagnostic> {
        match self {
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

            Self::TraitArgNumMismatch {
                span,
                expected,
                given,
            } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("expected {} arguments here, but {} given", expected, given,),
                    span.resolve(db),
                )]
            }

            Self::TraitArgKindMismatch(span, msg) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                msg.clone(),
                span.resolve(db),
            )],

            Self::TraitBoundNotSat(span, msg) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                msg.clone(),
                span.resolve(db),
            )],

            Self::InfiniteBoundRecursion(span, msg) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                msg.clone(),
                span.resolve(db),
            )],

            Self::ConcreteTypeBound(span, msg) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                msg.clone(),
                span.resolve(db),
            )],
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }
}

impl DiagnosticVoucher for TraitConstraintDiag {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(DiagnosticPass::TraitSatisfaction, self.local_code())
    }

    fn to_complete(&self, db: &dyn SpannedHirDb) -> CompleteDiagnostic {
        let severity = self.severity();
        let error_code = self.error_code();
        let message = self.message();
        let sub_diags = self.sub_diags(db);

        CompleteDiagnostic::new(severity, message, sub_diags, vec![], error_code)
    }
}
