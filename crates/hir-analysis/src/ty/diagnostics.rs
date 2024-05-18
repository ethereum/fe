use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use either::Either;
use hir::{
    diagnostics::DiagnosticVoucher,
    hir_def::{
        FieldIndex, Func, FuncParamName, IdentId, ImplTrait, ItemKind, PathId, Trait,
        TypeAlias as HirTypeAlias,
    },
    span::{DynLazySpan, LazySpan},
    HirDb, SpannedHirDb,
};
use itertools::Itertools;

use super::{
    trait_def::{TraitDef, TraitInstId},
    ty_check::{RecordLike, TraitOps},
    ty_def::{Kind, TyData, TyId, TyVarSort},
};
use crate::{name_resolution::diagnostics::NameResDiag, HirAnalysisDb};

#[derive(Debug, PartialEq, Eq, Hash, Clone, derive_more::From)]
pub enum FuncBodyDiag<'db> {
    Ty(TyDiagCollection<'db>),
    Body(BodyDiag<'db>),
    NameRes(NameResDiag<'db>),
}

impl<'db> FuncBodyDiag<'db> {
    pub(super) fn to_voucher(&self) -> Box<dyn hir::diagnostics::DiagnosticVoucher<'db> + 'db> {
        match self {
            Self::Ty(diag) => diag.to_voucher(),
            Self::Body(diag) => Box::new(diag.clone()) as _,
            Self::NameRes(diag) => Box::new(diag.clone()) as _,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, derive_more::From)]
pub enum TyDiagCollection<'db> {
    Ty(TyLowerDiag<'db>),
    Satisfiability(TraitConstraintDiag<'db>),
    TraitLower(TraitLowerDiag<'db>),
    Impl(ImplDiag<'db>),
}

impl<'db> TyDiagCollection<'db> {
    pub(super) fn to_voucher(&self) -> Box<dyn hir::diagnostics::DiagnosticVoucher<'db> + 'db> {
        match self.clone() {
            TyDiagCollection::Ty(diag) => Box::new(diag) as _,
            TyDiagCollection::Satisfiability(diag) => Box::new(diag) as _,
            TyDiagCollection::TraitLower(diag) => Box::new(diag) as _,
            TyDiagCollection::Impl(diag) => Box::new(diag) as _,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyLowerDiag<'db> {
    ExpectedStarKind(DynLazySpan<'db>),
    InvalidTypeArgKind(DynLazySpan<'db>, String),
    TooManyGenericArgs {
        span: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    },

    RecursiveType {
        primary_span: DynLazySpan<'db>,
        field_span: DynLazySpan<'db>,
    },

    UnboundTypeAliasParam {
        span: DynLazySpan<'db>,
        type_alias: HirTypeAlias<'db>,
        n_given_arg: usize,
    },
    TypeAliasCycle {
        primary: DynLazySpan<'db>,
        cycle: Vec<HirTypeAlias<'db>>,
    },

    InconsistentKindBound(DynLazySpan<'db>, String),

    KindBoundNotAllowed(DynLazySpan<'db>),

    GenericParamAlreadyDefinedInParent {
        primary: DynLazySpan<'db>,
        conflict_with: DynLazySpan<'db>,
        name: IdentId<'db>,
    },

    DuplicatedArgName {
        primary: DynLazySpan<'db>,
        conflict_with: DynLazySpan<'db>,
        name: IdentId<'db>,
    },

    InvalidConstParamTy {
        primary: DynLazySpan<'db>,
    },

    RecursiveConstParamTy(DynLazySpan<'db>),

    ConstTyMismatch {
        primary: DynLazySpan<'db>,
        expected: String,
        actual: String,
    },

    ConstTyExpected {
        primary: DynLazySpan<'db>,
        expected: String,
    },

    NormalTypeExpected {
        primary: DynLazySpan<'db>,
        given: String,
    },

    AssocTy(DynLazySpan<'db>),

    InvalidConstTyExpr(DynLazySpan<'db>),
}

impl<'db> TyLowerDiag<'db> {
    pub fn expected_star_kind_ty(span: DynLazySpan<'db>) -> Self {
        Self::ExpectedStarKind(span)
    }

    pub fn invalid_type_arg_kind(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        expected: Option<Kind>,
        arg: TyId<'db>,
    ) -> Self {
        let msg = if let Some(expected) = expected {
            let arg_kind = arg.kind(db);
            debug_assert!(!expected.does_match(arg_kind));

            format!(
                "expected `{}` kind, but `{}` has `{}` kind",
                expected,
                arg.pretty_print(db),
                arg_kind
            )
        } else {
            "too many generic arguments".to_string()
        };

        Self::InvalidTypeArgKind(span, msg)
    }

    pub(super) fn recursive_type(
        primary_span: DynLazySpan<'db>,
        field_span: DynLazySpan<'db>,
    ) -> Self {
        Self::RecursiveType {
            primary_span,
            field_span,
        }
    }

    pub(super) fn unbound_type_alias_param(
        span: DynLazySpan<'db>,
        type_alias: HirTypeAlias<'db>,
        n_given_arg: usize,
    ) -> Self {
        Self::UnboundTypeAliasParam {
            span,
            type_alias,
            n_given_arg,
        }
    }

    pub(super) fn invalid_const_param_ty(primary: DynLazySpan<'db>) -> Self {
        Self::InvalidConstParamTy { primary }
    }

    pub(super) fn inconsistent_kind_bound(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        former_bound: &Kind,
        new_kind: &Kind,
    ) -> Self {
        let msg = format!(
            "`{}` is already declared with `{}` kind, but found `{}` kind here",
            ty.pretty_print(db),
            former_bound,
            new_kind
        );
        Self::InconsistentKindBound(span, msg)
    }

    pub(super) fn generic_param_conflict(
        primary: DynLazySpan<'db>,
        conflict_with: DynLazySpan<'db>,
        name: IdentId<'db>,
    ) -> Self {
        Self::GenericParamAlreadyDefinedInParent {
            primary,
            conflict_with,
            name,
        }
    }

    pub(super) fn const_ty_mismatch(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        expected: TyId<'db>,
        actual: TyId<'db>,
    ) -> Self {
        let expected = expected.pretty_print(db).to_string();
        let actual = actual.pretty_print(db).to_string();
        Self::ConstTyMismatch {
            primary,
            expected,
            actual,
        }
    }

    pub(super) fn const_ty_expected(
        db: &dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        expected: TyId<'db>,
    ) -> Self {
        let expected = expected.pretty_print(db).to_string();
        Self::ConstTyExpected { primary, expected }
    }

    pub(super) fn normal_type_expected(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        given: TyId<'db>,
    ) -> Self {
        let given = given.pretty_print(db).to_string();
        Self::NormalTypeExpected { primary, given }
    }

    pub(super) fn duplicated_arg_name(
        primary: DynLazySpan<'db>,
        conflict_with: DynLazySpan<'db>,
        name: IdentId<'db>,
    ) -> Self {
        Self::DuplicatedArgName {
            primary,
            conflict_with,
            name,
        }
    }

    pub(super) fn assoc_ty(span: DynLazySpan<'db>) -> Self {
        Self::AssocTy(span)
    }

    fn local_code(&self) -> u16 {
        match self {
            Self::ExpectedStarKind(_) => 0,
            Self::InvalidTypeArgKind(_, _) => 1,
            Self::RecursiveType { .. } => 2,
            Self::UnboundTypeAliasParam { .. } => 3,
            Self::TypeAliasCycle { .. } => 4,
            Self::InconsistentKindBound(_, _) => 5,
            Self::KindBoundNotAllowed(_) => 6,
            Self::GenericParamAlreadyDefinedInParent { .. } => 7,
            Self::DuplicatedArgName { .. } => 8,
            Self::InvalidConstParamTy { .. } => 9,
            Self::RecursiveConstParamTy { .. } => 10,
            Self::ConstTyMismatch { .. } => 11,
            Self::ConstTyExpected { .. } => 12,
            Self::NormalTypeExpected { .. } => 13,
            Self::AssocTy(_) => 14,
            Self::InvalidConstTyExpr(_) => 15,
            Self::TooManyGenericArgs { .. } => 16,
        }
    }

    fn message(&self) -> String {
        match self {
            Self::ExpectedStarKind(_) => "expected `*` kind in this context".to_string(),
            Self::InvalidTypeArgKind(_, _) => "invalid type argument kind".to_string(),
            Self::TooManyGenericArgs {
                span: _,
                expected,
                given,
            } => format!("too many generic args; expected {expected}, given {given}"),
            Self::RecursiveType { .. } => "recursive type is not allowed".to_string(),

            Self::UnboundTypeAliasParam { .. } => {
                "all type parameters of type alias must be given".to_string()
            }
            Self::TypeAliasCycle { .. } => "recursive type alias cycle is detected".to_string(),

            Self::InconsistentKindBound(_, _) => "duplicate type bound is not allowed.".to_string(),
            Self::KindBoundNotAllowed(_) => "kind bound is not allowed".to_string(),

            Self::GenericParamAlreadyDefinedInParent { .. } => {
                "generic parameter is already defined in the parent item".to_string()
            }

            Self::DuplicatedArgName { .. } => {
                "duplicated argument name in function definition is not allowed".to_string()
            }

            Self::InvalidConstParamTy { .. } => "invalid const parameter type".to_string(),

            Self::ConstTyMismatch { .. } => {
                "given type doesn't match the expected const type".to_string()
            }

            Self::ConstTyExpected { .. } => "expected const type".to_string(),

            Self::NormalTypeExpected { .. } => "expected a normal type".to_string(),

            Self::RecursiveConstParamTy(_) => {
                "recursive const parameter type is not allowed".to_string()
            }

            Self::AssocTy(_) => "associated type is not supported ".to_string(),

            Self::InvalidConstTyExpr(_) => {
                "the expression is not supported yet in a const type context".to_string()
            }
        }
    }

    fn sub_diags(&self, db: &dyn SpannedHirDb) -> Vec<SubDiagnostic> {
        match self {
            Self::ExpectedStarKind(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "expected `*` kind here".to_string(),
                span.resolve(db),
            )],

            Self::InvalidTypeArgKind(span, msg) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                msg.clone(),
                span.resolve(db),
            )],

            Self::TooManyGenericArgs {
                span,
                expected,
                given,
            } => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                format!("too many generic args; expected {expected}, given {given}"),
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

            Self::InconsistentKindBound(primary, msg) => {
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

            Self::GenericParamAlreadyDefinedInParent {
                primary,
                conflict_with,
                name,
            } => {
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!("`{}` is already defined", name.data(db.as_hir_db())),
                        primary.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "conflict with this generic parameter".to_string(),
                        conflict_with.resolve(db),
                    ),
                ]
            }

            Self::DuplicatedArgName {
                primary,
                conflict_with,
                name,
            } => {
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!("duplicated argument name `{}`", name.data(db.as_hir_db())),
                        primary.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "conflict with this argument name".to_string(),
                        conflict_with.resolve(db),
                    ),
                ]
            }

            Self::InvalidConstParamTy { primary, .. } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "only integer or bool types are allowed as a const parameter type".to_string(),
                    primary.resolve(db),
                )]
            }

            Self::ConstTyMismatch {
                primary,
                expected,
                actual,
            } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!(
                        "expected `{}` type here, but `{}` is given",
                        expected, actual
                    ),
                    primary.resolve(db),
                )]
            }

            Self::ConstTyExpected { primary, expected } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("expected const type of `{}` here", expected),
                    primary.resolve(db),
                )]
            }

            Self::NormalTypeExpected { primary, given } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("expected a normal type here, but `{}` is given", given,),
                    primary.resolve(db),
                )]
            }

            Self::RecursiveConstParamTy(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "recursive const parameter type is detected here".to_string(),
                span.resolve(db),
            )],

            Self::AssocTy(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "associated type is not implemented".to_string(),
                span.resolve(db),
            )],

            Self::InvalidConstTyExpr(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "only literal expression is supported".to_string(),
                span.resolve(db),
            )],
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }
}

impl<'db> DiagnosticVoucher<'db> for TyLowerDiag<'db> {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(DiagnosticPass::TypeDefinition, self.local_code())
    }

    fn to_complete(&self, db: &'db dyn SpannedHirDb) -> CompleteDiagnostic {
        let severity = self.severity();
        let error_code = self.error_code();
        let message = self.message();
        let sub_diags = self.sub_diags(db);

        CompleteDiagnostic::new(severity, message, sub_diags, vec![], error_code)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BodyDiag<'db> {
    TypeMismatch(DynLazySpan<'db>, String, String),
    InfiniteOccurrence(DynLazySpan<'db>),

    DuplicatedBinding {
        primary: DynLazySpan<'db>,
        conflicat_with: DynLazySpan<'db>,
        name: IdentId<'db>,
    },
    DuplicatedRestPat(DynLazySpan<'db>),

    InvalidPathDomainInPat {
        primary: DynLazySpan<'db>,
        resolved: Option<DynLazySpan<'db>>,
    },

    UnitVariantExpected {
        primary: DynLazySpan<'db>,
        kind_name: String,
        hint: Option<String>,
    },

    TupleVariantExpected {
        primary: DynLazySpan<'db>,
        kind_name: Option<String>,
        hint: Option<String>,
    },

    RecordExpected {
        primary: DynLazySpan<'db>,
        kind_name: Option<String>,
        hint: Option<String>,
    },

    MismatchedFieldCount {
        primary: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    },

    DuplicatedRecordFieldBind {
        primary: DynLazySpan<'db>,
        first_use: DynLazySpan<'db>,
        name: IdentId<'db>,
    },

    RecordFieldNotFound {
        primary: DynLazySpan<'db>,
        label: IdentId<'db>,
    },

    ExplicitLabelExpectedInRecord {
        primary: DynLazySpan<'db>,
        hint: Option<String>,
    },

    MissingRecordFields {
        primary: DynLazySpan<'db>,
        missing_fields: Vec<IdentId<'db>>,
        hint: Option<String>,
    },

    UndefinedVariable(DynLazySpan<'db>, IdentId<'db>),

    ReturnedTypeMismatch {
        primary: DynLazySpan<'db>,
        actual: String,
        expected: String,
        func: Option<Func<'db>>,
    },

    TypeMustBeKnown(DynLazySpan<'db>),

    AccessedFieldNotFound {
        primary: DynLazySpan<'db>,
        given_ty: String,
        index: FieldIndex<'db>,
    },

    OpsTraitNotImplemented {
        span: DynLazySpan<'db>,
        ty: String,
        op: IdentId<'db>,
        trait_path: PathId<'db>,
    },

    NonAssignableExpr(DynLazySpan<'db>),

    ImmutableAssignment {
        primary: DynLazySpan<'db>,
        binding: Option<(IdentId<'db>, DynLazySpan<'db>)>,
    },

    LoopControlOutsideOfLoop {
        primary: DynLazySpan<'db>,
        is_break: bool,
    },

    TraitNotImplemented {
        primary: DynLazySpan<'db>,
        ty: String,
        trait_name: IdentId<'db>,
    },

    NotCallable(DynLazySpan<'db>, String),

    CallGenericArgNumMismatch {
        primary: DynLazySpan<'db>,
        def_span: DynLazySpan<'db>,
        given: usize,
        expected: usize,
    },

    CallArgNumMismatch {
        primary: DynLazySpan<'db>,
        def_span: DynLazySpan<'db>,
        given: usize,
        expected: usize,
    },

    CallArgLabelMismatch {
        primary: DynLazySpan<'db>,
        def_span: DynLazySpan<'db>,
        given: Option<IdentId<'db>>,
        expected: IdentId<'db>,
    },

    AmbiguousInherentMethodCall {
        primary: DynLazySpan<'db>,
        method_name: IdentId<'db>,
        cand_spans: Vec<DynLazySpan<'db>>,
    },

    AmbiguousTrait {
        primary: DynLazySpan<'db>,
        method_name: IdentId<'db>,
        traits: Vec<Trait<'db>>,
    },

    AmbiguousTraitInst {
        primary: DynLazySpan<'db>,
        cands: Vec<String>,
    },

    InvisibleAmbiguousTrait {
        primary: DynLazySpan<'db>,
        traits: Vec<Trait<'db>>,
    },

    MethodNotFound {
        primary: DynLazySpan<'db>,
        method_name: IdentId<'db>,
        receiver: String,
    },

    NotValue {
        primary: DynLazySpan<'db>,
        given: Either<ItemKind<'db>, TyId<'db>>,
    },

    TypeAnnotationNeeded {
        primary: DynLazySpan<'db>,
        ty: Option<String>,
        is_integral: bool,
    },
}

impl<'db> BodyDiag<'db> {
    pub(super) fn type_mismatch(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        expected: TyId<'db>,
        actual: TyId<'db>,
    ) -> Self {
        let expected = expected.pretty_print(db).to_string();
        let actual = actual.pretty_print(db).to_string();
        Self::TypeMismatch(span, expected, actual)
    }

    pub(super) fn unit_variant_expected<T>(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        record_like: T,
    ) -> Self
    where
        T: RecordLike<'db>,
    {
        let kind_name = record_like.kind_name(db);
        let hint = record_like.initializer_hint(db);
        Self::UnitVariantExpected {
            primary,
            kind_name,
            hint,
        }
    }

    pub(super) fn tuple_variant_expected<T>(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        record_like: Option<T>,
    ) -> Self
    where
        T: RecordLike<'db>,
    {
        let (kind_name, hint) = if let Some(record_like) = record_like {
            (
                Some(record_like.kind_name(db)),
                record_like.initializer_hint(db),
            )
        } else {
            (None, None)
        };

        Self::TupleVariantExpected {
            primary,
            kind_name,
            hint,
        }
    }

    pub(super) fn record_expected<T>(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        record_like: Option<T>,
    ) -> Self
    where
        T: RecordLike<'db>,
    {
        let (kind_name, hint) = if let Some(record_like) = record_like {
            (
                Some(record_like.kind_name(db)),
                record_like.initializer_hint(db),
            )
        } else {
            (None, None)
        };

        Self::RecordExpected {
            primary,
            kind_name,
            hint,
        }
    }

    pub(super) fn record_field_not_found(primary: DynLazySpan<'db>, label: IdentId<'db>) -> Self {
        Self::RecordFieldNotFound { primary, label }
    }

    pub(super) fn returned_type_mismatch(
        db: &dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        actual: TyId<'db>,
        expected: TyId<'db>,
        func: Option<Func<'db>>,
    ) -> Self {
        let actual = actual.pretty_print(db).to_string();
        let expected = expected.pretty_print(db).to_string();
        Self::ReturnedTypeMismatch {
            primary,
            actual,
            expected,
            func,
        }
    }

    pub(super) fn accessed_field_not_found(
        db: &dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        given_ty: TyId<'db>,
        index: FieldIndex<'db>,
    ) -> Self {
        let given_ty = given_ty.pretty_print(db).to_string();
        Self::AccessedFieldNotFound {
            primary,
            given_ty,
            index,
        }
    }

    pub(super) fn ops_trait_not_implemented<T>(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
        ops: T,
    ) -> Self
    where
        T: TraitOps,
    {
        let ty = ty.pretty_print(db).to_string();
        let op = ops.op_symbol(db);
        let trait_path = ops.trait_path(db);
        Self::OpsTraitNotImplemented {
            span,
            ty,
            op,
            trait_path,
        }
    }
    pub(super) fn not_callable(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
    ) -> Self {
        let ty = ty.pretty_print(db).to_string();
        Self::NotCallable(span, ty)
    }

    pub(super) fn method_not_found(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        method_name: IdentId<'db>,
        receiver: Either<TyId<'db>, TraitDef<'db>>,
    ) -> Self {
        let receiver = match receiver {
            Either::Left(ty) => ty.pretty_print(db),
            Either::Right(trait_) => trait_
                .trait_(db)
                .name(db.as_hir_db())
                .unwrap()
                .data(db.as_hir_db()),
        };

        Self::MethodNotFound {
            primary,
            method_name,
            receiver: receiver.to_string(),
        }
    }

    pub(super) fn type_annotation_needed(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        ty: TyId<'db>,
    ) -> Self {
        let (ty, is_integral) = match ty.base_ty(db).data(db) {
            TyData::TyVar(var) => (None, matches!(var.sort, TyVarSort::Integral)),
            _ => (ty.pretty_print(db).to_string().into(), false),
        };

        Self::TypeAnnotationNeeded {
            primary,
            ty,
            is_integral,
        }
    }

    pub(super) fn ambiguous_trait_inst(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        cands: Vec<TraitInstId<'db>>,
    ) -> Self {
        let cands = cands
            .into_iter()
            .map(|cand| cand.pretty_print(db, false))
            .collect_vec();
        Self::AmbiguousTraitInst { primary, cands }
    }

    fn local_code(&self) -> u16 {
        match self {
            Self::TypeMismatch(..) => 0,
            Self::InfiniteOccurrence(..) => 1,
            Self::DuplicatedRestPat(..) => 2,
            Self::InvalidPathDomainInPat { .. } => 3,
            Self::UnitVariantExpected { .. } => 4,
            Self::TupleVariantExpected { .. } => 5,
            Self::RecordExpected { .. } => 6,
            Self::MismatchedFieldCount { .. } => 7,
            Self::DuplicatedRecordFieldBind { .. } => 8,
            Self::RecordFieldNotFound { .. } => 9,
            Self::ExplicitLabelExpectedInRecord { .. } => 10,
            Self::MissingRecordFields { .. } => 11,
            Self::UndefinedVariable(..) => 12,
            Self::ReturnedTypeMismatch { .. } => 13,
            Self::TypeMustBeKnown(..) => 14,
            Self::AccessedFieldNotFound { .. } => 15,
            Self::OpsTraitNotImplemented { .. } => 16,
            Self::NonAssignableExpr(..) => 17,
            Self::ImmutableAssignment { .. } => 18,
            Self::LoopControlOutsideOfLoop { .. } => 19,
            Self::TraitNotImplemented { .. } => 20,
            Self::NotCallable(..) => 21,
            Self::CallGenericArgNumMismatch { .. } => 22,
            Self::CallArgNumMismatch { .. } => 23,
            Self::CallArgLabelMismatch { .. } => 24,
            Self::AmbiguousInherentMethodCall { .. } => 25,
            Self::AmbiguousTrait { .. } => 26,
            Self::AmbiguousTraitInst { .. } => 27,
            Self::InvisibleAmbiguousTrait { .. } => 28,
            Self::MethodNotFound { .. } => 29,
            Self::NotValue { .. } => 30,
            Self::TypeAnnotationNeeded { .. } => 31,
            Self::DuplicatedBinding { .. } => 32,
        }
    }

    fn message(&self, db: &dyn HirDb) -> String {
        match self {
            Self::TypeMismatch(_, _, _) => "type mismatch".to_string(),
            Self::InfiniteOccurrence(_) => "infinite sized type found".to_string(),
            Self::DuplicatedBinding { name, .. } => {
                format!("duplicate binding `{}` in pattern", name.data(db))
            }
            Self::DuplicatedRestPat(_) => "duplicate `..` found".to_string(),
            Self::InvalidPathDomainInPat { .. } => "invalid item is given here".to_string(),
            Self::UnitVariantExpected { .. } => "expected unit variant".to_string(),
            Self::TupleVariantExpected { .. } => "expected tuple variant".to_string(),
            Self::RecordExpected { .. } => "expected record variant or struct".to_string(),
            Self::MismatchedFieldCount { .. } => "field count mismatch".to_string(),
            Self::DuplicatedRecordFieldBind { .. } => "duplicated record field binding".to_string(),
            Self::RecordFieldNotFound { .. } => "specified field not found".to_string(),
            Self::ExplicitLabelExpectedInRecord { .. } => "explicit label is required".to_string(),
            Self::MissingRecordFields { .. } => "all fields are not given".to_string(),
            Self::UndefinedVariable(..) => "undefined variable".to_string(),
            Self::ReturnedTypeMismatch { .. } => "returned type mismatch".to_string(),
            Self::TypeMustBeKnown(..) => "type must be known here".to_string(),
            Self::AccessedFieldNotFound { .. } => "invalid field index".to_string(),
            Self::OpsTraitNotImplemented { trait_path, .. } => {
                format!("`{}` trait is not implemented", trait_path.pretty_print(db))
            }
            Self::NonAssignableExpr { .. } => {
                "not assignable left-hand side of assignment".to_string()
            }
            Self::ImmutableAssignment { .. } => {
                "left-hand side of assignment is immutable".to_string()
            }

            Self::LoopControlOutsideOfLoop { is_break, .. } => {
                format!(
                    "`{}` is not allowed outside of a loop",
                    if *is_break { "break" } else { "continue" }
                )
            }

            Self::TraitNotImplemented { trait_name, ty, .. } => {
                format!("`{}` needs to be implemented for {ty}", trait_name.data(db))
            }

            Self::NotCallable(..) => "not callable type is given in call expression".to_string(),

            Self::CallGenericArgNumMismatch { .. } => {
                "given generic argument number mismatch".to_string()
            }

            Self::CallArgNumMismatch { .. } => "given argument number mismatch".to_string(),
            Self::CallArgLabelMismatch { .. } => "given argument label mismatch".to_string(),

            Self::AmbiguousInherentMethodCall { .. } => "ambiguous method call".to_string(),

            Self::AmbiguousTrait { .. } => "multiple trait candidates found".to_string(),

            Self::AmbiguousTraitInst { .. } => "ambiguous trait implementation".to_string(),

            Self::InvisibleAmbiguousTrait { .. } => "trait is not in the scope".to_string(),

            Self::MethodNotFound { method_name, .. } => {
                format!("`{}` is not found", method_name.data(db))
            }

            Self::NotValue { .. } => "value is expected".to_string(),
            Self::TypeAnnotationNeeded { .. } => "type annotation is needed".to_string(),
        }
    }

    fn sub_diags(&self, db: &dyn SpannedHirDb) -> Vec<SubDiagnostic> {
        match self {
            Self::TypeMismatch(span, expected, actual) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                format!("expected `{}`, but `{}` is given", expected, actual),
                span.resolve(db),
            )],

            Self::InfiniteOccurrence(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "infinite sized type found".to_string(),
                span.resolve(db),
            )],

            Self::DuplicatedBinding {
                primary,
                conflicat_with,
                name,
            } => {
                let name = name.data(db.as_hir_db());
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!("`{name}` is defined again here",),
                        primary.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("first definition of `{name}` in this pattern"),
                        conflicat_with.resolve(db),
                    ),
                ]
            }

            Self::DuplicatedRestPat(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "`..` can be used only once".to_string(),
                span.resolve(db),
            )],

            Self::InvalidPathDomainInPat { primary, resolved } => {
                let mut diag = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "expected type or enum variant here".to_string(),
                    primary.resolve(db),
                )];
                if let Some(resolved) = resolved {
                    diag.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "this item given".to_string(),
                        resolved.resolve(db),
                    ))
                }
                diag
            }

            Self::UnitVariantExpected {
                primary,
                kind_name: pat_kind,
                hint,
            } => {
                let mut diag = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("expected unit variant here, but found {}", pat_kind,),
                    primary.resolve(db),
                )];
                if let Some(hint) = hint {
                    diag.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("Consider using `{}` instead", hint),
                        primary.resolve(db),
                    ))
                }
                diag
            }

            Self::TupleVariantExpected {
                primary,
                kind_name: pat_kind,
                hint,
            } => {
                let mut diag = if let Some(pat_kind) = pat_kind {
                    vec![SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!("expected tuple variant here, but found {}", pat_kind,),
                        primary.resolve(db),
                    )]
                } else {
                    vec![SubDiagnostic::new(
                        LabelStyle::Primary,
                        "expected tuple variant here".to_string(),
                        primary.resolve(db),
                    )]
                };

                if let Some(hint) = hint {
                    diag.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("Consider using `{}` instead", hint),
                        primary.resolve(db),
                    ))
                }

                diag
            }

            Self::RecordExpected {
                primary,
                kind_name: pat_kind,
                hint,
            } => {
                let mut diag = if let Some(pat_kind) = pat_kind {
                    vec![SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!(
                            "expected record variant or struct here, but found {}",
                            pat_kind,
                        ),
                        primary.resolve(db),
                    )]
                } else {
                    vec![SubDiagnostic::new(
                        LabelStyle::Primary,
                        "expected record variant or struct here".to_string(),
                        primary.resolve(db),
                    )]
                };

                if let Some(hint) = hint {
                    diag.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("Consider using `{}` instead", hint),
                        primary.resolve(db),
                    ))
                }

                diag
            }

            Self::MismatchedFieldCount {
                primary,
                expected,
                given,
            } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("expected {} fields here, but {} given", expected, given,),
                    primary.resolve(db),
                )]
            }

            Self::DuplicatedRecordFieldBind {
                primary,
                first_use,
                name,
            } => {
                let name = name.data(db.as_hir_db());
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!("duplicated field binding `{}`", name),
                        primary.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("first use of `{}`", name),
                        first_use.resolve(db),
                    ),
                ]
            }

            Self::RecordFieldNotFound { primary, label } => {
                let label = label.data(db.as_hir_db());
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("field `{}` not found", label),
                    primary.resolve(db),
                )]
            }

            Self::ExplicitLabelExpectedInRecord { primary, hint } => {
                let mut diag = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "explicit label is required".to_string(),
                    primary.resolve(db),
                )];
                if let Some(hint) = hint {
                    diag.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("Consider using `{}` instead", hint),
                        primary.resolve(db),
                    ))
                }

                diag
            }

            Self::MissingRecordFields {
                primary,
                missing_fields,
                hint,
            } => {
                let missing = missing_fields
                    .iter()
                    .map(|id| id.data(db.as_hir_db()))
                    .join(", ");

                let mut diag = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format! {"missing `{}`", missing},
                    primary.resolve(db),
                )];
                if let Some(hint) = hint {
                    diag.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("Consider using `{}` instead", hint),
                        primary.resolve(db),
                    ))
                }
                diag
            }

            Self::UndefinedVariable(primary, ident) => {
                let ident = ident.data(db.as_hir_db());

                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("undefined variable `{}`", ident),
                    primary.resolve(db),
                )]
            }

            Self::ReturnedTypeMismatch {
                primary,
                actual,
                expected,
                func,
            } => {
                let mut diag = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("expected `{}`, but `{}` is returned", expected, actual),
                    primary.resolve(db),
                )];

                if let Some(func) = func {
                    if func.ret_ty(db.as_hir_db()).is_some() {
                        diag.push(SubDiagnostic::new(
                            LabelStyle::Secondary,
                            format!("this function expects `{}` to be returned", expected),
                            func.lazy_span().ret_ty_moved().resolve(db),
                        ))
                    } else {
                        diag.push(SubDiagnostic::new(
                            LabelStyle::Secondary,
                            format!("try adding `-> {}`", actual),
                            func.lazy_span().name_moved().resolve(db),
                        ))
                    }
                }

                diag
            }

            Self::TypeMustBeKnown(span) => vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "type must be known here".to_string(),
                span.resolve(db),
            )],

            Self::AccessedFieldNotFound {
                primary,
                given_ty,
                index,
            } => {
                let message = match index {
                    FieldIndex::Ident(ident) => {
                        format!(
                            "field `{}` is not found in `{}`",
                            ident.data(db.as_hir_db()),
                            &given_ty,
                        )
                    }
                    FieldIndex::Index(index) => {
                        format!(
                            "field `{}` is not found in `{}`",
                            index.data(db.as_hir_db()),
                            &given_ty
                        )
                    }
                };

                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    message,
                    primary.resolve(db),
                )]
            }

            Self::OpsTraitNotImplemented {
                span,
                ty,
                op,
                trait_path,
            } => {
                let op = op.data(db.as_hir_db());
                let trait_path = trait_path.pretty_print(db.as_hir_db());

                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!("`{}` cant be applied to `{}`", op, ty),
                        span.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format! {"Try implementing `{}` for `{}`", trait_path, ty},
                        span.resolve(db),
                    ),
                ]
            }

            Self::NonAssignableExpr(primary) => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "cant assign to this expression".to_string(),
                    primary.resolve(db),
                )]
            }

            Self::ImmutableAssignment { primary, binding } => {
                let mut diag = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "immutable assignment".to_string(),
                    primary.resolve(db),
                )];
                if let Some((name, span)) = binding {
                    diag.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("try changing to `mut {}`", name.data(db.as_hir_db())),
                        span.resolve(db),
                    ));
                }
                diag
            }

            Self::LoopControlOutsideOfLoop { primary, is_break } => {
                let stmt = if *is_break { "break" } else { "continue" };
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{}` is not allowed here", stmt),
                    primary.resolve(db),
                )]
            }

            Self::TraitNotImplemented {
                primary,
                ty,
                trait_name,
            } => {
                let trait_name = trait_name.data(db.as_hir_db());
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!("`{trait_name}` needs to be implemented for `{ty}`"),
                        primary.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("consider implementing `{trait_name}` for `{ty}`"),
                        primary.resolve(db),
                    ),
                ]
            }

            Self::NotCallable(primary, ty) => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{ty}` is not callable"),
                    primary.resolve(db),
                )]
            }

            Self::CallGenericArgNumMismatch {
                primary,
                def_span,
                given,
                expected,
            } => {
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!(
                            "expected {} generic arguments, but {} given",
                            expected, given
                        ),
                        primary.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "function defined here".to_string(),
                        def_span.resolve(db),
                    ),
                ]
            }

            Self::CallArgNumMismatch {
                primary,
                def_span,
                given,
                expected,
            } => {
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!("expected {} arguments, but {} given", expected, given),
                        primary.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "function defined here".to_string(),
                        def_span.resolve(db),
                    ),
                ]
            }

            Self::CallArgLabelMismatch {
                primary,
                def_span,
                given,
                expected,
            } => {
                let mut diags = if let Some(given) = given {
                    vec![SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!(
                            "expected `{}` label, but `{}` given",
                            expected.data(db.as_hir_db()),
                            given.data(db.as_hir_db())
                        ),
                        primary.resolve(db),
                    )]
                } else {
                    vec![SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!("expected `{}` label", expected.data(db.as_hir_db())),
                        primary.resolve(db),
                    )]
                };

                diags.push(SubDiagnostic::new(
                    LabelStyle::Secondary,
                    "function defined here".to_string(),
                    def_span.resolve(db),
                ));

                diags
            }

            Self::AmbiguousInherentMethodCall {
                primary,
                method_name,
                cand_spans: candidates,
            } => {
                let method_name = method_name.data(db.as_hir_db());
                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{}` is ambiguous", method_name),
                    primary.resolve(db),
                )];

                for candidate in candidates.iter() {
                    diags.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("`{method_name}` is defined here"),
                        candidate.resolve(db),
                    ));
                }

                diags
            }

            Self::AmbiguousTrait {
                primary,
                method_name,
                traits,
            } => {
                let method_name = method_name.data(db.as_hir_db());
                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{method_name}` is ambiguous"),
                    primary.resolve(db),
                )];

                for trait_ in traits.iter() {
                    let trait_name = trait_.name(db.as_hir_db()).unwrap().data(db.as_hir_db());
                    diags.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("candidate: `{trait_name}::{method_name}`"),
                        primary.resolve(db),
                    ));
                }

                diags
            }

            Self::AmbiguousTraitInst { primary, cands } => {
                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "multiple implementations are found".to_string(),
                    primary.resolve(db),
                )];

                for cand in cands {
                    diags.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("candidate: {cand}"),
                        primary.resolve(db),
                    ))
                }

                diags
            }

            Self::InvisibleAmbiguousTrait { primary, traits } => {
                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "consider importing one of the following traits into the scope to resolve the ambiguity"
                        .to_string(),
                    primary.resolve(db),
                )];

                for trait_ in traits {
                    if let Some(path) = trait_.scope().pretty_path(db.as_hir_db()) {
                        let diag = SubDiagnostic::new(
                            LabelStyle::Secondary,
                            format!("`use {path}`"),
                            primary.resolve(db),
                        );
                        diags.push(diag);
                    };
                }

                diags
            }

            Self::MethodNotFound {
                primary,
                method_name,
                receiver: ty,
            } => {
                let method_name = method_name.data(db.as_hir_db());
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{}` is not found in `{}`", method_name, ty),
                    primary.resolve(db),
                )]
            }

            Self::NotValue { primary, given } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!(
                        "`{}` cannot be used as a value",
                        match given {
                            Either::Left(item) => item.kind_name(),
                            Either::Right(_) => "type",
                        }
                    ),
                    primary.resolve(db),
                )]
            }

            Self::TypeAnnotationNeeded {
                primary,
                ty,
                is_integral,
            } => {
                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "type annotation is needed".to_string(),
                    primary.resolve(db),
                )];

                let sub_diag_msg = match ty {
                    Some(ty) => format!("consider giving `: {ty}` here"),
                    None if *is_integral => "no default type is provided for an integer type. consider giving integer type".to_string(),
                    None => "consider giving `: Type` here".to_string(),
                };

                diags.push(SubDiagnostic::new(
                    LabelStyle::Secondary,
                    sub_diag_msg,
                    primary.resolve(db),
                ));

                diags
            }
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }
}

impl<'db> DiagnosticVoucher<'db> for BodyDiag<'db> {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(DiagnosticPass::TyCheck, self.local_code())
    }

    fn to_complete(&self, db: &'db dyn SpannedHirDb) -> CompleteDiagnostic {
        let severity = self.severity();
        let error_code = self.error_code();
        let message = self.message(db.as_hir_db());
        let sub_diags = self.sub_diags(db);

        CompleteDiagnostic::new(severity, message, sub_diags, vec![], error_code)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitLowerDiag<'db> {
    ExternalTraitForExternalType(DynLazySpan<'db>),

    ConflictTraitImpl {
        primary: ImplTrait<'db>,
        conflict_with: ImplTrait<'db>,
    },

    CyclicSuperTraits(DynLazySpan<'db>),
}

impl<'db> TraitLowerDiag<'db> {
    pub(super) fn external_trait_for_external_type(impl_trait: ImplTrait<'db>) -> Self {
        Self::ExternalTraitForExternalType(impl_trait.lazy_span().trait_ref().into())
    }

    pub(super) fn conflict_impl(primary: ImplTrait<'db>, conflict_with: ImplTrait<'db>) -> Self {
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

impl<'db> DiagnosticVoucher<'db> for TraitLowerDiag<'db> {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(DiagnosticPass::ImplTraitDefinition, self.local_code())
    }

    fn to_complete(&self, db: &'db dyn hir::SpannedHirDb) -> CompleteDiagnostic {
        let severity = self.severity();
        let error_code = self.error_code();
        let message = self.message();
        let sub_diags = self.sub_diags(db);

        CompleteDiagnostic::new(severity, message, sub_diags, vec![], error_code)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitConstraintDiag<'db> {
    KindMismatch {
        primary: DynLazySpan<'db>,
        trait_def: Trait<'db>,
    },

    TraitArgNumMismatch {
        span: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    },

    TraitArgKindMismatch(DynLazySpan<'db>, String),

    TraitBoundNotSat(DynLazySpan<'db>, String, Option<String>),

    InfiniteBoundRecursion(DynLazySpan<'db>, String),

    ConcreteTypeBound(DynLazySpan<'db>, String),

    ConstTyBound(DynLazySpan<'db>, String),
}

impl<'db> TraitConstraintDiag<'db> {
    pub(super) fn kind_mismatch(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        expected: &Kind,
        actual: TyId<'db>,
    ) -> Self {
        let actual_kind = actual.kind(db);
        let ty_display = actual.pretty_print(db);
        let msg = format!(
            "expected `{}` kind, but `{}` has `{}` kind",
            expected, ty_display, actual_kind,
        );
        Self::TraitArgKindMismatch(span, msg)
    }

    pub(super) fn trait_arg_num_mismatch(
        span: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    ) -> Self {
        Self::TraitArgNumMismatch {
            span,
            expected,
            given,
        }
    }

    pub(super) fn trait_bound_not_satisfied(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        primary_goal: TraitInstId<'db>,
        unsat_subgoal: Option<TraitInstId<'db>>,
    ) -> Self {
        let msg = format!(
            "`{}` doesn't implement `{}`",
            primary_goal.self_ty(db).pretty_print(db),
            primary_goal.pretty_print(db, false)
        );

        let unsat_subgoal = unsat_subgoal.map(|unsat| {
            format!(
                "trait bound `{}` is not satisfied",
                unsat.pretty_print(db, true)
            )
        });
        Self::TraitBoundNotSat(span, msg, unsat_subgoal)
    }

    pub(super) fn const_ty_bound(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        span: DynLazySpan<'db>,
    ) -> Self {
        let msg = format!("`{}` is a const type", ty.pretty_print(db));
        Self::ConstTyBound(span, msg)
    }

    pub(super) fn concrete_type_bound(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        ty: TyId<'db>,
    ) -> Self {
        let msg = format!("`{}` is a concrete type", ty.pretty_print(db));
        Self::ConcreteTypeBound(span, msg)
    }

    fn local_code(&self) -> u16 {
        match self {
            Self::KindMismatch { .. } => 0,
            Self::TraitArgNumMismatch { .. } => 1,
            Self::TraitArgKindMismatch(_, _) => 2,
            Self::TraitBoundNotSat(..) => 3,
            Self::InfiniteBoundRecursion(..) => 4,
            Self::ConcreteTypeBound(..) => 5,
            Self::ConstTyBound(..) => 6,
        }
    }

    fn message(&self) -> String {
        match self {
            Self::KindMismatch { .. } => "type doesn't satisfy required kind bound".to_string(),

            Self::TraitArgNumMismatch { .. } => "given trait argument number mismatch".to_string(),

            Self::TraitArgKindMismatch(..) => "given trait argument kind mismatch".to_string(),

            Self::TraitBoundNotSat(..) => "trait bound is not satisfied".to_string(),

            Self::InfiniteBoundRecursion(..) => "infinite trait bound recursion".to_string(),

            Self::ConcreteTypeBound(..) => {
                "trait bound for concrete type is not allowed".to_string()
            }

            Self::ConstTyBound(..) => "trait bound for const type is not allowed".to_string(),
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

            Self::TraitBoundNotSat(span, msg, subgoal) => {
                let mut diags = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    msg.clone(),
                    span.resolve(db),
                )];

                if let Some(subgoal) = subgoal {
                    diags.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        subgoal.clone(),
                        span.resolve(db),
                    ))
                }

                diags
            }

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

            Self::ConstTyBound(span, msg) => vec![SubDiagnostic::new(
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

impl<'db> DiagnosticVoucher<'db> for TraitConstraintDiag<'db> {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(DiagnosticPass::TraitSatisfaction, self.local_code())
    }

    fn to_complete(&self, db: &'db dyn SpannedHirDb) -> CompleteDiagnostic {
        let severity = self.severity();
        let error_code = self.error_code();
        let message = self.message();
        let sub_diags = self.sub_diags(db);

        CompleteDiagnostic::new(severity, message, sub_diags, vec![], error_code)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImplDiag<'db> {
    ConflictMethodImpl {
        primary: DynLazySpan<'db>,
        conflict_with: DynLazySpan<'db>,
    },

    MethodNotDefinedInTrait {
        primary: DynLazySpan<'db>,
        trait_: Trait<'db>,
        method_name: IdentId<'db>,
    },

    NotAllTraitItemsImplemented {
        primary: DynLazySpan<'db>,
        not_implemented: Vec<IdentId<'db>>,
    },

    MethodTypeParamNumMismatch {
        primary: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    },

    MethodTypeParamKindMismatch {
        primary: DynLazySpan<'db>,
        message: String,
    },

    MethodArgNumMismatch {
        primary: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    },

    MethodArgLabelMismatch {
        primary: DynLazySpan<'db>,
        definition: DynLazySpan<'db>,
        message: String,
    },

    MethodArgTyMismatch {
        primary: DynLazySpan<'db>,
        message: String,
    },

    MethodRetTyMismatch {
        primary: DynLazySpan<'db>,
        message: String,
    },

    MethodStricterBound {
        primary: DynLazySpan<'db>,
        message: String,
    },

    InvalidSelfType {
        primary: DynLazySpan<'db>,
        message: String,
    },

    InherentImplIsNotAllowed {
        primary: DynLazySpan<'db>,
        ty: String,
        is_nominal: bool,
    },
}

impl<'db> ImplDiag<'db> {
    pub(super) fn conflict_method_impl(
        primary: DynLazySpan<'db>,
        conflict_with: DynLazySpan<'db>,
    ) -> Self {
        Self::ConflictMethodImpl {
            primary,
            conflict_with,
        }
    }

    pub(super) fn method_not_defined_in_trait(
        primary: DynLazySpan<'db>,
        trait_: Trait<'db>,
        method_name: IdentId<'db>,
    ) -> Self {
        Self::MethodNotDefinedInTrait {
            primary,
            trait_,
            method_name,
        }
    }

    pub(super) fn not_all_trait_items_implemented(
        primary: DynLazySpan<'db>,
        not_implemented: Vec<IdentId<'db>>,
    ) -> Self {
        Self::NotAllTraitItemsImplemented {
            primary,
            not_implemented,
        }
    }

    pub(super) fn method_param_num_mismatch(
        primary: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    ) -> Self {
        Self::MethodTypeParamNumMismatch {
            primary,
            expected,
            given,
        }
    }

    pub(super) fn method_arg_num_mismatch(
        primary: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    ) -> Self {
        Self::MethodArgNumMismatch {
            primary,
            expected,
            given,
        }
    }

    pub fn method_arg_ty_mismatch(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
    ) -> Self {
        let message = format!(
            "expected `{}` type, but the given type is `{}`",
            expected.pretty_print(db),
            given.pretty_print(db),
        );

        Self::MethodArgTyMismatch { primary, message }
    }

    pub fn method_arg_label_mismatch(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        definition: DynLazySpan<'db>,
        expected: FuncParamName<'db>,
        given: FuncParamName<'db>,
    ) -> Self {
        let message = format!(
            "expected `{}` label, but the given label is `{}`",
            expected.pretty_print(db.as_hir_db()),
            given.pretty_print(db.as_hir_db())
        );

        Self::MethodArgLabelMismatch {
            primary,
            definition,
            message,
        }
    }

    pub fn method_ret_type_mismatch(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        expected: TyId<'db>,
        given: TyId<'db>,
    ) -> Self {
        let message = format!(
            "expected `{}` type, but the given type is `{}`",
            expected.pretty_print(db),
            given.pretty_print(db),
        );

        Self::MethodRetTyMismatch { primary, message }
    }

    pub(super) fn method_param_kind_mismatch(
        primary: DynLazySpan<'db>,
        expected: &Kind,
        given: &Kind,
    ) -> Self {
        let message = format!(
            "expected `{}` kind, but the given type has `{}` kind",
            expected, given,
        );

        Self::MethodTypeParamKindMismatch { primary, message }
    }

    pub(super) fn method_stricter_bound(
        db: &'db dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        stricter_bounds: &[TraitInstId<'db>],
    ) -> Self {
        let message = format!(
            "method has stricter bounds than the declared method in the trait: {}",
            stricter_bounds
                .iter()
                .map(|pred| format!("`{}`", pred.pretty_print(db, true)))
                .join(", ")
        );
        Self::MethodStricterBound { primary, message }
    }

    pub(super) fn invalid_self_ty(
        db: &dyn HirAnalysisDb,
        primary: DynLazySpan<'db>,
        expected: TyId<'db>,
        actual: TyId<'db>,
    ) -> Self {
        let message = if !expected.is_trait_self(db) {
            format!(
                "type of `self` must starts with `Self` or `{}`, but the given type is `{}`",
                expected.pretty_print(db),
                actual.pretty_print(db),
            )
        } else {
            format!(
                "type of `self` must starts with `Self`, but the given type is `{}`",
                actual.pretty_print(db),
            )
        };

        Self::InvalidSelfType { primary, message }
    }

    pub fn local_code(&self) -> u16 {
        match self {
            Self::ConflictMethodImpl { .. } => 0,
            Self::MethodNotDefinedInTrait { .. } => 1,
            Self::NotAllTraitItemsImplemented { .. } => 2,
            Self::MethodTypeParamNumMismatch { .. } => 3,
            Self::MethodTypeParamKindMismatch { .. } => 4,
            Self::MethodArgNumMismatch { .. } => 5,
            Self::MethodArgLabelMismatch { .. } => 6,
            Self::MethodArgTyMismatch { .. } => 7,
            Self::MethodRetTyMismatch { .. } => 8,
            Self::MethodStricterBound { .. } => 9,
            Self::InvalidSelfType { .. } => 10,
            Self::InherentImplIsNotAllowed { .. } => 11,
        }
    }

    fn message(&self, db: &dyn HirDb) -> String {
        match self {
            Self::ConflictMethodImpl { .. } => "conflict method implementation".to_string(),
            Self::MethodNotDefinedInTrait {
                trait_,
                method_name,
                ..
            } => format!(
                "method `{}` is not defined in trait `{}`",
                method_name.data(db),
                trait_.name(db).unwrap().data(db),
            ),

            Self::NotAllTraitItemsImplemented { .. } => {
                "not all trait methods are implemented".to_string()
            }

            Self::MethodTypeParamNumMismatch { .. } => {
                "trait method type parameter number mismatch".to_string()
            }

            Self::MethodTypeParamKindMismatch { .. } => {
                "trait method type parameter kind mismatch".to_string()
            }

            Self::MethodArgNumMismatch { .. } => {
                "trait method argument number mismatch".to_string()
            }

            Self::MethodArgLabelMismatch { .. } => {
                "given argument label doesn't match the expected label required by trait"
                    .to_string()
            }

            Self::MethodArgTyMismatch { .. } => {
                "given argument type doesn't match the expected type required by trait".to_string()
            }

            Self::MethodRetTyMismatch { .. } => {
                "given return type doesn't match the expected type required by trait".to_string()
            }

            Self::MethodStricterBound { .. } => {
                "impl method has stricter bound than the declared method in the trait".to_string()
            }

            Self::InvalidSelfType { .. } => "invalid type for `self` argument".to_string(),

            Self::InherentImplIsNotAllowed { .. } => "inherent impl is not allowed".to_string(),
        }
    }

    fn sub_diags(&self, db: &dyn SpannedHirDb) -> Vec<SubDiagnostic> {
        match self {
            Self::ConflictMethodImpl {
                primary,
                conflict_with,
            } => vec![
                SubDiagnostic::new(
                    LabelStyle::Primary,
                    "conflict method implementation".to_string(),
                    primary.resolve(db),
                ),
                SubDiagnostic::new(
                    LabelStyle::Secondary,
                    "conflict with this method implementation".to_string(),
                    conflict_with.resolve(db),
                ),
            ],

            Self::MethodNotDefinedInTrait {
                primary,
                trait_,
                method_name,
            } => {
                vec![
                    SubDiagnostic::new(
                        LabelStyle::Primary,
                        format!(
                            "method `{}` is not defined in trait `{}`",
                            method_name.data(db.as_hir_db()),
                            trait_.name(db.as_hir_db()).unwrap().data(db.as_hir_db()),
                        ),
                        primary.resolve(db),
                    ),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "trait is defined here".to_string(),
                        trait_.lazy_span().name().resolve(db),
                    ),
                ]
            }

            Self::NotAllTraitItemsImplemented {
                primary,
                not_implemented,
            } => {
                let not_implemented: String = not_implemented
                    .iter()
                    .map(|name| name.data(db.as_hir_db()))
                    .join(", ");

                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!(
                        "all required trait items must be implemented, missing: `{}`",
                        not_implemented
                    ),
                    primary.resolve(db),
                )]
            }

            Self::MethodTypeParamNumMismatch {
                primary,
                expected,
                given,
            } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!(
                        "expected {} type parameters here, but {} given",
                        expected, given
                    ),
                    primary.resolve(db),
                )]
            }

            Self::MethodTypeParamKindMismatch { primary, message } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    message.clone(),
                    primary.resolve(db),
                )]
            }

            Self::MethodArgNumMismatch {
                primary,
                expected,
                given,
            } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("expected {} arguments here, but {} given", expected, given),
                    primary.resolve(db),
                )]
            }

            Self::MethodArgLabelMismatch {
                primary,
                definition,
                message,
            } => {
                vec![
                    SubDiagnostic::new(LabelStyle::Primary, message.clone(), primary.resolve(db)),
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        "argument label is defined here".to_string(),
                        definition.resolve(db),
                    ),
                ]
            }

            Self::MethodArgTyMismatch { primary, message } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    message.clone(),
                    primary.resolve(db),
                )]
            }

            Self::MethodRetTyMismatch { primary, message } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    message.clone(),
                    primary.resolve(db),
                )]
            }

            Self::MethodStricterBound { primary, message } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    message.clone(),
                    primary.resolve(db),
                )]
            }

            Self::InvalidSelfType { primary, message } => {
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    message.clone(),
                    primary.resolve(db),
                )]
            }

            Self::InherentImplIsNotAllowed {
                primary,
                ty,
                is_nominal,
            } => {
                let msg = if *is_nominal {
                    format!("inherent impl is not allowed for foreign type `{}`", ty)
                } else {
                    "inherent impl is not allowed for non nominal type".to_string()
                };

                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    msg,
                    primary.resolve(db),
                )]
            }
        }
    }

    fn severity(&self) -> Severity {
        Severity::Error
    }
}

impl<'db> DiagnosticVoucher<'db> for ImplDiag<'db> {
    fn error_code(&self) -> GlobalErrorCode {
        GlobalErrorCode::new(DiagnosticPass::TraitSatisfaction, self.local_code())
    }

    fn to_complete(&self, db: &'db dyn SpannedHirDb) -> CompleteDiagnostic {
        let severity = self.severity();
        let error_code = self.error_code();
        let message = self.message(db.as_hir_db());
        let sub_diags = self.sub_diags(db);

        CompleteDiagnostic::new(severity, message, sub_diags, vec![], error_code)
    }
}
