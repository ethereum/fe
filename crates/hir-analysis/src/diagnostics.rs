//! This module defines the diagnostics that can be accumulated inside salsa-db
//! with span-agnostic forms. All diagnostics accumulated in salsa-db should
//! implement [`DiagnosticVoucher`] which defines the conversion into
//! [`CompleteDiagnostic`].

use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, Span, SpanKind,
    SubDiagnostic,
};
use either::Either;
use hir::{
    hir_def::FieldIndex,
    span::{DynLazySpan, LazySpan},
    ParserError,
};

use crate::{
    name_resolution::diagnostics::NameResDiag,
    ty::diagnostics::{
        BodyDiag, FuncBodyDiag, ImplDiag, TraitConstraintDiag, TraitLowerDiag, TyDiagCollection,
        TyLowerDiag,
    },
    HirAnalysisDb,
};

/// All diagnostics accumulated in salsa-db should implement
/// [`DiagnosticVoucher`] which defines the conversion into
/// [`CompleteDiagnostic`].
///
/// All types that implement `DiagnosticVoucher` must NOT have a span
/// information which invalidates cache in salsa-db. Instead of it, the all
/// information is given by [`SpannedHirDb`] to allow evaluating span lazily.
///
/// The reason why we use `DiagnosticVoucher` is that we want to evaluate span
/// lazily to avoid invalidating cache in salsa-db.
///
/// To obtain a span from HIR nodes in a lazy manner, it's recommended to use
/// `[LazySpan]`(crate::span::LazySpan) and types that implement `LazySpan`.
pub trait DiagnosticVoucher<'db>: Send {
    /// Makes a [`CompleteDiagnostic`].
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic;
}

impl DiagnosticVoucher<'_> for CompleteDiagnostic {
    fn to_complete(&self, _db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        self.clone()
    }
}

impl<'db> DiagnosticVoucher<'db> for Box<dyn DiagnosticVoucher<'db> + 'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        self.as_ref().to_complete(db)
    }
}

#[salsa::db]
pub trait SpannedHirAnalysisDb:
    salsa::Database + hir::HirDb + hir::SpannedHirDb + HirAnalysisDb
{
    fn as_spanned_hir_analysis_db(&self) -> &dyn SpannedHirAnalysisDb;

    fn resolve(&self, span: &DynLazySpan) -> Option<Span> {
        span.resolve(self.as_spanned_hir_db())
    }
}

// `ParseError` has span information, but this is not a problem because the
// parsing procedure itself depends on the file content, and thus span
// information.
impl<'db> DiagnosticVoucher<'db> for ParserError {
    fn to_complete(&self, _db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::Parse, 1);
        let span = Span::new(self.file, self.error.range(), SpanKind::Original);
        CompleteDiagnostic::new(
            Severity::Error,
            self.error.msg(),
            vec![SubDiagnostic::new(
                LabelStyle::Primary,
                self.error.label(),
                Some(span),
            )],
            vec![],
            error_code,
        )
    }
}

pub trait LazyDiagnostic<'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic;
}

impl<'db> DiagnosticVoucher<'db> for FuncBodyDiag<'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        match self {
            Self::Ty(diag) => diag.to_complete(db),
            Self::Body(diag) => diag.to_complete(db),
            Self::NameRes(diag) => diag.to_complete(db),
        }
    }
}

impl<'db> DiagnosticVoucher<'db> for TyDiagCollection<'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        match self {
            Self::Ty(diag) => diag.to_complete(db),
            Self::Satisfiability(diag) => diag.to_complete(db),
            Self::TraitLower(diag) => diag.to_complete(db),
            Self::Impl(diag) => diag.to_complete(db),
        }
    }
}

impl<'db> DiagnosticVoucher<'db> for NameResDiag<'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::NameResolution, self.local_code());
        let severity = Severity::Error;
        match self {
            Self::Conflict(ident, conflicts) => {
                let ident = ident.data(db.as_hir_db());
                let mut spans: Vec<_> = conflicts
                    .iter()
                    .filter_map(|span| span.resolve(db))
                    .collect();
                spans.sort_unstable();
                let mut spans = spans.into_iter();
                let mut diags = Vec::with_capacity(conflicts.len());
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

                CompleteDiagnostic {
                    severity,
                    message: format!("`{}` conflicts with other definitions", ident),
                    sub_diagnostics: diags,
                    notes: vec![],
                    error_code,
                }
            }

            Self::NotFound(prim_span, ident) => {
                let ident = ident.data(db.as_hir_db());
                CompleteDiagnostic {
                    severity,
                    message: format!("`{}` is not found", ident),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{ident}` is not found"),
                        span: db.resolve(prim_span),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::Invisible(prim_span, ident, span) => {
                let ident = ident.data(db.as_hir_db());

                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{ident}` is not visible"),
                    span: db.resolve(prim_span),
                }];
                if let Some(span) = span {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{ident}` is defined here"),
                        span: db.resolve(span),
                    });
                }

                CompleteDiagnostic {
                    severity,
                    message: format!("`{ident}` is not visible"),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::Ambiguous(prim_span, ident, candidates) => {
                let ident = ident.data(db.as_hir_db());
                let mut diags = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{ident}` is ambiguous"),
                    span: db.resolve(prim_span),
                }];

                let mut cand_spans: Vec<_> = candidates
                    .iter()
                    .filter_map(|span| span.resolve(db))
                    .collect();
                cand_spans.sort_unstable();
                diags.extend(cand_spans.into_iter().enumerate().map(|(i, span)| {
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("candidate {}", i + 1),
                        Some(span),
                    )
                }));

                CompleteDiagnostic {
                    severity,
                    message: format!("`{ident}` is ambiguous"),
                    sub_diagnostics: diags,
                    notes: vec![],
                    error_code,
                }
            }

            Self::InvalidPathSegment(prim_span, name, res_span) => {
                let name = name.data(db.as_hir_db());
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{}` can't be used as a middle segment of a path", name,),
                    span: db.resolve(prim_span),
                }];

                if let Some(span) = res_span {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{name}` is defined here"),
                        span: db.resolve(span),
                    });
                }

                CompleteDiagnostic {
                    severity,
                    message: format!("`{name}` can't be used as a middle segment of a path"),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }

            Self::ExpectedType(prim_span, name, given_kind) => {
                let name = name.data(db.as_hir_db());
                CompleteDiagnostic {
                    severity,
                    message: "expected type item here".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("expected type here, but found {given_kind} `{name}`"),
                        span: prim_span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::ExpectedTrait(prim_span, name, given_kind) => {
                let name = name.data(db.as_hir_db());
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected trait item here".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("expected trait here, but found {given_kind} `{name}`"),
                        span: prim_span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::ExpectedValue(prim_span, name, given_kind) => {
                let name = name.data(db.as_hir_db());
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected value here".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("expected value here, but found {given_kind} `{name}`"),
                        span: prim_span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::TooManyGenericArgs {
                span,
                expected,
                given,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: format!("too many generic args; expected {expected}, given {given}"),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected {expected} arguments here, but {given} given"),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },
        }
    }
}

impl<'db> DiagnosticVoucher<'db> for TyLowerDiag<'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let ha_db = db.as_hir_analysis_db();
        let error_code = GlobalErrorCode::new(DiagnosticPass::TypeDefinition, self.local_code());
        match self {
            Self::ExpectedStarKind(span) => {
                // find expected ty name, num of generic args, etc
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected `*` kind in this context".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "expected `*` kind here".to_string(),
                        span: span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::InvalidTypeArgKind {
                span,
                given,
                expected,
            } => {
                let msg = if let Some(expected) = expected {
                    let arg_kind = given.kind(ha_db);
                    debug_assert!(!expected.does_match(arg_kind));

                    format!(
                        "expected `{}` kind, but `{}` has `{}` kind",
                        expected,
                        given.pretty_print(ha_db),
                        arg_kind
                    )
                } else {
                    "too many generic arguments".to_string()
                };

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "invalid type argument kind".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: msg.to_string(),
                        span: span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::TooManyGenericArgs {
                span,
                expected,
                given,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: format!("too many generic args; expected {expected}, given {given}"),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected {expected} arguments, but {given} were given"),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::RecursiveType {
                primary_span,
                field_span,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "recursive type is not allowed".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "recursive type definition here".to_string(),
                        span: primary_span.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "recursion occurs here".to_string(),
                        span: field_span.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },
            Self::UnboundTypeAliasParam {
                span,
                alias,
                n_given_args: _,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "all type parameters of type alias must be given".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "expected at least {} arguments here",
                            alias.generic_params(db.as_hir_db()).len(db.as_hir_db())
                        ),
                        span: span.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "type alias defined here".to_string(),
                        span: alias.lazy_span().resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::TypeAliasCycle { primary, cycle } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "recursive type alias cycle is detected".to_string(),
                sub_diagnostics: {
                    let mut labels = vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "cycle happens here".to_string(),
                        span: primary.resolve(db),
                    }];
                    labels.extend(cycle.iter().map(|type_alias| SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "type alias defined here".to_string(),
                        span: type_alias.lazy_span().alias_moved().resolve(db),
                    }));
                    labels
                },
                notes: vec![],
                error_code,
            },

            Self::InconsistentKindBound { span, ty, old, new } => {
                let msg = format!(
                    "`{}` is already declared with `{}` kind, but found `{}` kind here",
                    ty.pretty_print(ha_db),
                    old,
                    new
                );

                CompleteDiagnostic {
                    severity: Severity::Error,
                    // xxx improve message
                    message: "duplicate type bound is not allowed.".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: msg.to_string(),
                        span: span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::KindBoundNotAllowed(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "kind bound is not allowed".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "kind bound is not allowed here".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::GenericParamAlreadyDefinedInParent {
                span,
                conflict_with,
                name,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "generic parameter is already defined in the parent item".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{}` is already defined", name.data(db.as_hir_db())),
                        span: span.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "conflict with this generic parameter".to_string(),
                        span: conflict_with.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::DuplicatedArgName {
                primary,
                conflict_with,
                name,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "duplicated argument name in function definition is not allowed"
                    .to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "duplicated argument name `{}`",
                            name.data(db.as_hir_db())
                        ),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "conflict with this argument name".to_string(),
                        span: conflict_with.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::InvalidConstParamTy(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "invalid const parameter type".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "only integer or bool types are allowed as a const parameter type"
                        .to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::RecursiveConstParamTy(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "recursive const parameter type is not allowed".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "recursive const parameter type is detected here".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConstTyMismatch {
                span,
                expected,
                given,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "given type doesn't match the expected const type".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected `{}` type here, but `{}` is given",
                        expected.pretty_print(ha_db),
                        given.pretty_print(ha_db)
                    ),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConstTyExpected { span, expected } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "expected const type".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected const type of `{}` here",
                        expected.pretty_print(ha_db)
                    ),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::NormalTypeExpected { span, given } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "expected a normal type".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected a normal type here, but `{}` is given",
                        given.pretty_print(ha_db)
                    ),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::AssocTy(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "associated type is not supported ".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "associated type is not implemented".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::InvalidConstTyExpr(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "the expression is not supported yet in a const type context".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "only literal expression is supported".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },
        }
    }
}

impl<'db> DiagnosticVoucher<'db> for BodyDiag<'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::TyCheck, self.local_code());
        let severity = Severity::Error;

        match self {
            Self::TypeMismatch(span, expected, actual) => CompleteDiagnostic {
                severity,
                message: "type mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected `{expected}`, but `{actual}` is given"),
                    span: span.resolve(db),
                }],
                error_code,
                notes: vec![],
            },
            Self::InfiniteOccurrence(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "infinite sized type found".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "infinite sized type found".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::DuplicatedBinding {
                primary,
                conflicat_with,
                name,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: format!(
                    "duplicate binding `{}` in pattern",
                    name.data(db.as_hir_db())
                ),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{}` is defined again here", name.data(db.as_hir_db())),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!(
                            "first definition of `{}` in this pattern",
                            name.data(db.as_hir_db())
                        ),
                        span: conflicat_with.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::DuplicatedRestPat(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "duplicate `..` in pattern".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "`..` can be used only once".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::InvalidPathDomainInPat { primary, resolved } => {
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "expected type or enum variant here".to_string(),
                    span: primary.resolve(db),
                }];

                if let Some(resolved) = resolved {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "this item given".to_string(),
                        span: resolved.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "invalid item is given here".to_string(),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }

            Self::UnitVariantExpected {
                primary,
                kind_name,
                hint,
            } => {
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected unit variant here, but found {}", kind_name),
                    span: primary.resolve(db),
                }];

                if let Some(hint) = hint {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("Consider using `{}` instead", hint),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected unit variant".to_string(),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }

            Self::TupleVariantExpected {
                primary,
                kind_name,
                hint,
            } => {
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: if let Some(kind_name) = kind_name {
                        format!("expected tuple variant here, but found {}", kind_name)
                    } else {
                        "expected tuple variant here".to_string()
                    },
                    span: primary.resolve(db),
                }];

                if let Some(hint) = hint {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("Consider using `{}` instead", hint),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected tuple variant".to_string(),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }

            Self::RecordExpected {
                primary,
                kind_name,
                hint,
            } => {
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: if let Some(kind_name) = kind_name {
                        format!(
                            "expected record variant or struct here, but found {}",
                            kind_name
                        )
                    } else {
                        "expected record variant or struct here".to_string()
                    },
                    span: primary.resolve(db),
                }];

                if let Some(hint) = hint {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("Consider using `{}` instead", hint),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected record variant or struct".to_string(),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }

            Self::MismatchedFieldCount {
                primary,
                expected,
                given,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "field count mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected {} fields here, but {} given", expected, given),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::DuplicatedRecordFieldBind {
                primary,
                first_use,
                name,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "duplicated record field binding".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("duplicate field binding `{}`", name.data(db.as_hir_db())),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("first use of `{}`", name.data(db.as_hir_db())),
                        span: first_use.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::RecordFieldNotFound { primary, label } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "specified field not found".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("field `{}` not found", label.data(db.as_hir_db())),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ExplicitLabelExpectedInRecord { primary, hint } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "explicit label is required".to_string(),
                    span: primary.resolve(db),
                }];

                if let Some(hint) = hint {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("Consider using `{}` instead", hint),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "explicit label is required".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::MissingRecordFields {
                primary,
                missing_fields,
                hint,
            } => {
                let missing = missing_fields
                    .iter()
                    .map(|id| id.data(db.as_hir_db()).as_str())
                    .collect::<Vec<_>>()
                    .join(", ");

                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("missing `{}`", missing),
                    span: primary.resolve(db),
                }];

                if let Some(hint) = hint {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("Consider using `{}` instead", hint),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "missing fields in record pattern".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::UndefinedVariable(primary, ident) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "undefined variable".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("undefined variable `{}`", ident.data(db.as_hir_db())),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ReturnedTypeMismatch {
                primary,
                actual,
                expected,
                func,
            } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected `{}`, but `{}` is returned", expected, actual),
                    span: primary.resolve(db),
                }];

                if let Some(func) = func {
                    if func.ret_ty(db.as_hir_db()).is_some() {
                        sub_diagnostics.push(SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: format!("this function expects `{}` to be returned", expected),
                            span: func.lazy_span().ret_ty_moved().resolve(db),
                        });
                    } else {
                        sub_diagnostics.push(SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: format!("try adding `-> {}`", actual),
                            span: func.lazy_span().name_moved().resolve(db),
                        });
                    }
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "returned type mismatch".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            Self::TypeMustBeKnown(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "type must be known here".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "type must be known here".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::AccessedFieldNotFound {
                primary,
                given_ty,
                index,
            } => {
                let message = match index {
                    FieldIndex::Ident(ident) => format!(
                        "field `{}` is not found in `{}`",
                        ident.data(db.as_hir_db()),
                        given_ty
                    ),
                    FieldIndex::Index(index) => format!(
                        "field `{}` is not found in `{}`",
                        index.data(db.as_hir_db()),
                        given_ty
                    ),
                };

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "invalid field index".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message,
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::OpsTraitNotImplemented {
                span,
                ty,
                op,
                trait_path,
            } => {
                let sub_diagnostics = vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "`{}` can't be applied to `{}`",
                            op.data(db.as_hir_db()),
                            ty
                        ),
                        span: span.resolve(db),
                    },
                    // xxx move to hint
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!(
                            "Try implementing `{}` for `{}`",
                            trait_path.pretty_print(db.as_hir_db()),
                            ty
                        ),
                        span: span.resolve(db),
                    },
                ];

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "`{}` trait is not implemented",
                        trait_path.pretty_print(db.as_hir_db())
                    ),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::NonAssignableExpr(primary) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "not assignable left-hand side of assignment".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "cant assign to this expression".to_string(),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ImmutableAssignment { primary, binding } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "immutable assignment".to_string(),
                    span: primary.resolve(db),
                }];

                if let Some((name, span)) = binding {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("try changing to `mut {}`", name.data(db.as_hir_db())),
                        span: span.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "left-hand side of assignment is immutable".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::LoopControlOutsideOfLoop { primary, is_break } => {
                let stmt = if *is_break { "break" } else { "continue" };

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!("`{}` is not allowed outside of a loop", stmt),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{}` is not allowed here", stmt),
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::TraitNotImplemented {
                primary,
                ty,
                trait_name,
            } => {
                let trait_name = trait_name.data(db.as_hir_db());

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!("`{}` needs to be implemented for {}", trait_name, ty),
                    sub_diagnostics: vec![
                        SubDiagnostic {
                            style: LabelStyle::Primary,
                            message: format!("`{trait_name}` needs to be implemented for `{ty}`"),
                            span: primary.resolve(db),
                        },
                        SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: format!("consider implementing `{trait_name}` for `{ty}`"),
                            span: primary.resolve(db),
                        },
                    ],
                    notes: vec![],
                    error_code,
                }
            }

            Self::NotCallable(primary, ty) => CompleteDiagnostic {
                severity: Severity::Error,
                message: format!("expected function, found `{ty}`"),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("call expression requires function; `{ty}` is not callable"),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::CallGenericArgNumMismatch {
                primary,
                def_span,
                given,
                expected,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "given generic argument number mismatch".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "expected {} generic arguments, but {} given",
                            expected, given
                        ),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "function defined here".to_string(),
                        span: def_span.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::CallArgNumMismatch {
                primary,
                def_span,
                given,
                expected,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "argument number mismatch".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("expected {} arguments, but {} given", expected, given),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "function defined here".to_string(),
                        span: def_span.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::CallArgLabelMismatch {
                primary,
                def_span,
                given,
                expected,
            } => {
                let mut sub_diagnostics = if let Some(given) = given {
                    vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "expected `{}` label, but `{}` given",
                            expected.data(db.as_hir_db()),
                            given.data(db.as_hir_db())
                        ),
                        span: primary.resolve(db),
                    }]
                } else {
                    vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("expected `{}` label", expected.data(db.as_hir_db())),
                        span: primary.resolve(db),
                    }]
                };

                sub_diagnostics.push(SubDiagnostic {
                    style: LabelStyle::Secondary,
                    message: "function defined here".to_string(),
                    span: def_span.resolve(db),
                });

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "argument label mismatch".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::AmbiguousInherentMethodCall {
                primary,
                method_name,
                cand_spans,
            } => {
                let method_name = method_name.data(db.as_hir_db());
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{}` is ambiguous", method_name),
                    span: primary.resolve(db),
                }];

                for span in cand_spans {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{method_name}` is defined here"),
                        span: span.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "ambiguous method call".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::AmbiguousTrait {
                primary,
                method_name,
                traits,
            } => {
                let method_name = method_name.data(db.as_hir_db());
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{method_name}` is ambiguous"),
                    span: primary.resolve(db),
                }];

                for trait_ in traits {
                    let trait_name = trait_.name(db.as_hir_db()).unwrap().data(db.as_hir_db());
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("candidate: `{trait_name}::{method_name}`"),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "multiple trait candidates found".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::AmbiguousTraitInst { primary, cands } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "multiple implementations are found".to_string(),
                    span: primary.resolve(db),
                }];

                for cand in cands {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("candidate: {cand}"),
                        span: primary.resolve(db), // xxx cand span??
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "ambiguous trait implementation".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::InvisibleAmbiguousTrait { primary, traits } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "consider importing one of the following traits into the scope to resolve the ambiguity".to_string(),
                    span: primary.resolve(db),
                }];

                for trait_ in traits {
                    if let Some(path) = trait_.scope().pretty_path(db.as_hir_db()) {
                        sub_diagnostics.push(SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: format!("`use {path}`"),
                            span: primary.resolve(db),
                        });
                    }
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "trait is not in the scope".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::MethodNotFound {
                primary,
                method_name,
                receiver,
            } => {
                let method_name = method_name.data(db.as_hir_db());
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!("`{}` is not found", method_name),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{}` is not found in `{}`", method_name, receiver),
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::NotValue { primary, given } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "value is expected".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "`{}` cannot be used as a value",
                        match given {
                            Either::Left(item) => item.kind_name(),
                            Either::Right(_) => "type",
                        }
                    ),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::TypeAnnotationNeeded {
                primary,
                ty,
                is_integral,
            } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "type annotation is needed".to_string(),
                    span: primary.resolve(db),
                }];

                let sub_diag_msg = match ty {
                    Some(ty) => format!("consider giving `: {ty}` here"),
                    None if *is_integral => {
                        "no default type is provided for an integer type. consider giving integer type"
                            .to_string()
                    }
                    None => "consider giving `: Type` here".to_string(),
                };

                sub_diagnostics.push(SubDiagnostic {
                    style: LabelStyle::Secondary,
                    message: sub_diag_msg,
                    span: primary.resolve(db),
                });

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "type annotation is needed".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
        }
    }
}

impl<'db> DiagnosticVoucher<'db> for TraitLowerDiag<'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code =
            GlobalErrorCode::new(DiagnosticPass::ImplTraitDefinition, self.local_code());
        match self {
            Self::ExternalTraitForExternalType(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "external trait cannot be implemented for external type".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "external trait cannot be implemented for external type".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConflictTraitImpl {
                primary,
                conflict_with,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "conflict trait implementation".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "conflict trait implementation".to_string(),
                        span: primary.lazy_span().ty().resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "conflict with this trait implementation".to_string(),
                        span: conflict_with.lazy_span().ty().resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::CyclicSuperTraits(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "cyclic super traits are not allowed".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "super traits cycle is detected here".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },
        }
    }
}

impl<'db> DiagnosticVoucher<'db> for TraitConstraintDiag<'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::TraitSatisfaction, self.local_code());
        let severity = Severity::Error;
        match self {
            Self::KindMismatch { primary, trait_def } => CompleteDiagnostic {
                severity,
                message: "type doesn't satisfy required kind bound".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "type doesn't satisfy required kind bound here".to_string(),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "trait is defined here".to_string(),
                        span: trait_def.lazy_span().name().resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::TraitArgNumMismatch {
                span,
                expected,
                given,
            } => CompleteDiagnostic {
                severity,
                message: "given trait argument number mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected {} arguments here, but {} given", expected, given),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::TraitArgKindMismatch(span, msg) => CompleteDiagnostic {
                severity,
                message: "given trait argument kind mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: msg.to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::TraitBoundNotSat(span, msg, subgoal) => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: msg.to_string(),
                    span: span.resolve(db),
                }];

                if let Some(subgoal) = subgoal {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: subgoal.to_string(),
                        span: span.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity,
                    message: "trait bound is not satisfied".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::InfiniteBoundRecursion(span, msg) => CompleteDiagnostic {
                severity,
                message: "infinite trait bound recursion".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: msg.to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConcreteTypeBound(span, msg) => CompleteDiagnostic {
                severity,
                message: "trait bound for concrete type is not allowed".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: msg.to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConstTyBound(span, msg) => CompleteDiagnostic {
                severity,
                message: "trait bound for const type is not allowed".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: msg.to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },
        }
    }
}

impl<'db> DiagnosticVoucher<'db> for ImplDiag<'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::TraitSatisfaction, self.local_code());
        let severity = Severity::Error;

        match self {
            Self::ConflictMethodImpl {
                primary,
                conflict_with,
            } => CompleteDiagnostic {
                severity,
                message: "conflicting method implementations".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "".into(),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "".into(),
                        span: conflict_with.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::MethodNotDefinedInTrait {
                primary,
                trait_,
                method_name,
            } => CompleteDiagnostic {
                severity,
                message: "method not defined in trait".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "method `{}` is not defined in trait `{}`",
                        method_name.data(db.as_hir_db()),
                        trait_.name(db.as_hir_db()).unwrap().data(db.as_hir_db())
                    ),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::NotAllTraitItemsImplemented {
                primary,
                not_implemented,
            } => {
                let missing = not_implemented
                    .iter()
                    .map(|id| id.data(db.as_hir_db()).as_str())
                    .collect::<Vec<_>>()
                    .join(", ");

                CompleteDiagnostic {
                    severity,
                    message: "not all trait methods are implemented".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("missing implementations: {}", missing),
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::MethodTypeParamNumMismatch {
                primary,
                expected,
                given,
            } => CompleteDiagnostic {
                severity,
                message: "method type parameter count mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected {} type parameters, but {} given", expected, given),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::MethodTypeParamKindMismatch { primary, message } => CompleteDiagnostic {
                severity,
                message: "method type parameter kind mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: message.clone(),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::MethodArgNumMismatch {
                primary,
                expected,
                given,
            } => CompleteDiagnostic {
                severity,
                message: "method argument count mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected {} arguments, but {} given", expected, given),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::MethodArgLabelMismatch {
                primary,
                definition,
                message,
            } => CompleteDiagnostic {
                severity,
                message: "method argument label mismatch".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: message.clone(),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "argument label defined here".to_string(),
                        span: definition.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::MethodArgTyMismatch { primary, message } => CompleteDiagnostic {
                severity,
                message: "method argument type mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: message.clone(),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::MethodRetTyMismatch { primary, message } => CompleteDiagnostic {
                severity,
                message: "method return type mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: message.clone(),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::MethodStricterBound { primary, message } => CompleteDiagnostic {
                severity,
                message: "method has stricter bounds than trait".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: message.clone(),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::InvalidSelfType { primary, message } => CompleteDiagnostic {
                severity,
                message: "invalid type for `self` parameter".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: message.clone(),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

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

                CompleteDiagnostic {
                    severity,
                    message: "invalid inherent implementation".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: msg,
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }
        }
    }
}
