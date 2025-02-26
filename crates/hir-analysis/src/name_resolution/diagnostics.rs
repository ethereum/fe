use hir::{
    hir_def::{IdentId, TopLevelMod},
    span::DynLazySpan,
};
use salsa::Update;

use super::NameRes;
use crate::HirAnalysisDb;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum NameResDiag<'db> {
    /// The definition conflicts with other definitions.
    Conflict(IdentId<'db>, Vec<DynLazySpan<'db>>),

    /// The name is not found.
    NotFound(DynLazySpan<'db>, IdentId<'db>),

    /// The resolved name is not visible.
    Invisible(DynLazySpan<'db>, IdentId<'db>, Option<DynLazySpan<'db>>),

    /// The resolved name is ambiguous.
    Ambiguous(DynLazySpan<'db>, IdentId<'db>, Vec<DynLazySpan<'db>>),

    /// The name is found, but it can't be used as a middle segment of a path.
    InvalidPathSegment(DynLazySpan<'db>, IdentId<'db>, Option<DynLazySpan<'db>>),

    TooManyGenericArgs {
        span: DynLazySpan<'db>,
        expected: usize,
        given: usize,
    },

    /// The name is found but belongs to a different name domain other than the
    /// Type.
    ExpectedType(DynLazySpan<'db>, IdentId<'db>, &'static str),

    /// The name is found but belongs to a different name domain other than the
    /// trait.
    ExpectedTrait(DynLazySpan<'db>, IdentId<'db>, &'static str),

    /// The name is found but belongs to a different name domain other than the
    /// value.
    ExpectedValue(DynLazySpan<'db>, IdentId<'db>, &'static str),
}

impl<'db> NameResDiag<'db> {
    /// Returns the top-level module where the diagnostic is located.
    pub fn top_mod(&self, db: &'db dyn HirAnalysisDb) -> TopLevelMod<'db> {
        match self {
            Self::Conflict(_, conflicts) => conflicts
                .iter()
                .filter_map(|span| span.top_mod(db.as_hir_db()))
                .min()
                .unwrap(),
            Self::NotFound(span, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::Invisible(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::Ambiguous(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::InvalidPathSegment(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::ExpectedType(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::ExpectedTrait(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::ExpectedValue(span, _, _) => span.top_mod(db.as_hir_db()).unwrap(),
            Self::TooManyGenericArgs { span, .. } => span.top_mod(db.as_hir_db()).unwrap(),
        }
    }

    pub(super) fn ambiguous(
        db: &'db dyn HirAnalysisDb,
        span: DynLazySpan<'db>,
        ident: IdentId<'db>,
        cands: Vec<NameRes<'db>>,
    ) -> Self {
        let cands = cands
            .into_iter()
            .filter_map(|name| name.kind.name_span(db))
            .collect();
        Self::Ambiguous(span, ident, cands)
    }

    pub fn local_code(&self) -> u16 {
        match self {
            Self::Conflict(..) => 1,
            Self::NotFound(..) => 2,
            Self::Invisible(..) => 3,
            Self::Ambiguous(..) => 4,
            Self::InvalidPathSegment(..) => 5,
            Self::ExpectedType(..) => 6,
            Self::ExpectedTrait(..) => 7,
            Self::ExpectedValue(..) => 8,
            Self::TooManyGenericArgs { .. } => 9,
        }
    }
}
