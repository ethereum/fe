use hir::hir_def::{IntegerId, PatId, StringId};

use super::{ty_check::TypedBody, ty_def::TyId};
use crate::HirAnalysisDb;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimplifiedPat<'db> {
    ctor: PatCtor<'db>,
    args: Vec<Self>,
    hir_pat: PatId,
    ty: TyId<'db>,
}

impl<'db> SimplifiedPat<'db> {
    pub fn new(
        ctor: PatCtor<'db>,
        args: Vec<SimplifiedPat<'db>>,
        hir_pat: PatId,
        ty: TyId<'db>,
    ) -> Self {
        Self {
            ctor,
            args,
            hir_pat,
            ty,
        }
    }

    pub fn simplify(db: &'db dyn HirAnalysisDb, pat: PatId, body: TypedBody) -> Self {
        let ty = body.pat_ty(db, pat);
        assert!(!ty.has_invalid(db));

        todo!()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatCtor<'db> {
    Or,
    WildCard,
    Tuple,
    Struct,
    Variant(u32),
    // FIXME: Extend this to `IntRange` when we add range pattern.
    Int(IntegerId<'db>),
    String(StringId<'db>),
    Bool(bool),
}
