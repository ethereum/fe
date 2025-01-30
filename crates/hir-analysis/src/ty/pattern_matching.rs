use hir::hir_def::{Body, IntegerId, LitKind, Pat, PatId, RecordPatField, StringId};

use super::{ty_check::TypedBody, ty_def::TyId};
use crate::{
    name_resolution::{resolve_path, PathRes, PathResError, PathResErrorKind, ResolvedVariant},
    ty::ty_check::RecordLike,
    HirAnalysisDb,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimplifiedPat<'db> {
    ctor: PatCtor<'db>,
    args: Vec<Self>,
    hir_pat: Option<PatId>,
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
            hir_pat: Some(hir_pat),
            ty,
        }
    }

    pub fn wildcard(ty: TyId<'db>) -> Self {
        Self {
            ctor: PatCtor::WildCard,
            args: vec![],
            hir_pat: None,
            ty,
        }
    }

    pub fn simplify(db: &'db dyn HirAnalysisDb, pat: PatId, typed_body: &'db TypedBody) -> Self {
        let ty = typed_body.pat_ty(db, pat);
        assert!(!ty.has_invalid(db));

        let pat_data = pat.data(db.as_hir_db(), typed_body.body.unwrap()).unwrap();

        match pat_data {
            Pat::WildCard => SimplifiedPat::new(PatCtor::WildCard, vec![], pat, ty),
            Pat::Rest => unreachable!(), // Not allowed as a root pattern
            Pat::Lit(partial) => match partial.unwrap() {
                LitKind::Int(integer_id) => {
                    SimplifiedPat::new(PatCtor::Int(*integer_id), vec![], pat, ty)
                }
                LitKind::String(string_id) => {
                    SimplifiedPat::new(PatCtor::String(*string_id), vec![], pat, ty)
                }
                LitKind::Bool(b) => SimplifiedPat::new(PatCtor::Bool(*b), vec![], pat, ty),
            },
            Pat::Tuple(vec) => simplify_tuple_pattern(db, pat, &vec, typed_body),
            Pat::Path(partial, _) => {
                let path = partial.unwrap();
                let scope = typed_body.body.unwrap().scope(); // xxx use tighter scope??
                match resolve_path(db, *path, scope, true) {
                    Ok(res) => {
                        if let PathRes::EnumVariant(var) = res {
                            // xxx use var.ty??
                            Self {
                                ctor: PatCtor::Variant(var.idx as u32), // xxx change idx to u32
                                args: vec![],                           // xxx
                                hir_pat: Some(pat),
                                ty,
                            }
                        } else {
                            todo!()
                        }
                    }
                    Err(err) => {
                        if path.is_bare_ident(db.as_hir_db()) {
                            Self {
                                ctor: PatCtor::WildCard,
                                args: vec![], // xxx
                                hir_pat: Some(pat),
                                ty,
                            }
                        } else {
                            todo!()
                        }
                    }
                }
            }
            Pat::PathTuple(_, vec) => simplify_tuple_pattern(db, pat, &vec, typed_body),

            Pat::Record(path, pat_fields) => {
                let path = path.unwrap();
                let body = typed_body.body.unwrap();
                let scope = body.scope();
                match resolve_path(db, *path, scope, true) {
                    Ok(res) => match res {
                        PathRes::Ty(ty_id) if ty_id.is_struct(db) => {
                            simplify_record_pattern(db, &ty_id.into(), pat, typed_body)
                        }
                        PathRes::EnumVariant(variant) => {
                            simplify_record_pattern(db, &variant.into(), pat, typed_body)
                        }
                        _ => todo!(),
                    },
                    Err(_) => {
                        todo!()
                    }
                }
            }
            Pat::Or(pat_id, pat_id1) => {
                let pat = Self::simplify(db, *pat_id, typed_body);
                let pat1 = Self::simplify(db, *pat_id1, typed_body);
                Self {
                    ctor: PatCtor::Or,
                    args: vec![pat, pat1],
                    hir_pat: Some(*pat_id),
                    ty,
                }
            }
        }
    }
}

fn simplify_record_pattern<'db>(
    db: &'db dyn HirAnalysisDb,
    record: &RecordLike<'db>,
    pat: PatId,
    typed_body: &'db TypedBody<'db>,
) -> SimplifiedPat<'db> {
    let pat_data = pat.data(db.as_hir_db(), typed_body.body.unwrap()).unwrap();
    let Pat::Record(_, fields) = pat_data else {
        unreachable!()
    };

    let body = typed_body.body.unwrap();
    let args = record
        .record_labels(db)
        .iter()
        .map(|l| {
            if let Some(pat_field) = fields
                .iter()
                .find(|pf| pf.label(db.as_hir_db(), body).unwrap().data(db) == l.data(db))
            {
                SimplifiedPat::simplify(db, pat_field.pat, typed_body)
            } else {
                let ty = record.record_field_ty(db, *l).unwrap();
                SimplifiedPat::wildcard(ty)
            }
        })
        .collect();

    SimplifiedPat {
        ctor: PatCtor::Struct,
        args,
        hir_pat: Some(pat),
        ty: typed_body.pat_ty(db, pat),
    }
}

fn simplify_tuple_pattern<'db>(
    db: &'db dyn HirAnalysisDb,
    pat: PatId,
    elem_pats: &[PatId],
    typed_body: &TypedBody<'db>,
) -> SimplifiedPat<'db> {
    let body = typed_body.body.unwrap();
    let ty = typed_body.pat_ty(db, pat);

    // should we make a `TupleLike` trait too?
    todo!()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatCtor<'db> {
    Or,
    WildCard, // xxx v1 had Option<(SmolStr, usize)>
    Tuple,
    Struct,
    Variant(u32),
    // FIXME: Extend this to `IntRange` when we add range pattern.
    Int(IntegerId<'db>),
    String(StringId<'db>),
    Bool(bool),
}
