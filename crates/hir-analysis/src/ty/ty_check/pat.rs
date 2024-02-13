use std::ops::Range;

use hir::{
    hir_def::{scope_graph::ScopeId, Enum, IdentId, ItemKind, Partial, Pat, PatId, PathId},
    span::path::LazyPathSpan,
};

use super::{env::TyCheckEnv, ResolvedPathData, TyChecker};
use crate::{
    name_resolution::{
        resolve_path_early, resolve_segments_early, EarlyResolvedPath, NameDomain, NameRes,
        NameResBucket, NameResKind,
    },
    ty::{
        diagnostics::{BodyDiag, FuncBodyDiag, FuncBodyDiagAccumulator, TyLowerDiag},
        ty_def::{AdtRefId, InvalidCause, Kind, TyId, TyVarUniverse},
        ty_lower::lower_adt,
    },
    HirAnalysisDb,
};

impl<'db> TyChecker<'db> {
    pub(super) fn check_pat(&mut self, pat: PatId, expected: TyId) -> TyId {
        let Partial::Present(pat_data) = pat.data(self.db.as_hir_db(), self.body()) else {
            let actual = TyId::invalid(self.db, InvalidCause::Other);
            return self.unify_ty(pat, actual, expected);
        };

        let ty = match pat_data {
            Pat::WildCard => {
                let ty_var = self.table.new_var(TyVarUniverse::General, &Kind::Star);
                self.unify_ty(pat, ty_var, expected)
            }

            Pat::Rest => unreachable!(),
            Pat::Lit(..) => self.check_lit_pat(pat, pat_data),
            Pat::Tuple(..) => self.check_tuple_pat(pat, pat_data, expected),
            Pat::Path(..) => self.check_path_pat(pat, pat_data),
            Pat::PathTuple(..) => self.check_path_tuple_pat(pat, pat_data),
            Pat::Record(..) => todo!(),

            Pat::Or(lhs, rhs) => {
                self.check_pat(*lhs, expected);
                self.check_pat(*rhs, expected)
            }
        };

        self.unify_ty(pat, ty, expected)
    }

    fn check_lit_pat(&mut self, _pat: PatId, pat_data: &Pat) -> TyId {
        let Pat::Lit(lit) = pat_data else {
            unreachable!()
        };

        match lit {
            Partial::Present(lit) => self.lit_ty(lit),
            Partial::Absent => TyId::invalid(self.db, InvalidCause::Other),
        }
    }

    fn check_tuple_pat(&mut self, pat: PatId, pat_data: &Pat, expected: TyId) -> TyId {
        let Pat::Tuple(pat_tup) = pat_data else {
            unreachable!()
        };

        let expected_len = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_tuple(self.db) => Some(args.len()),
            _ => None,
        };
        let (actual, rest_range) = self.unpack_rest_pat(pat_tup, expected_len);
        let actual = TyId::tuple_with_elems(self.db, &actual);

        let unified = self.unify_ty(pat, actual, expected);
        if unified.contains_invalid(self.db) {
            pat_tup.iter().for_each(|&pat| {
                self.env
                    .type_pat(pat, TyId::invalid(self.db, InvalidCause::Other));
            });
            return unified;
        }

        let mut pat_idx = 0;
        for (i, &pat_ty) in unified.decompose_ty_app(self.db).1.iter().enumerate() {
            if pat_idx >= pat_tup.len() {
                break;
            };

            if pat_tup[pat_idx].is_rest(self.db.as_hir_db(), self.body()) {
                pat_idx += 1;
                continue;
            }

            if rest_range.contains(&i) {
                continue;
            }

            self.check_pat(pat_tup[pat_idx], pat_ty);
            pat_idx += 1;
        }

        unified
    }

    fn check_path_pat(&mut self, pat: PatId, pat_data: &Pat) -> TyId {
        let Pat::Path(path) = pat_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match resolve_path_in_pat(self.db, &self.env, *path, pat) {
            ResolvedPathInPat::Data(data) => {
                if data.is_unit_variant(self.db) {
                    data.ty(self.db, &mut self.table)
                } else {
                    let diag = BodyDiag::unit_variant_expected_in_pat(
                        self.db,
                        pat.lazy_span(self.body()).into(),
                        data,
                    );

                    FuncBodyDiagAccumulator::push(self.db, diag.into());
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }

            ResolvedPathInPat::NewBinding(binding) => {
                self.env.register_pending_binding(binding, pat);
                self.fresh_ty()
            }

            ResolvedPathInPat::Diag(diag) => {
                FuncBodyDiagAccumulator::push(self.db, diag);
                TyId::invalid(self.db, InvalidCause::Other)
            }

            ResolvedPathInPat::Invalid => TyId::invalid(self.db, InvalidCause::Other),
        }
    }

    fn check_path_tuple_pat(&mut self, pat: PatId, pat_data: &Pat) -> TyId {
        let Pat::PathTuple(path, args) = pat_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        let (pat_ty, expected_fields) = match resolve_path_in_pat(self.db, &self.env, *path, pat) {
            ResolvedPathInPat::Data(data) => {
                if data.is_tuple_variant(self.db) {
                    let ty = data.ty(self.db, &mut self.table);
                    let field_tys = data.field_tys(self.db);

                    (ty, Some(field_tys))
                } else {
                    let diag = BodyDiag::tuple_variant_expected_in_pat(
                        self.db,
                        pat.lazy_span(self.body()).into(),
                        Some(data),
                    );
                    FuncBodyDiagAccumulator::push(self.db, diag.into());

                    (TyId::invalid(self.db, InvalidCause::Other), None)
                }
            }

            ResolvedPathInPat::NewBinding(_) => {
                let diag = BodyDiag::tuple_variant_expected_in_pat(
                    self.db,
                    pat.lazy_span(self.body()).into(),
                    None,
                );
                FuncBodyDiagAccumulator::push(self.db, diag.into());

                (TyId::invalid(self.db, InvalidCause::Other), None)
            }

            ResolvedPathInPat::Diag(diag) => {
                FuncBodyDiagAccumulator::push(self.db, diag);

                (TyId::invalid(self.db, InvalidCause::Other), None)
            }

            ResolvedPathInPat::Invalid => (TyId::invalid(self.db, InvalidCause::Other), None),
        };

        let Some(expected_fields) = expected_fields else {
            args.iter().for_each(|&pat| {
                self.env
                    .type_pat(pat, TyId::invalid(self.db, InvalidCause::Other));
            });

            return pat_ty;
        };

        let (actual_args, rest_range) = self.unpack_rest_pat(args, Some(expected_fields.len()));
        if actual_args.len() != expected_fields.len() {
            let diag = BodyDiag::MismatchedFieldCount {
                primary: pat.lazy_span(self.body()).into(),
                expected: expected_fields.len(),
                given: actual_args.len(),
            };

            FuncBodyDiagAccumulator::push(self.db, diag.into());
            return pat_ty;
        };

        let mut arg_idx = 0;
        for (i, &arg_ty) in expected_fields.iter().enumerate() {
            if arg_idx >= args.len() {
                break;
            }

            if args[arg_idx].is_rest(self.db.as_hir_db(), self.body()) {
                arg_idx += 1;
                continue;
            }

            if rest_range.contains(&i) {
                continue;
            }

            self.check_pat(args[arg_idx], arg_ty);
            arg_idx += 1;
        }

        pat_ty
    }

    fn unpack_rest_pat(
        &mut self,
        pat_tup: &[PatId],
        expected_len: Option<usize>,
    ) -> (Vec<TyId>, std::ops::Range<usize>) {
        let mut rest_start = None;
        for (i, &pat) in pat_tup.iter().enumerate() {
            if pat.is_rest(self.db.as_hir_db(), self.body()) && rest_start.replace(i).is_some() {
                let span = pat.lazy_span(self.body());
                FuncBodyDiagAccumulator::push(
                    self.db,
                    BodyDiag::DuplicatedRestPat(span.into()).into(),
                );
                return (
                    self.fresh_tys_n(expected_len.unwrap_or(0)),
                    Range::default(),
                );
            }
        }

        match rest_start {
            Some(rest_start) => {
                let expected_len = expected_len.unwrap_or(0);
                let minimum_len = pat_tup.len() - 1;

                if minimum_len <= expected_len {
                    let diff = expected_len - minimum_len;
                    let range = rest_start..rest_start + diff;
                    (self.fresh_tys_n(expected_len), range)
                } else {
                    (self.fresh_tys_n(minimum_len), Range::default())
                }
            }

            None => (self.fresh_tys_n(pat_tup.len()), Range::default()),
        }
    }
}

fn resolve_path_in_pat(
    db: &dyn HirAnalysisDb,
    env: &TyCheckEnv,
    path: PathId,
    pat: PatId,
) -> ResolvedPathInPat {
    PathResolver { db, env, pat, path }.resolve_path()
}

struct PathResolver<'db, 'env> {
    db: &'db dyn HirAnalysisDb,
    env: &'env TyCheckEnv<'db>,
    pat: PatId,
    path: PathId,
}

impl<'db, 'env> PathResolver<'db, 'env> {
    fn resolve_path(&self) -> ResolvedPathInPat {
        let early_resolved_path = resolve_path_early(self.db, self.path, self.env.scope());
        let resolved = self.resolve_early_resolved_path(early_resolved_path);

        if !self.path.is_ident(self.db.as_hir_db()) {
            resolved
        } else {
            match resolved {
                ResolvedPathInPat::Diag(_) | ResolvedPathInPat::Invalid => {
                    if let Some(ident) = self.path.last_segment(self.db.as_hir_db()).to_opt() {
                        ResolvedPathInPat::NewBinding(ident)
                    } else {
                        resolved
                    }
                }

                _ => resolved,
            }
        }
    }

    fn resolve_early_resolved_path(&self, early: EarlyResolvedPath) -> ResolvedPathInPat {
        let hir_db = self.db.as_hir_db();

        // Try to resolve the partially resolved path as an enum variant.
        let early = match early {
            EarlyResolvedPath::Partial {
                res,
                unresolved_from,
            } if res.is_enum() && unresolved_from + 1 == self.path.len(hir_db) => {
                let segments = &self.path.segments(hir_db)[unresolved_from..];
                let scope = res.scope().unwrap();
                resolve_segments_early(self.db, segments, scope)
            }

            _ => early,
        };

        match early {
            EarlyResolvedPath::Full(bucket) => self.resolve_bucket(bucket),

            EarlyResolvedPath::Partial { .. } => {
                let span = self.path_span().into();
                let diag = TyLowerDiag::AssocTy(span);
                ResolvedPathInPat::Diag(FuncBodyDiag::Ty(diag.into()))
            }
        }
    }

    fn resolve_bucket(&self, bucket: NameResBucket) -> ResolvedPathInPat {
        if let Ok(res) = bucket.pick(NameDomain::Value) {
            let res = self.resolve_name_res(res);
            if !matches!(res, ResolvedPathInPat::Diag(_) | ResolvedPathInPat::Invalid) {
                return res;
            }
        }

        match bucket.pick(NameDomain::Type) {
            Ok(res) => self.resolve_name_res(res),
            Err(_) => ResolvedPathInPat::Invalid,
        }
    }

    fn resolve_name_res(&self, res: &NameRes) -> ResolvedPathInPat {
        match res.kind {
            NameResKind::Scope(ScopeId::Item(ItemKind::Struct(struct_))) => {
                let adt = lower_adt(self.db, AdtRefId::from_struct(self.db, struct_));
                ResolvedPathInPat::Data(ResolvedPathData::Adt(adt, self.path))
            }

            NameResKind::Scope(ScopeId::Variant(parent, idx)) => {
                let enum_: Enum = parent.try_into().unwrap();
                let data = ResolvedPathData::Variant(enum_, idx, self.path);
                ResolvedPathInPat::Data(data)
            }

            _ => {
                let diag = BodyDiag::InvalidPathDomainInPat {
                    primary: self.path_span().into(),
                    resolved: res
                        .scope()
                        .and_then(|scope| scope.name_span(self.db.as_hir_db())),
                };

                ResolvedPathInPat::Diag(diag.into())
            }
        }
    }

    fn path_span(&self) -> LazyPathSpan {
        let Partial::Present(pat_data) = self.pat.data(self.db.as_hir_db(), self.env.body()) else {
            unreachable!()
        };

        let pat_span = self.pat.lazy_span(self.env.body());

        match pat_data {
            Pat::Path(_) => pat_span.into_path_pat().path(),
            Pat::PathTuple(..) => pat_span.into_path_tuple_pat().path(),
            Pat::Record(..) => pat_span.into_record_pat().path(),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum ResolvedPathInPat {
    Data(ResolvedPathData),
    NewBinding(IdentId),
    Diag(FuncBodyDiag),
    Invalid,
}
