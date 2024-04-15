use std::ops::Range;

use hir::hir_def::{Partial, Pat, PatId, VariantKind};

use super::{
    env::LocalBinding,
    path::{resolve_path_in_pat, RecordInitChecker, ResolvedPathInPat},
    RecordLike, TyChecker,
};
use crate::ty::{
    binder::Binder,
    diagnostics::{BodyDiag, FuncBodyDiagAccumulator},
    ty_def::{InvalidCause, Kind, TyId, TyVarSort},
    ty_lower::lower_hir_ty,
};

impl<'db> TyChecker<'db> {
    pub(super) fn check_pat(&mut self, pat: PatId, expected: TyId) -> TyId {
        let Partial::Present(pat_data) = pat.data(self.db.as_hir_db(), self.body()) else {
            let actual = TyId::invalid(self.db, InvalidCause::Other);
            return self.unify_ty(pat, actual, expected);
        };

        let ty = match pat_data {
            Pat::WildCard => {
                let ty_var = self.table.new_var(TyVarSort::General, &Kind::Star);
                self.unify_ty(pat, ty_var, expected)
            }

            Pat::Rest => unreachable!(),
            Pat::Lit(..) => self.check_lit_pat(pat, pat_data),
            Pat::Tuple(..) => self.check_tuple_pat(pat, pat_data, expected),
            Pat::Path(..) => self.check_path_pat(pat, pat_data),
            Pat::PathTuple(..) => self.check_path_tuple_pat(pat, pat_data),
            Pat::Record(..) => self.check_record_pat(pat, pat_data),

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
        let Pat::Path(path, is_mut) = pat_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match resolve_path_in_pat(self, *path, pat) {
            ResolvedPathInPat::Ty(ty) => {
                let diag =
                    BodyDiag::unit_variant_expected(self.db, pat.lazy_span(self.body()).into(), ty);

                FuncBodyDiagAccumulator::push(self.db, diag.into());
                TyId::invalid(self.db, InvalidCause::Other)
            }

            ResolvedPathInPat::Variant(variant) => {
                if matches!(variant.variant_kind(self.db), VariantKind::Unit) {
                    variant.ty(self.db)
                } else {
                    let diag = BodyDiag::unit_variant_expected(
                        self.db,
                        pat.lazy_span(self.body()).into(),
                        variant,
                    );

                    FuncBodyDiagAccumulator::push(self.db, diag.into());
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }

            ResolvedPathInPat::NewBinding(ident) => {
                let binding = LocalBinding::local(pat, *is_mut);
                self.env.register_pending_binding(ident, binding);
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
        let Pat::PathTuple(Partial::Present(path), elems) = pat_data else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        let (variant, expected_elems) = match resolve_path_in_pat(self, *path, pat) {
            ResolvedPathInPat::Ty(ty) => {
                let diag = BodyDiag::tuple_variant_expected(
                    self.db,
                    pat.lazy_span(self.body()).into(),
                    Some(ty),
                );
                FuncBodyDiagAccumulator::push(self.db, diag.into());
                return TyId::invalid(self.db, InvalidCause::Other);
            }

            ResolvedPathInPat::Variant(variant) => match variant.variant_kind(self.db) {
                VariantKind::Tuple(elems) => (variant, elems),
                _ => {
                    let diag = BodyDiag::tuple_variant_expected(
                        self.db,
                        pat.lazy_span(self.body()).into(),
                        Some(variant),
                    );
                    FuncBodyDiagAccumulator::push(self.db, diag.into());
                    return TyId::invalid(self.db, InvalidCause::Other);
                }
            },

            ResolvedPathInPat::NewBinding(_) => {
                let diag = BodyDiag::tuple_variant_expected::<TyId>(
                    self.db,
                    pat.lazy_span(self.body()).into(),
                    None,
                );
                FuncBodyDiagAccumulator::push(self.db, diag.into());

                return TyId::invalid(self.db, InvalidCause::Other);
            }

            ResolvedPathInPat::Diag(diag) => {
                FuncBodyDiagAccumulator::push(self.db, diag);

                return TyId::invalid(self.db, InvalidCause::Other);
            }

            ResolvedPathInPat::Invalid => {
                return TyId::invalid(self.db, InvalidCause::Other);
            }
        };

        let pat_ty = variant.ty(self.db);
        let expected_len = expected_elems.len(self.db.as_hir_db());

        let (actual_elems, rest_range) = self.unpack_rest_pat(elems, Some(expected_len));
        if actual_elems.len() != expected_len {
            let diag = BodyDiag::MismatchedFieldCount {
                primary: pat.lazy_span(self.body()).into(),
                expected: expected_len,
                given: actual_elems.len(),
            };

            FuncBodyDiagAccumulator::push(self.db, diag.into());
            return pat_ty;
        };

        let mut arg_idx = 0;
        for (i, &hir_ty) in expected_elems.data(self.db.as_hir_db()).iter().enumerate() {
            if arg_idx >= elems.len() {
                break;
            }

            if elems[arg_idx].is_rest(self.db.as_hir_db(), self.body()) {
                arg_idx += 1;
                continue;
            }

            if rest_range.contains(&i) {
                continue;
            }
            let elem_ty = match hir_ty.to_opt() {
                Some(ty) => {
                    let ty = lower_hir_ty(self.db, ty, self.env.scope());
                    Binder::bind(ty).instantiate(self.db, variant.args())
                }
                _ => TyId::invalid(self.db, InvalidCause::Other),
            };

            self.check_pat(elems[arg_idx], elem_ty);
            arg_idx += 1;
        }

        pat_ty
    }

    fn check_record_pat(&mut self, pat: PatId, pat_data: &Pat) -> TyId {
        let Pat::Record(Partial::Present(path), _) = pat_data else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match resolve_path_in_pat(self, *path, pat) {
            ResolvedPathInPat::Ty(ty) if ty.is_record(self.db) => {
                self.check_record_pat_fields(ty, pat);
                ty
            }

            ResolvedPathInPat::Ty(ty) => {
                let diag =
                    BodyDiag::record_expected(self.db, pat.lazy_span(self.body()).into(), Some(ty));
                FuncBodyDiagAccumulator::push(self.db, diag.into());

                TyId::invalid(self.db, InvalidCause::Other)
            }

            ResolvedPathInPat::Variant(variant) if variant.is_record(self.db) => {
                let ty = variant.ty(self.db);
                self.check_record_pat_fields(variant, pat);
                ty
            }

            ResolvedPathInPat::Variant(variant) => {
                let diag = BodyDiag::record_expected(
                    self.db,
                    pat.lazy_span(self.body()).into(),
                    Some(variant),
                );
                FuncBodyDiagAccumulator::push(self.db, diag.into());

                TyId::invalid(self.db, InvalidCause::Other)
            }

            ResolvedPathInPat::NewBinding(_) => {
                let diag = BodyDiag::record_expected::<TyId>(
                    self.db,
                    pat.lazy_span(self.body()).into(),
                    None,
                );
                FuncBodyDiagAccumulator::push(self.db, diag.into());

                TyId::invalid(self.db, InvalidCause::Other)
            }

            ResolvedPathInPat::Diag(diag) => {
                FuncBodyDiagAccumulator::push(self.db, diag);

                TyId::invalid(self.db, InvalidCause::Other)
            }

            ResolvedPathInPat::Invalid => TyId::invalid(self.db, InvalidCause::Other),
        }
    }

    fn check_record_pat_fields<T>(&mut self, mut record_like: T, pat: PatId)
    where
        T: RecordLike,
    {
        let Partial::Present(Pat::Record(_, fields)) = pat.data(self.db.as_hir_db(), self.body())
        else {
            unreachable!()
        };

        let hir_db = self.db.as_hir_db();
        let mut contains_rest = false;

        let pat_span = pat.lazy_span(self.body()).into_record_pat();
        let mut rec_checker = RecordInitChecker::new(self, &mut record_like);

        for (i, field_pat) in fields.iter().enumerate() {
            let field_pat_span = pat_span.fields().field(i);

            if field_pat.pat.is_rest(hir_db, rec_checker.tc.body()) {
                if contains_rest {
                    let diag = BodyDiag::DuplicatedRestPat(
                        field_pat.pat.lazy_span(rec_checker.tc.body()).into(),
                    );
                    FuncBodyDiagAccumulator::push(rec_checker.tc.db, diag.into());
                    continue;
                }

                contains_rest = true;
                continue;
            }

            let label = field_pat.label(hir_db, rec_checker.tc.body());
            let expected = match rec_checker.feed_label(label, field_pat_span.into()) {
                Ok(ty) => ty,
                Err(diag) => {
                    FuncBodyDiagAccumulator::push(rec_checker.tc.db, diag);
                    TyId::invalid(rec_checker.tc.db, InvalidCause::Other)
                }
            };

            rec_checker.tc.check_pat(field_pat.pat, expected);
        }

        if let Err(diag) = rec_checker.finalize(pat_span.fields().into(), contains_rest) {
            FuncBodyDiagAccumulator::push(self.db, diag);
        }
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
