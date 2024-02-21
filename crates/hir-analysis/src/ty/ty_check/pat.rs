use std::ops::Range;

use hir::hir_def::{Partial, Pat, PatId};

use super::{
    path::{resolve_path_in_pat, RecordInitChecker, ResolvedPathInPat},
    TyChecker,
};
use crate::ty::{
    diagnostics::{BodyDiag, FuncBodyDiagAccumulator},
    ty_def::{InvalidCause, Kind, TyId, TyVarUniverse},
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
        let Pat::Path(path, _) = pat_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        match resolve_path_in_pat(self, *path, pat) {
            ResolvedPathInPat::Data(data) => {
                if data.is_unit_variant(self.db) {
                    data.ty(self.db)
                } else {
                    let diag = BodyDiag::unit_variant_expected(
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
        let Pat::PathTuple(Partial::Present(path), args) = pat_data else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        let mut path_data = match resolve_path_in_pat(self, *path, pat) {
            ResolvedPathInPat::Data(data) => {
                if data.is_tuple_variant(self.db) {
                    data
                } else {
                    let diag = BodyDiag::tuple_variant_expected(
                        self.db,
                        pat.lazy_span(self.body()).into(),
                        Some(data),
                    );
                    FuncBodyDiagAccumulator::push(self.db, diag.into());
                    return TyId::invalid(self.db, InvalidCause::Other);
                }
            }

            ResolvedPathInPat::NewBinding(_) => {
                let diag = BodyDiag::tuple_variant_expected(
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

        let pat_ty = path_data.ty(self.db);
        let expected_fields = path_data.field_tys(self.db);

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

    fn check_record_pat(&mut self, pat: PatId, pat_data: &Pat) -> TyId {
        let Pat::Record(Partial::Present(path), fields) = pat_data else {
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        let mut data = match resolve_path_in_pat(self, *path, pat) {
            ResolvedPathInPat::Data(data) => {
                if data.is_record(self.db) {
                    data
                } else {
                    let diag = BodyDiag::record_expected(
                        self.db,
                        pat.lazy_span(self.body()).into(),
                        Some(data),
                    );
                    FuncBodyDiagAccumulator::push(self.db, diag.into());

                    return TyId::invalid(self.db, InvalidCause::Other);
                }
            }

            ResolvedPathInPat::NewBinding(_) => {
                let diag =
                    BodyDiag::record_expected(self.db, pat.lazy_span(self.body()).into(), None);
                FuncBodyDiagAccumulator::push(self.db, diag.into());

                return TyId::invalid(self.db, InvalidCause::Other);
            }

            ResolvedPathInPat::Diag(diag) => {
                FuncBodyDiagAccumulator::push(self.db, diag);

                return TyId::invalid(self.db, InvalidCause::Other);
            }

            ResolvedPathInPat::Invalid => return TyId::invalid(self.db, InvalidCause::Other),
        };

        let hir_db = self.db.as_hir_db();
        let mut contains_rest = false;

        let pat_span = pat.lazy_span(self.body()).into_record_pat();
        let mut rec_checker = RecordInitChecker::new(self, &mut data);

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

        data.ty(self.db)
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
