use std::{
    collections::{hash_map::Entry, BTreeSet},
    ops::Range,
};

use hir::hir_def::{IdentId, Partial, Pat, PatId};
use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    path::{resolve_path_in_pat, ResolvedPathInPat},
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

        let mut path_data = match resolve_path_in_pat(self, *path, pat) {
            ResolvedPathInPat::Data(data) => {
                if data.is_record(self.db) {
                    data
                } else {
                    let diag = BodyDiag::record_variant_expected(
                        self.db,
                        pat.lazy_span(self.body()).into(),
                        Some(data),
                    );
                    FuncBodyDiagAccumulator::push(self.db, diag.into());

                    return TyId::invalid(self.db, InvalidCause::Other);
                }
            }

            ResolvedPathInPat::NewBinding(_) => {
                let diag = BodyDiag::record_variant_expected(
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

            ResolvedPathInPat::Invalid => return TyId::invalid(self.db, InvalidCause::Other),
        };

        let hir_db = self.db.as_hir_db();
        let mut seen_fields = FxHashMap::default();
        let mut contains_rest = false;
        let mut contains_invalid_field = false;

        let pat_span = pat.lazy_span(self.body()).into_record_pat();

        for (i, field_pat) in fields.iter().enumerate() {
            let field_pat_span = pat_span.fields().field(i);

            if field_pat.pat.is_rest(hir_db, self.body()) {
                if contains_rest {
                    let diag =
                        BodyDiag::DuplicatedRestPat(field_pat.pat.lazy_span(self.body()).into());
                    FuncBodyDiagAccumulator::push(self.db, diag.into());
                    continue;
                }

                contains_rest = true;
                continue;
            }

            let expected_ty = match field_pat.label(hir_db, self.body()) {
                Some(label) => match seen_fields.entry(label) {
                    Entry::Occupied(i) => {
                        let first_use = pat_span.fields().field(*i.get()).name();
                        let diag = BodyDiag::DuplicatedRecordFieldBind {
                            primary: pat_span.clone().into(),
                            first_use: first_use.into(),
                            name: label,
                        };
                        FuncBodyDiagAccumulator::push(self.db, diag.into());
                        contains_invalid_field = true;

                        TyId::invalid(self.db, InvalidCause::Other)
                    }

                    Entry::Vacant(entry) => {
                        entry.insert(i);
                        if let Some(ty) = path_data.record_field_ty(self.db, label) {
                            ty
                        } else {
                            let diag = BodyDiag::record_field_not_found(
                                self.db,
                                field_pat_span.into(),
                                &path_data,
                                label,
                            );
                            FuncBodyDiagAccumulator::push(self.db, diag.into());
                            contains_invalid_field = true;

                            TyId::invalid(self.db, InvalidCause::Other)
                        }
                    }
                },

                None => {
                    let diag = BodyDiag::ExplicitLabelExpectedInRecord {
                        primary: field_pat_span.into(),
                        hint: path_data.initializer_hint(self.db),
                    };
                    FuncBodyDiagAccumulator::push(self.db, diag.into());
                    contains_invalid_field = true;

                    TyId::invalid(self.db, InvalidCause::Other)
                }
            };

            self.check_pat(field_pat.pat, expected_ty);
        }

        // Check for missing fields if no rest pat in the record.
        if !contains_rest && !contains_invalid_field {
            let expected_labels = path_data.record_labels(self.db);
            let found = seen_fields.keys().copied().collect::<FxHashSet<_>>();
            let missing_fields: BTreeSet<IdentId> =
                expected_labels.difference(&found).copied().collect();

            if !missing_fields.is_empty() {
                let diag = BodyDiag::MissingRecordFields {
                    primary: pat_span.into(),
                    missing_fields,
                    hint: path_data.initializer_hint(self.db),
                };

                FuncBodyDiagAccumulator::push(self.db, diag.into());
            }
        }

        path_data.ty(self.db)
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
