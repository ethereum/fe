use std::ops::Range;

use hir::{
    hir_def::{
        scope_graph::ScopeId, Enum, IdentId, ItemKind, Partial, Pat, PatId, PathId,
        VariantKind as HirVariantKind,
    },
    span::DynLazySpan,
};

use super::{env::TyCheckEnv, TyChecker};
use crate::{
    name_resolution::{
        resolve_path_early, EarlyResolvedPath, NameDomain, NameRes, NameResBucket, NameResKind,
    },
    ty::{
        diagnostics::{BodyDiag, FuncBodyDiag, FuncBodyDiagAccumulator, TyLowerDiag},
        ty_def::{AdtDef, AdtRef, AdtRefId, InvalidCause, Kind, TyId, TyVarUniverse},
        ty_lower::lower_adt,
        unify::UnificationTable,
    },
    HirAnalysisDb,
};

impl<'db> TyChecker<'db> {
    pub(super) fn check_pat(&mut self, pat: PatId, expected: TyId) -> TyId {
        let Partial::Present(pat_data) = pat.data(self.db.as_hir_db(), self.env.body()) else {
            let actual = TyId::invalid(self.db, InvalidCause::Other);
            return self.unify_ty(pat, actual, expected);
        };

        match pat_data {
            Pat::WildCard => {
                let ty_var = self.table.new_var(TyVarUniverse::General, &Kind::Star);
                self.unify_ty(pat, ty_var, expected)
            }

            Pat::Rest => unreachable!(),

            Pat::Lit(lit) => {
                let actual = match lit {
                    Partial::Present(lit) => self.lit_ty(lit),
                    Partial::Absent => TyId::invalid(self.db, InvalidCause::Other),
                };
                self.unify_ty(pat, actual, expected)
            }

            Pat::Tuple(pat_tup) => {
                let (actual, rest_range) = self.unpack_rest_pat(pat_tup, expected);
                let unified = self.unify_ty(pat, actual, expected);

                if unified.contains_invalid(self.db) {
                    pat_tup.iter().for_each(|&pat| {
                        self.env
                            .type_pat(pat, TyId::invalid(self.db, InvalidCause::Other));
                    });
                    return unified;
                }

                for (i, &pat_ty) in unified.decompose_ty_app(self.db).1.iter().enumerate() {
                    if rest_range.contains(&i)
                        || pat_tup[i].is_rest(self.db.as_hir_db(), self.env.body())
                    {
                        continue;
                    }

                    self.check_pat(pat_tup[i], pat_ty);
                }

                unified
            }

            Pat::Path(path) => {
                let Partial::Present(path) = path else {
                    return TyId::invalid(self.db, InvalidCause::Other);
                };

                match resolve_path_in_pat(self.db, &self.env, *path, pat) {
                    ResolvedPathInPat::Data(data) => {
                        if data.is_unit_variant(self.db) {
                            let ty = data.ty(self.db, &mut self.table);
                            self.unify_ty(pat, ty, expected)
                        } else {
                            let diag = BodyDiag::unit_variant_expected_in_pat(
                                self.db,
                                pat.lazy_span(self.env.body()).into(),
                                data,
                            );

                            FuncBodyDiagAccumulator::push(self.db, diag.into());
                            self.unify_ty(
                                pat,
                                TyId::invalid(self.db, InvalidCause::Other),
                                expected,
                            )
                        }
                    }

                    ResolvedPathInPat::NewBinding(binding) => {
                        self.env.register_pending_binding(binding, pat);
                        let actual = self.table.new_var(TyVarUniverse::General, &Kind::Star);
                        self.unify_ty(pat, actual, expected)
                    }

                    ResolvedPathInPat::Diag(diag) => {
                        FuncBodyDiagAccumulator::push(self.db, diag);
                        TyId::invalid(self.db, InvalidCause::Other)
                    }

                    ResolvedPathInPat::Invalid => TyId::invalid(self.db, InvalidCause::Other),
                }
            }

            Pat::PathTuple(..) => todo!(),

            Pat::Record(..) => todo!(),

            Pat::Or(lhs, rhs) => {
                self.check_pat(*lhs, expected);
                self.check_pat(*rhs, expected)
            }
        }
    }

    fn unpack_rest_pat(
        &mut self,
        pat_tup: &[PatId],
        expected: TyId,
    ) -> (TyId, std::ops::Range<usize>) {
        let mut rest_start = None;
        for (i, &pat) in pat_tup.iter().enumerate() {
            if pat.is_rest(self.db.as_hir_db(), self.env.body()) && rest_start.replace(i).is_some()
            {
                let span = pat.lazy_span(self.env.body());
                FuncBodyDiagAccumulator::push(
                    self.db,
                    BodyDiag::DuplicatedRestPat(span.into()).into(),
                );
                return (
                    TyId::invalid(self.db, InvalidCause::Other),
                    Range::default(),
                );
            }
        }

        let mut make_args = |len: usize| {
            (0..len)
                .map(|_| self.table.new_var(TyVarUniverse::General, &Kind::Star))
                .collect::<Vec<_>>()
        };

        match rest_start {
            Some(rest_start) => {
                let (base, expected_args) = expected.decompose_ty_app(self.db);
                let expected_args_len = expected_args.len();
                let minimum_len = expected_args.len() - 1;

                if base.is_tuple(self.db) && minimum_len <= expected_args.len() {
                    let diff = expected_args_len - minimum_len;
                    let range = rest_start..rest_start + diff;
                    let ty_args = make_args(expected_args_len);
                    (TyId::tuple_with_elems(self.db, &ty_args), range)
                } else {
                    let ty_args = make_args(minimum_len);
                    (TyId::tuple_with_elems(self.db, &ty_args), Range::default())
                }
            }

            None => {
                let ty_args = make_args(pat_tup.len());
                (TyId::tuple_with_elems(self.db, &ty_args), Range::default())
            }
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
        match early {
            EarlyResolvedPath::Full(bucket) => self.resolve_bucket(bucket),
            EarlyResolvedPath::Partial { .. } => {
                let diag = TyLowerDiag::AssocTy(self.pat.lazy_span(self.env.body()).into());
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
                ResolvedPathInPat::Data(ResolvedPatData::Adt(adt, self.path))
            }

            NameResKind::Scope(ScopeId::Variant(parent, idx)) => {
                let enum_: Enum = parent.try_into().unwrap();
                let data = ResolvedPatData::Variant(enum_, idx, self.path);
                ResolvedPathInPat::Data(data)
            }

            _ => {
                let diag = BodyDiag::InvalidPathDomainInPat {
                    primary: self.span(),
                    resolved: res
                        .scope()
                        .and_then(|scope| scope.name_span(self.db.as_hir_db())),
                };

                ResolvedPathInPat::Diag(diag.into())
            }
        }
    }

    fn span(&self) -> DynLazySpan {
        self.pat.lazy_span(self.env.body()).into()
    }
}

#[derive(Clone, Debug)]
pub(crate) enum ResolvedPathInPat {
    Data(ResolvedPatData),
    NewBinding(IdentId),
    Diag(FuncBodyDiag),
    Invalid,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) enum ResolvedPatData {
    Adt(AdtDef, PathId),
    Variant(Enum, usize, PathId),
}

impl ResolvedPatData {
    pub(crate) fn adt_ref(&self, db: &dyn HirAnalysisDb) -> AdtRef {
        match self {
            Self::Adt(adt, _) => adt.adt_ref(db).data(db),
            Self::Variant(enum_, _, _) => AdtRef::Enum(*enum_),
        }
    }

    pub(crate) fn data_kind(&self, db: &dyn HirAnalysisDb) -> &'static str {
        match self {
            Self::Adt(adt, _) => adt.adt_ref(db).kind_name(db),
            Self::Variant(enum_, idx, _) => {
                let hir_db = db.as_hir_db();
                match enum_.variants(hir_db).data(hir_db)[*idx].kind {
                    hir::hir_def::VariantKind::Unit => "unit variant",
                    HirVariantKind::Tuple(_) => "tuple variant",
                    HirVariantKind::Record(_) => "record variant",
                }
            }
        }
    }

    pub(crate) fn hint(&self, db: &dyn HirAnalysisDb) -> Option<String> {
        let hir_db = db.as_hir_db();

        let expected_sub_pat = match self {
            Self::Adt(_, _) => {
                let AdtRef::Struct(s) = self.adt_ref(db) else {
                    return None;
                };

                s.format_initializer_args(hir_db)
            }

            Self::Variant(enum_, idx, _) => {
                enum_.variants(hir_db).data(hir_db)[*idx].format_initializer_args(hir_db)
            }
        };

        let path = self.path().pretty_print(hir_db);
        Some(format!("{}{}", path, expected_sub_pat))
    }

    fn path(&self) -> PathId {
        match self {
            Self::Adt(_, path) => *path,
            Self::Variant(_, _, path) => *path,
        }
    }

    fn ty(&self, db: &dyn HirAnalysisDb, table: &mut UnificationTable) -> TyId {
        let adt = match self {
            Self::Adt(adt, _) => *adt,

            Self::Variant(enum_, ..) => lower_adt(db, AdtRefId::from_enum(db, *enum_)),
        };

        let adt_ty = TyId::adt(db, adt);
        adt.params(db).iter().fold(adt_ty, |ty, param| {
            let param_ty = table.new_var_from_param(*param);
            TyId::app(db, ty, param_ty)
        })
    }

    fn is_unit_variant(&self, db: &dyn HirAnalysisDb) -> bool {
        match self {
            Self::Adt(_, _) => false,
            Self::Variant(enum_, idx, _) => {
                let hir_db = db.as_hir_db();
                matches!(
                    enum_.variants(hir_db).data(hir_db)[*idx].kind,
                    hir::hir_def::VariantKind::Unit
                )
            }
        }
    }
}
