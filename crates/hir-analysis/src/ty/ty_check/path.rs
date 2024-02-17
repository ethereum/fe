use hir::{
    hir_def::{
        scope_graph::ScopeId, Enum, IdentId, ItemKind, Partial, Pat, PatId, PathId,
        VariantKind as HirVariantKind,
    },
    span::DynLazySpan,
};
use rustc_hash::{FxHashMap, FxHashSet};

use super::TyChecker;
use crate::{
    name_resolution::{
        resolve_path_early, resolve_segments_early, EarlyResolvedPath, NameDomain, NameRes,
        NameResBucket, NameResKind,
    },
    ty::{
        diagnostics::{BodyDiag, FuncBodyDiag, TyLowerDiag},
        ty_def::{AdtDef, AdtRef, AdtRefId, Subst, TyId},
        ty_lower::lower_adt,
        unify::UnificationTable,
    },
    HirAnalysisDb,
};

pub(super) fn resolve_path_in_pat(
    tc: &mut TyChecker,
    path: PathId,
    pat: PatId,
) -> ResolvedPathInPat {
    let Partial::Present(pat_data) = pat.data(tc.db.as_hir_db(), tc.env.body()) else {
        unreachable!()
    };

    let pat_span = pat.lazy_span(tc.env.body());

    let span = match pat_data {
        Pat::Path(_) => pat_span.into_path_pat().path(),
        Pat::PathTuple(..) => pat_span.into_path_tuple_pat().path(),
        Pat::Record(..) => pat_span.into_record_pat().path(),
        _ => unreachable!(),
    }
    .into();

    let mut resolver = PatPathResolver {
        tc,
        path,
        span,
        mode: PathMode::Pat,
    };

    match resolver.resolve_path() {
        ResolvedPathInBody::Data(data) => ResolvedPathInPat::Data(data),
        ResolvedPathInBody::NewBinding(binding) => ResolvedPathInPat::NewBinding(binding),
        ResolvedPathInBody::Diag(diag) => ResolvedPathInPat::Diag(diag),
        ResolvedPathInBody::Invalid => ResolvedPathInPat::Invalid,
    }
}

#[derive(Clone, Debug)]
pub(super) enum ResolvedPathInPat {
    Data(ResolvedPathData),
    NewBinding(IdentId),
    Diag(FuncBodyDiag),
    Invalid,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum ResolvedPathData {
    Adt {
        adt: AdtDef,
        args: FxHashMap<TyId, TyId>,
        path: PathId,
    },

    Variant {
        enum_: Enum,
        idx: usize,
        args: FxHashMap<TyId, TyId>,
        path: PathId,
    },
}

impl ResolvedPathData {
    pub(crate) fn adt_ref(&self, db: &dyn HirAnalysisDb) -> AdtRefId {
        match self {
            Self::Adt { adt, .. } => adt.adt_ref(db),
            Self::Variant { enum_, .. } => AdtRefId::from_enum(db, *enum_),
        }
    }

    pub(crate) fn adt_def(&self, db: &dyn HirAnalysisDb) -> AdtDef {
        match self {
            Self::Adt { adt, .. } => *adt,
            Self::Variant { enum_, .. } => lower_adt(db, AdtRefId::from_enum(db, *enum_)),
        }
    }

    pub(crate) fn data_kind(&self, db: &dyn HirAnalysisDb) -> &'static str {
        match self {
            Self::Adt { adt, .. } => adt.adt_ref(db).kind_name(db),
            Self::Variant { enum_, idx, .. } => {
                let hir_db = db.as_hir_db();
                match enum_.variants(hir_db).data(hir_db)[*idx].kind {
                    hir::hir_def::VariantKind::Unit => "unit variant",
                    HirVariantKind::Tuple(_) => "tuple variant",
                    HirVariantKind::Record(_) => "record variant",
                }
            }
        }
    }

    pub(crate) fn initializer_hint(&self, db: &dyn HirAnalysisDb) -> Option<String> {
        let hir_db = db.as_hir_db();

        let expected_sub_pat = match self {
            Self::Adt { .. } => {
                let AdtRef::Struct(s) = self.adt_ref(db).data(db) else {
                    return None;
                };

                s.format_initializer_args(hir_db)
            }

            Self::Variant { enum_, idx, .. } => {
                enum_.variants(hir_db).data(hir_db)[*idx].format_initializer_args(hir_db)
            }
        };

        let path = self.path().pretty_print(hir_db);
        Some(format!("{}{}", path, expected_sub_pat))
    }

    pub(crate) fn def_span(&self, db: &dyn HirAnalysisDb) -> DynLazySpan {
        match self {
            Self::Adt { .. } => self.adt_ref(db).name_span(db),
            Self::Variant { enum_, idx, .. } => {
                enum_.lazy_span().variants_moved().variant(*idx).into()
            }
        }
    }

    pub(crate) fn def_name(&self, db: &dyn HirAnalysisDb) -> IdentId {
        match self {
            Self::Adt { adt, .. } => adt.adt_ref(db).name(db),
            Self::Variant { enum_, idx, .. } => {
                *enum_.variants(db.as_hir_db()).data(db.as_hir_db())[*idx]
                    .name
                    .unwrap()
            }
        }
    }

    pub(super) fn path(&self) -> PathId {
        match self {
            Self::Adt { path, .. } => *path,
            Self::Variant { path, .. } => *path,
        }
    }

    pub(super) fn is_record(&self, db: &dyn HirAnalysisDb) -> bool {
        match self {
            Self::Adt { adt, .. } => matches!(
                adt.adt_ref(db).data(db),
                AdtRef::Struct(_) | AdtRef::Contract(_)
            ),

            Self::Variant { enum_, idx, .. } => matches!(
                enum_.variants(db.as_hir_db()).data(db.as_hir_db())[*idx].kind,
                HirVariantKind::Record(_)
            ),
        }
    }

    pub(super) fn is_unit_variant(&self, db: &dyn HirAnalysisDb) -> bool {
        match self {
            Self::Adt { .. } => false,
            Self::Variant { enum_, idx, .. } => {
                let hir_db = db.as_hir_db();
                matches!(
                    enum_.variants(hir_db).data(hir_db)[*idx].kind,
                    hir::hir_def::VariantKind::Unit
                )
            }
        }
    }

    pub(super) fn is_tuple_variant(&self, db: &dyn HirAnalysisDb) -> bool {
        match self {
            Self::Adt { .. } => false,
            Self::Variant { enum_, idx, .. } => {
                let hir_db = db.as_hir_db();
                matches!(
                    enum_.variants(hir_db).data(hir_db)[*idx].kind,
                    HirVariantKind::Tuple(_)
                )
            }
        }
    }

    pub(super) fn ty(&self, db: &dyn HirAnalysisDb) -> TyId {
        let adt = match self {
            Self::Adt { adt, .. } => *adt,

            Self::Variant { enum_, .. } => lower_adt(db, AdtRefId::from_enum(db, *enum_)),
        };

        let adt_ty = TyId::adt(db, adt);
        self.args().fold(adt_ty, |ty, arg| TyId::app(db, ty, *arg))
    }

    pub(super) fn record_field_ty(
        &mut self,
        db: &dyn HirAnalysisDb,
        name: IdentId,
    ) -> Option<TyId> {
        let hir_db = db.as_hir_db();

        let (hir_field_list, field_list) = match self {
            Self::Adt { adt, .. } => match adt.adt_ref(db).data(db) {
                AdtRef::Struct(s) => (s.fields(hir_db), &adt.fields(db)[0]),
                AdtRef::Contract(c) => (c.fields(hir_db), &adt.fields(db)[0]),

                _ => return None,
            },

            Self::Variant { enum_, ref idx, .. } => {
                match enum_.variants(hir_db).data(hir_db)[*idx].kind {
                    hir::hir_def::VariantKind::Record(fields) => {
                        (fields, &self.adt_def(db).fields(db)[*idx])
                    }

                    _ => return None,
                }
            }
        };

        let field_idx = hir_field_list.field_idx(hir_db, name)?;
        let ty = field_list.ty(db, field_idx);

        Some(ty.apply_subst(db, self.subst()))
    }

    pub(super) fn record_labels(&mut self, db: &dyn HirAnalysisDb) -> FxHashSet<IdentId> {
        let hir_db = db.as_hir_db();

        let fields = match self {
            Self::Adt { adt, .. } => match adt.adt_ref(db).data(db) {
                AdtRef::Struct(s) => s.fields(hir_db),
                AdtRef::Contract(c) => c.fields(hir_db),

                _ => return FxHashSet::default(),
            },

            Self::Variant { enum_, ref idx, .. } => {
                match enum_.variants(hir_db).data(hir_db)[*idx].kind {
                    hir::hir_def::VariantKind::Record(fields) => fields,

                    _ => return FxHashSet::default(),
                }
            }
        };

        fields
            .data(hir_db)
            .iter()
            .filter_map(|field| field.name.to_opt())
            .collect()
    }

    pub(super) fn field_tys(&mut self, db: &dyn HirAnalysisDb) -> Vec<TyId> {
        let (adt, idx) = match self {
            Self::Adt { adt, .. } => {
                if matches!(
                    adt.adt_ref(db).data(db),
                    AdtRef::Struct(_) | AdtRef::Contract(_)
                ) {
                    (*adt, 0)
                } else {
                    return vec![];
                }
            }
            Self::Variant { enum_, idx, .. } => {
                let adt = lower_adt(db, AdtRefId::from_enum(db, *enum_));
                (adt, *idx)
            }
        };

        adt.fields(db)[idx]
            .iter_types(db)
            .map(|ty| ty.apply_subst(db, self.subst()))
            .collect()
    }

    fn new_adt(
        db: &dyn HirAnalysisDb,
        table: &mut UnificationTable,
        adt: AdtDef,
        path: PathId,
    ) -> Self {
        let args = adt
            .params(db)
            .iter()
            .map(|param| (*param, table.new_var_from_param(*param)))
            .collect();

        Self::Adt { adt, args, path }
    }

    fn new_variant(
        db: &dyn HirAnalysisDb,
        table: &mut UnificationTable,
        enum_: Enum,
        idx: usize,
        path: PathId,
    ) -> Self {
        let args = lower_adt(db, AdtRefId::from_enum(db, enum_))
            .params(db)
            .iter()
            .map(|param| (*param, table.new_var_from_param(*param)))
            .collect();

        Self::Variant {
            enum_,
            idx,
            args,
            path,
        }
    }

    fn args(&self) -> impl Iterator<Item = &TyId> {
        match self {
            Self::Adt { args, .. } | Self::Variant { args, .. } => args.values(),
        }
    }

    fn subst(&mut self) -> &mut impl Subst {
        match self {
            Self::Adt { args, .. } | Self::Variant { args, .. } => args,
        }
    }
}

struct PatPathResolver<'db, 'tc> {
    tc: &'tc mut TyChecker<'db>,
    path: PathId,
    span: DynLazySpan,
    mode: PathMode,
}

impl<'db, 'env> PatPathResolver<'db, 'env> {
    fn resolve_path(&mut self) -> ResolvedPathInBody {
        let hir_db = self.tc.db.as_hir_db();

        if self.path.is_ident(hir_db) {
            let Some(ident) = self.path.last_segment(hir_db).to_opt() else {
                return ResolvedPathInBody::Invalid;
            };
            self.resolve_ident(ident)
        } else {
            let early_resolved_path =
                resolve_path_early(self.tc.db, self.path, self.tc.env.scope());
            self.resolve_early_resolved_path(early_resolved_path)
        }
    }

    fn resolve_ident(&mut self, ident: IdentId) -> ResolvedPathInBody {
        let hir_db = self.tc.db.as_hir_db();

        match self.mode {
            PathMode::ExprValue => todo!(),
            PathMode::RecordInit => todo!(),
            PathMode::Pat => {
                let early_resolved_path =
                    resolve_path_early(self.tc.db, self.path, self.tc.env.scope());
                let resolved = self.resolve_early_resolved_path(early_resolved_path);

                match resolved {
                    ResolvedPathInBody::Diag(_) | ResolvedPathInBody::Invalid => {
                        if let Some(ident) = self.path.last_segment(hir_db).to_opt() {
                            ResolvedPathInBody::NewBinding(ident)
                        } else {
                            resolved
                        }
                    }

                    _ => resolved,
                }
            }
        }
    }

    fn resolve_early_resolved_path(&mut self, early: EarlyResolvedPath) -> ResolvedPathInBody {
        let hir_db = self.tc.db.as_hir_db();

        // Try to resolve the partially resolved path as an enum variant.
        let early = match early {
            EarlyResolvedPath::Partial {
                res,
                unresolved_from,
            } if res.is_enum() && unresolved_from + 1 == self.path.len(hir_db) => {
                let segments = &self.path.segments(hir_db)[unresolved_from..];
                let scope = res.scope().unwrap();
                resolve_segments_early(self.tc.db, segments, scope)
            }

            _ => early,
        };

        match early {
            EarlyResolvedPath::Full(bucket) => self.resolve_bucket(bucket),

            EarlyResolvedPath::Partial { .. } => {
                let span = self.span.clone();
                let diag = TyLowerDiag::AssocTy(span);
                ResolvedPathInBody::Diag(FuncBodyDiag::Ty(diag.into()))
            }
        }
    }

    fn resolve_bucket(&mut self, bucket: NameResBucket) -> ResolvedPathInBody {
        match self.mode {
            PathMode::ExprValue => {
                if let Ok(res) = bucket.pick(NameDomain::Value) {
                    self.resolve_name_res(res)
                } else {
                    todo!()
                }
            }

            PathMode::RecordInit => {
                if let Ok(res) = bucket.pick(NameDomain::Type) {
                    self.resolve_name_res(res)
                } else {
                    todo!()
                }
            }

            PathMode::Pat => {
                if let Ok(res) = bucket.pick(NameDomain::Value) {
                    let res = self.resolve_name_res(res);
                    if !matches!(
                        res,
                        ResolvedPathInBody::Diag(_) | ResolvedPathInBody::Invalid
                    ) {
                        return res;
                    }
                }

                match bucket.pick(NameDomain::Type) {
                    Ok(res) => self.resolve_name_res(res),
                    Err(_) => ResolvedPathInBody::Invalid,
                }
            }
        }
    }

    fn resolve_name_res(&mut self, res: &NameRes) -> ResolvedPathInBody {
        match res.kind {
            NameResKind::Scope(ScopeId::Item(ItemKind::Struct(struct_))) => {
                let adt = lower_adt(self.tc.db, AdtRefId::from_struct(self.tc.db, struct_));
                ResolvedPathData::new_adt(self.tc.db, &mut self.tc.table, adt, self.path).into()
            }

            NameResKind::Scope(ScopeId::Variant(parent, idx)) => {
                let enum_: Enum = parent.try_into().unwrap();
                ResolvedPathData::new_variant(self.tc.db, &mut self.tc.table, enum_, idx, self.path)
                    .into()
            }

            _ => {
                let diag: FuncBodyDiag = match self.mode {
                    PathMode::ExprValue => {
                        todo!()
                    }

                    PathMode::RecordInit => {
                        todo!()
                    }

                    PathMode::Pat => BodyDiag::InvalidPathDomainInPat {
                        primary: self.span.clone(),
                        resolved: res
                            .scope()
                            .and_then(|scope| scope.name_span(self.tc.db.as_hir_db())),
                    },
                }
                .into();

                diag.into()
            }
        }
    }
}

#[derive(Clone, Debug, derive_more::From)]
enum ResolvedPathInBody {
    Data(ResolvedPathData),
    NewBinding(IdentId),
    Diag(FuncBodyDiag),
    Invalid,
}

enum PathMode {
    ExprValue,
    RecordInit,
    Pat,
}
