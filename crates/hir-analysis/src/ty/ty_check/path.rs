use std::collections::{hash_map::Entry, BTreeSet};

use hir::{
    hir_def::{
        scope_graph::ScopeId, Enum, ExprId, IdentId, ItemKind, Partial, Pat, PatId, PathId,
        VariantKind as HirVariantKind,
    },
    span::DynLazySpan,
};
use rustc_hash::{FxHashMap, FxHashSet};

use super::{env::LocalBinding, TyChecker};
use crate::{
    name_resolution::{
        resolve_path_early, resolve_query, resolve_segments_early, EarlyResolvedPath, NameDomain,
        NameQuery, NameRes, NameResBucket, NameResKind, QueryDirective,
    },
    ty::{
        diagnostics::{BodyDiag, FuncBodyDiag, TyLowerDiag},
        ty_def::{AdtDef, AdtRef, AdtRefId, FuncDef, Subst, TyId},
        ty_lower::{lower_adt, lower_func},
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

    let pat_span = pat.lazy_span(tc.body());
    let span: DynLazySpan = match pat_data {
        Pat::Path(_, _) => pat_span.into_path_pat().path(),
        Pat::PathTuple(..) => pat_span.into_path_tuple_pat().path(),
        Pat::Record(..) => pat_span.into_record_pat().path(),
        _ => unreachable!(),
    }
    .into();

    let mut resolver = PathResolver {
        tc,
        path,
        span: span.clone(),
        mode: PathMode::Pat,
    };

    match resolver.resolve_path() {
        ResolvedPathInBody::Data(data) => ResolvedPathInPat::Data(data),
        ResolvedPathInBody::Func(func) => {
            let diag = BodyDiag::InvalidPathDomainInPat {
                primary: span,
                resolved: Some(func.name_span(tc.db)),
            };
            ResolvedPathInPat::Diag(diag.into())
        }
        ResolvedPathInBody::Binding(binding, _) | ResolvedPathInBody::NewBinding(binding) => {
            ResolvedPathInPat::NewBinding(binding)
        }
        ResolvedPathInBody::Diag(diag) => ResolvedPathInPat::Diag(diag),
        ResolvedPathInBody::Invalid => ResolvedPathInPat::Invalid,
    }
}

pub(super) fn resolve_path_in_expr(
    tc: &mut TyChecker,
    path: PathId,
    path_expr: ExprId,
) -> ResolvedPathInExpr {
    let span: DynLazySpan = path_expr
        .lazy_span(tc.body())
        .into_path_expr()
        .path()
        .into();

    let mut resolver = PathResolver {
        tc,
        path,
        span: span.clone(),
        mode: PathMode::ExprValue,
    };

    match resolver.resolve_path() {
        ResolvedPathInBody::Data(data) => ResolvedPathInExpr::Data(data),
        ResolvedPathInBody::Func(func) => ResolvedPathInExpr::Func(func),
        ResolvedPathInBody::Binding(ident, binding) => ResolvedPathInExpr::Binding(ident, binding),
        ResolvedPathInBody::NewBinding(ident) => {
            let diag = BodyDiag::UndefinedVariable(span, ident);

            ResolvedPathInExpr::Diag(diag.into())
        }
        ResolvedPathInBody::Diag(diag) => ResolvedPathInExpr::Diag(diag),
        ResolvedPathInBody::Invalid => ResolvedPathInExpr::Invalid,
    }
}

pub(super) fn resolve_path_in_record_init(
    tc: &mut TyChecker,
    path: PathId,
    record_init_expr: ExprId,
) -> ResolvedPathInRecordInit {
    let span: DynLazySpan = record_init_expr
        .lazy_span(tc.body())
        .into_record_init_expr()
        .path()
        .into();

    let mut resolver = PathResolver {
        tc,
        path,
        span: span.clone(),
        mode: PathMode::RecordInit,
    };

    match resolver.resolve_path() {
        ResolvedPathInBody::Data(data) => ResolvedPathInRecordInit::Data(data),
        ResolvedPathInBody::Func(..) | ResolvedPathInBody::Binding(..) => {
            let diag = BodyDiag::record_expected(tc.db, span, None);
            ResolvedPathInRecordInit::Diag(diag.into())
        }
        ResolvedPathInBody::NewBinding(ident) => {
            let diag = BodyDiag::UndefinedVariable(span, ident);
            ResolvedPathInRecordInit::Diag(diag.into())
        }
        ResolvedPathInBody::Diag(diag) => ResolvedPathInRecordInit::Diag(diag),
        ResolvedPathInBody::Invalid => ResolvedPathInRecordInit::Invalid,
    }
}

#[derive(Clone, Debug)]
pub(super) enum ResolvedPathInPat {
    Data(ResolvedPathData),
    NewBinding(IdentId),
    Diag(FuncBodyDiag),
    Invalid,
}

#[derive(Clone, Debug)]
pub(super) enum ResolvedPathInExpr {
    Data(ResolvedPathData),
    Func(ResolvedPathFunc),
    Binding(IdentId, LocalBinding),
    Diag(FuncBodyDiag),
    Invalid,
}

#[derive(Clone, Debug)]
pub(super) enum ResolvedPathInRecordInit {
    Data(ResolvedPathData),
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

#[derive(Clone, Debug, derive_more::From)]
pub(crate) struct ResolvedPathFunc {
    def: FuncDef,
    args: FxHashMap<TyId, TyId>,
}

impl ResolvedPathFunc {
    pub(crate) fn name_span(&self, db: &dyn HirAnalysisDb) -> DynLazySpan {
        self.def.name_span(db)
    }

    pub(super) fn ty(&self, db: &dyn HirAnalysisDb) -> TyId {
        let ty = TyId::func(db, self.def);
        self.args().fold(ty, |ty, arg| TyId::app(db, ty, *arg))
    }

    fn new(db: &dyn HirAnalysisDb, def: FuncDef, table: &mut UnificationTable) -> Self {
        let args = def
            .params(db)
            .iter()
            .map(|param| (*param, table.new_var_from_param(*param)))
            .collect();

        Self { def, args }
    }

    fn args(&self) -> impl Iterator<Item = &TyId> {
        self.args.values()
    }
}

struct PathResolver<'db, 'tc> {
    pub(super) tc: &'tc mut TyChecker<'db>,
    path: PathId,
    span: DynLazySpan,
    mode: PathMode,
}

impl<'db, 'env> PathResolver<'db, 'env> {
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
            PathMode::ExprValue => self.resolve_ident_expr(ident),

            PathMode::RecordInit | PathMode::Pat => {
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

    fn resolve_ident_expr(&mut self, ident: IdentId) -> ResolvedPathInBody {
        let mut current_idx = self.tc.env.current_block_idx();
        loop {
            let env = self.tc.env.get_block(current_idx);
            if let Some(binding) = env.lookup_var(ident) {
                return ResolvedPathInBody::Binding(ident, binding);
            }

            let scope = env.scope;
            let directive = QueryDirective::new().disallow_lex();
            let query = NameQuery::with_directive(ident, scope, directive);
            let bucket = resolve_query(self.tc.db, query);

            let resolved = self.resolve_bucket(bucket);
            match resolved {
                ResolvedPathInBody::Invalid => {
                    if current_idx == 0 {
                        break;
                    } else {
                        current_idx -= 1;
                    }
                }
                _ => return resolved,
            }
        }

        let query = NameQuery::new(ident, self.tc.body().scope());
        let bucket = resolve_query(self.tc.db, query);

        let resolved = self.resolve_bucket(bucket);
        match resolved {
            ResolvedPathInBody::Invalid => ResolvedPathInBody::NewBinding(ident),
            resolved => resolved,
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
                // TODO: Try resolving this partial path as an associated function(including
                // method) or trait function.
                let span = self.span.clone();
                let diag = TyLowerDiag::AssocTy(span);
                ResolvedPathInBody::Diag(FuncBodyDiag::Ty(diag.into()))
            }
        }
    }

    fn resolve_bucket(&mut self, bucket: NameResBucket) -> ResolvedPathInBody {
        match self.mode {
            PathMode::ExprValue => {
                match bucket.pick(NameDomain::Value) {
                    Ok(res) => self.resolve_name_res(res),
                    Err(_) => {
                        if let Ok(res) = bucket.pick(NameDomain::Type) {
                            self.resolve_name_res(res)
                        } else {
                            // This error is already reported in the name resolution phase.
                            ResolvedPathInBody::Invalid
                        }
                    }
                }
            }

            PathMode::RecordInit | PathMode::Pat => {
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

            NameResKind::Scope(ScopeId::Item(ItemKind::Enum(enum_))) => {
                let adt = lower_adt(self.tc.db, AdtRefId::from_enum(self.tc.db, enum_));
                ResolvedPathData::new_adt(self.tc.db, &mut self.tc.table, adt, self.path).into()
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Contract(contract_))) => {
                let adt = lower_adt(self.tc.db, AdtRefId::from_contract(self.tc.db, contract_));
                ResolvedPathData::new_adt(self.tc.db, &mut self.tc.table, adt, self.path).into()
            }

            NameResKind::Scope(ScopeId::Variant(parent, idx)) => {
                let enum_: Enum = parent.try_into().unwrap();
                ResolvedPathData::new_variant(self.tc.db, &mut self.tc.table, enum_, idx, self.path)
                    .into()
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Func(func))) => {
                let func_def = lower_func(self.tc.db, func).unwrap();
                ResolvedPathFunc::new(self.tc.db, func_def, &mut self.tc.table).into()
            }

            _ => ResolvedPathInBody::Invalid,
        }
    }
}

#[derive(Clone, Debug, derive_more::From)]
enum ResolvedPathInBody {
    Data(ResolvedPathData),
    Func(ResolvedPathFunc),
    #[from(ignore)]
    Binding(IdentId, LocalBinding),
    NewBinding(IdentId),
    Diag(FuncBodyDiag),
    Invalid,
}

enum PathMode {
    ExprValue,
    RecordInit,
    Pat,
}

pub(super) struct RecordInitChecker<'tc, 'db, 'a> {
    pub(super) tc: &'tc mut TyChecker<'db>,
    data: &'a mut ResolvedPathData,
    already_given: FxHashMap<IdentId, DynLazySpan>,
    invalid_field_given: bool,
}

impl<'tc, 'db, 'a> RecordInitChecker<'tc, 'db, 'a> {
    /// Create a new `RecordInitChecker` for the given record path.
    ///
    /// ## Panics
    /// Panics if the given `data` is not a record.
    pub(super) fn new(tc: &'tc mut TyChecker<'db>, data: &'a mut ResolvedPathData) -> Self {
        assert!(data.is_record(tc.db));

        Self {
            tc,
            data,
            already_given: FxHashMap::default(),
            invalid_field_given: false,
        }
    }

    /// Feed a label to the checker.
    /// Returns the type of the field if the label is valid, otherwise returns
    /// an error.
    pub(super) fn feed_label(
        &mut self,
        label: Option<IdentId>,
        field_span: DynLazySpan,
    ) -> Result<TyId, FuncBodyDiag> {
        match label {
            Some(label) => match self.already_given.entry(label) {
                Entry::Occupied(first_use) => {
                    let diag = BodyDiag::DuplicatedRecordFieldBind {
                        primary: field_span.clone(),
                        first_use: first_use.get().clone(),
                        name: label,
                    };
                    self.invalid_field_given = true;

                    Err(diag.into())
                }

                Entry::Vacant(entry) => {
                    entry.insert(field_span.clone());
                    if let Some(ty) = self.data.record_field_ty(self.tc.db, label) {
                        Ok(ty)
                    } else {
                        let diag = BodyDiag::record_field_not_found(
                            self.tc.db, field_span, self.data, label,
                        );
                        self.invalid_field_given = true;

                        Err(diag.into())
                    }
                }
            },

            None => {
                let diag = BodyDiag::ExplicitLabelExpectedInRecord {
                    primary: field_span,
                    hint: self.data.initializer_hint(self.tc.db),
                };
                self.invalid_field_given = true;

                Err(diag.into())
            }
        }
    }

    /// Finalize the checker and return an error if there are missing fields.
    pub(super) fn finalize(
        self,
        initializer_span: DynLazySpan,
        allow_missing_field: bool,
    ) -> Result<(), FuncBodyDiag> {
        if !self.invalid_field_given && !allow_missing_field {
            let expected_labels = self.data.record_labels(self.tc.db);
            let found = self.already_given.keys().copied().collect::<FxHashSet<_>>();
            let missing_fields: BTreeSet<IdentId> =
                expected_labels.difference(&found).copied().collect();

            if !missing_fields.is_empty() {
                let diag = BodyDiag::MissingRecordFields {
                    primary: initializer_span,
                    missing_fields,
                    hint: self.data.initializer_hint(self.tc.db),
                };

                return Err(diag.into());
            }
        }

        Ok(())
    }
}
