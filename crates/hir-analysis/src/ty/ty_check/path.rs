use std::collections::{hash_map::Entry, BTreeSet};

use hir::{
    hir_def::{
        scope_graph::{FieldParent, ScopeId},
        Enum, ExprId, FieldDefListId as HirFieldDefListId, IdentId, ItemKind, Partial, Pat, PatId,
        PathId, VariantKind as HirVariantKind,
    },
    span::DynLazySpan,
};
use if_chain::if_chain;
use rustc_hash::{FxHashMap, FxHashSet};

use super::{env::LocalBinding, TyChecker};
use crate::{
    name_resolution::{
        diagnostics::NameResDiag, is_scope_visible_from, resolve_path_early, resolve_query,
        resolve_segments_early, EarlyResolvedPath, NameDomain, NameQuery, NameRes, NameResBucket,
        NameResKind, QueryDirective,
    },
    ty::{
        diagnostics::{BodyDiag, FuncBodyDiag, TyLowerDiag},
        ty_def::{
            AdtDef, AdtFieldList, AdtRef, AdtRefId, FuncDef, InvalidCause, Subst, TyData, TyId,
        },
        ty_lower::{lower_adt, lower_func, lower_hir_ty},
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
        Pat::Path(..) => pat_span.into_path_pat().path(),
        Pat::PathTuple(..) => pat_span.into_path_tuple_pat().path(),
        Pat::Record(..) => pat_span.into_record_pat().path(),
        _ => unreachable!(),
    }
    .into();

    let mut resolver = PathResolver::new(tc, path, span.clone(), PathMode::Pat);

    match resolver.resolve_path() {
        ResolvedPathInBody::Ty(ty_in_body) => ResolvedPathInPat::Ty(ty_in_body),
        ResolvedPathInBody::Variant(variant) => ResolvedPathInPat::Variant(variant),
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

    let mut resolver = PathResolver::new(tc, path, span.clone(), PathMode::ExprValue);

    match resolver.resolve_path() {
        ResolvedPathInBody::Ty(ty_in_body) => ResolvedPathInExpr::Ty(ty_in_body),
        ResolvedPathInBody::Variant(variant) => ResolvedPathInExpr::Variant(variant),
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

    let mut resolver = PathResolver::new(tc, path, span.clone(), PathMode::RecordInit);

    match resolver.resolve_path() {
        ResolvedPathInBody::Ty(ty_in_body) => ResolvedPathInRecordInit::Ty(ty_in_body),
        ResolvedPathInBody::Variant(variant) => ResolvedPathInRecordInit::Variant(variant),

        ResolvedPathInBody::Binding(..) => {
            let diag = BodyDiag::record_expected::<TyInBody>(tc.db, span, None);
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
    Ty(TyInBody),
    Variant(ResolvedVariant),
    NewBinding(IdentId),
    Diag(FuncBodyDiag),
    Invalid,
}

#[derive(Clone, Debug)]
pub(super) enum ResolvedPathInExpr {
    Ty(TyInBody),
    Variant(ResolvedVariant),
    Binding(IdentId, LocalBinding),
    Diag(FuncBodyDiag),
    Invalid,
}

#[derive(Clone, Debug)]
pub(super) enum ResolvedPathInRecordInit {
    Ty(TyInBody),
    Variant(ResolvedVariant),
    Diag(FuncBodyDiag),
    Invalid,
}

#[derive(Clone, Debug)]
pub struct TyInBody {
    ty: TyId,
    subst: FxHashMap<TyId, TyId>,
    path: PathId,
}

impl TyInBody {
    pub(crate) fn ty(&mut self, db: &dyn HirAnalysisDb) -> TyId {
        let base = self.ty.decompose_ty_app(db).0;
        self.ty.generic_params(db).iter().fold(base, |ty, param| {
            let arg = param.apply_subst(db, &mut self.subst);
            TyId::app(db, ty, arg)
        })
    }

    pub(crate) fn is_func(&self, db: &dyn HirAnalysisDb) -> bool {
        self.func_def(db).is_some()
    }

    pub(crate) fn adt_ref(&self, db: &dyn HirAnalysisDb) -> Option<AdtRefId> {
        self.adt_def(db).map(|def| def.adt_ref(db))
    }

    pub(crate) fn adt_def(&self, db: &dyn HirAnalysisDb) -> Option<AdtDef> {
        let base = self.ty.decompose_ty_app(db).0;
        match base.data(db) {
            TyData::TyBase(base) => base.adt(),
            _ => None,
        }
    }

    pub(crate) fn func_def(&self, db: &dyn HirAnalysisDb) -> Option<FuncDef> {
        let base = self.ty.decompose_ty_app(db).0;
        match base.data(db) {
            TyData::TyBase(base) => base.func(),
            _ => None,
        }
    }

    pub(super) fn new(db: &dyn HirAnalysisDb, table: &mut UnificationTable, ty: TyId) -> Self {
        let (base, _) = ty.decompose_ty_app(db);
        let base_name = IdentId::new(db.as_hir_db(), base.pretty_print(db).to_string());
        let path = PathId::from_ident(db.as_hir_db(), base_name);

        Self::new_from_path_type(db, table, ty, path)
    }

    fn new_from_path_type(
        db: &dyn HirAnalysisDb,
        table: &mut UnificationTable,
        ty: TyId,
        path: PathId,
    ) -> Self {
        let args = ty.generic_args(db);
        let params = ty.generic_params(db);
        assert!(args.len() <= params.len());

        let mut subst = FxHashMap::default();
        for (param, arg) in params.iter().zip(args) {
            subst.insert(*param, *arg);
        }

        // Make fresh type variables for each unbound type parameter.
        let unbound = ty.unbound_params(db);
        assert!(unbound.len() == params[args.len()..].len());
        for &param in unbound {
            let fresh_ty = table.new_var_from_param(param);
            subst.insert(param, fresh_ty);
        }

        Self { ty, subst, path }
    }
}

#[derive(Clone, Debug)]
pub struct ResolvedVariant {
    enum_: Enum,
    idx: usize,
    subst: FxHashMap<TyId, TyId>,
    path: PathId,
}

impl ResolvedVariant {
    pub(super) fn ty(&mut self, db: &dyn HirAnalysisDb) -> TyId {
        let adt = lower_adt(db, AdtRefId::from_enum(db, self.enum_));

        let adt_ty = TyId::adt(db, adt);
        adt_ty.generic_params(db).iter().fold(adt_ty, |ty, arg| {
            let arg = arg.apply_subst(db, &mut self.subst);
            TyId::app(db, ty, arg)
        })
    }

    pub(super) fn adt_def(&self, db: &dyn HirAnalysisDb) -> AdtDef {
        lower_adt(db, AdtRefId::from_enum(db, self.enum_))
    }

    pub(super) fn variant_kind(&self, db: &dyn HirAnalysisDb) -> HirVariantKind {
        self.enum_.variants(db.as_hir_db()).data(db.as_hir_db())[self.idx].kind
    }

    pub(super) fn subst(&mut self) -> &mut impl Subst {
        &mut self.subst
    }

    fn new(
        db: &dyn HirAnalysisDb,
        table: &mut UnificationTable,
        enum_: Enum,
        idx: usize,
        path: PathId,
    ) -> Self {
        let adt = lower_adt(db, AdtRefId::from_enum(db, enum_));
        let subst = adt
            .params(db)
            .iter()
            .map(|param| (*param, table.new_var_from_param(*param)))
            .collect();

        Self {
            enum_,
            idx,
            subst,
            path,
        }
    }
}

struct PathResolver<'db, 'tc> {
    pub(super) tc: &'tc mut TyChecker<'db>,
    path: PathId,
    span: DynLazySpan,
    mode: PathMode,
}

impl<'db, 'env> PathResolver<'db, 'env> {
    fn new(tc: &'env mut TyChecker<'db>, path: PathId, span: DynLazySpan, mode: PathMode) -> Self {
        Self {
            tc,
            path,
            span,
            mode,
        }
    }

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

            PathMode::RecordInit => {
                let early_resolved_path =
                    resolve_path_early(self.tc.db, self.path, self.tc.env.scope());
                self.resolve_early_resolved_path(early_resolved_path)
            }

            PathMode::Pat => {
                let early_resolved_path =
                    resolve_path_early(self.tc.db, self.path, self.tc.env.scope());
                let resolved = self.resolve_early_resolved_path(early_resolved_path);

                match resolved {
                    ResolvedPathInBody::Variant(..) => resolved,
                    ResolvedPathInBody::Ty(ref ty_in_body) if ty_in_body.is_record(self.tc.db) => {
                        resolved
                    }
                    _ => {
                        if let Some(ident) = self.path.last_segment(hir_db).to_opt() {
                            ResolvedPathInBody::NewBinding(ident)
                        } else {
                            resolved
                        }
                    }
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
        match early {
            EarlyResolvedPath::Full(bucket) => self.resolve_bucket(bucket),

            // Try to resolve the partially resolved path as an enum variant.
            EarlyResolvedPath::Partial {
                res,
                unresolved_from,
            } => self.resolve_partial(res, unresolved_from),
        }
    }

    fn resolve_partial(&mut self, res: NameRes, unresolved_from: usize) -> ResolvedPathInBody {
        let db = self.tc.db;
        let hir_db = db.as_hir_db();

        let ty_in_body = match self.resolve_name_res(&res) {
            ResolvedPathInBody::Ty(ty) => ty,

            ResolvedPathInBody::Variant(_)
            | ResolvedPathInBody::Binding(_, _)
            | ResolvedPathInBody::NewBinding(..) => return ResolvedPathInBody::Invalid,

            resolved @ (ResolvedPathInBody::Diag(_) | ResolvedPathInBody::Invalid) => {
                return resolved
            }
        };

        // Check the possibility of resolving the partial path as an enum variant.
        if_chain! {
            if unresolved_from + 1 == self.path.len(hir_db);
            if let Some(adt_ref) = ty_in_body.adt_ref(db);
            if let AdtRef::Enum(enum_) = adt_ref.data(db);
            then {
                let segments = &self.path.segments(hir_db)[unresolved_from..];
                let scope = enum_.scope();
                let early = resolve_segments_early(self.tc.db, segments, scope);
                if let resolved @ ResolvedPathInBody::Variant(..) = self.resolve_early_resolved_path(early) {
                    return resolved;
                }
            }
        }

        // Try to resolve the partial as method call.
        // TODO: Try resolving this partial path as an associated function
        // function (i.e., method or trait function).

        // If the partial path is not resolved as an enum variant or method call,
        // report an error.
        let diag = TyLowerDiag::AssocTy(self.span.clone());
        ResolvedPathInBody::Diag(FuncBodyDiag::Ty(diag.into()))
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
                let ty = TyId::adt(self.tc.db, adt);
                TyInBody::new_from_path_type(self.tc.db, &mut self.tc.table, ty, self.path).into()
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Enum(enum_))) => {
                let adt = lower_adt(self.tc.db, AdtRefId::from_enum(self.tc.db, enum_));
                let ty = TyId::adt(self.tc.db, adt);
                TyInBody::new_from_path_type(self.tc.db, &mut self.tc.table, ty, self.path).into()
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Contract(contract_))) => {
                let adt = lower_adt(self.tc.db, AdtRefId::from_contract(self.tc.db, contract_));
                let ty = TyId::adt(self.tc.db, adt);
                TyInBody::new_from_path_type(self.tc.db, &mut self.tc.table, ty, self.path).into()
            }

            NameResKind::Scope(ScopeId::Variant(parent, idx)) => {
                let enum_: Enum = parent.try_into().unwrap();
                ResolvedVariant::new(self.tc.db, &mut self.tc.table, enum_, idx, self.path).into()
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Func(func))) => {
                let func_def = lower_func(self.tc.db, func).unwrap();
                let ty = TyId::func(self.tc.db, func_def);
                TyInBody::new_from_path_type(self.tc.db, &mut self.tc.table, ty, self.path).into()
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Impl(impl_))) => {
                match impl_.ty(self.tc.db.as_hir_db()).to_opt() {
                    Some(hir_ty) => {
                        let ty = lower_hir_ty(self.tc.db, hir_ty, self.tc.env.scope());
                        let ty_in_body = TyInBody::new_from_path_type(
                            self.tc.db,
                            &mut self.tc.table,
                            ty,
                            self.path,
                        );
                        ResolvedPathInBody::Ty(ty_in_body)
                    }

                    None => ResolvedPathInBody::Invalid,
                }
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Trait(..))) => {
                todo!()
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::ImplTrait(impl_trait))) => {
                match impl_trait.ty(self.tc.db.as_hir_db()).to_opt() {
                    Some(hir_ty) => {
                        let ty = lower_hir_ty(self.tc.db, hir_ty, self.tc.env.scope());
                        let ty_in_body = TyInBody::new_from_path_type(
                            self.tc.db,
                            &mut self.tc.table,
                            ty,
                            self.path,
                        );
                        ResolvedPathInBody::Ty(ty_in_body)
                    }

                    None => ResolvedPathInBody::Invalid,
                }
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Const(_))) => {
                todo!()
            }

            NameResKind::Prim(prim) => {
                let ty = TyId::from_hir_prim_ty(self.tc.db, prim);
                TyInBody::new_from_path_type(self.tc.db, &mut self.tc.table, ty, self.path).into()
            }

            _ => ResolvedPathInBody::Invalid,
        }
    }
}

#[derive(Clone, Debug, derive_more::From)]
enum ResolvedPathInBody {
    Ty(TyInBody),
    Variant(ResolvedVariant),
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

pub(super) struct RecordInitChecker<'tc, 'db, 'a, T> {
    pub(super) tc: &'tc mut TyChecker<'db>,
    data: &'a mut T,
    already_given: FxHashMap<IdentId, DynLazySpan>,
    invalid_field_given: bool,
}

impl<'tc, 'db, 'a, T> RecordInitChecker<'tc, 'db, 'a, T>
where
    T: RecordLike,
{
    /// Create a new `RecordInitChecker` for the given record path.
    ///
    /// ## Panics
    /// Panics if the given `data` is not a record.
    pub(super) fn new(tc: &'tc mut TyChecker<'db>, data: &'a mut T) -> Self {
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
        let label = match label {
            Some(label) => match self.already_given.entry(label) {
                Entry::Occupied(first_use) => {
                    let diag = BodyDiag::DuplicatedRecordFieldBind {
                        primary: field_span.clone(),
                        first_use: first_use.get().clone(),
                        name: label,
                    };

                    self.invalid_field_given = true;
                    return Err(diag.into());
                }

                Entry::Vacant(entry) => {
                    entry.insert(field_span.clone());
                    label
                }
            },

            None => {
                let diag = BodyDiag::ExplicitLabelExpectedInRecord {
                    primary: field_span,
                    hint: self.data.initializer_hint(self.tc.db),
                };

                self.invalid_field_given = true;
                return Err(diag.into());
            }
        };

        let Some(ty) = self.data.record_field_ty(self.tc.db, label) else {
            let diag = BodyDiag::record_field_not_found(field_span, label);

            self.invalid_field_given = true;
            return Err(diag.into());
        };

        let field_scope = self.data.record_field_scope(self.tc.db, label).unwrap();
        if is_scope_visible_from(self.tc.db, field_scope, self.tc.env.scope()) {
            Ok(ty)
        } else {
            let diag = NameResDiag::invisible(
                field_span,
                label,
                field_scope.name_span(self.tc.db.as_hir_db()),
            );

            self.invalid_field_given = true;
            Err(diag.into())
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

pub(crate) trait RecordLike {
    fn is_record(&self, db: &dyn HirAnalysisDb) -> bool;

    fn record_field_ty(&mut self, db: &dyn HirAnalysisDb, name: IdentId) -> Option<TyId>;

    fn record_field_list<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(HirFieldDefListId, &'db AdtFieldList)>;

    fn record_field_idx(&self, db: &dyn HirAnalysisDb, name: IdentId) -> Option<usize> {
        let (hir_field_list, _) = self.record_field_list(db)?;
        hir_field_list.field_idx(db.as_hir_db(), name)
    }

    fn record_field_scope(&self, db: &dyn HirAnalysisDb, name: IdentId) -> Option<ScopeId>;

    fn record_labels(&self, db: &dyn HirAnalysisDb) -> FxHashSet<IdentId>;

    fn initializer_hint(&self, db: &dyn HirAnalysisDb) -> Option<String>;

    fn kind_name(&mut self, db: &dyn HirAnalysisDb) -> String;
}

impl RecordLike for TyInBody {
    fn is_record(&self, db: &dyn HirAnalysisDb) -> bool {
        let Some(adt_ref) = self.adt_ref(db) else {
            return false;
        };

        matches!(adt_ref.data(db), AdtRef::Struct(..))
    }

    fn record_field_ty(&mut self, db: &dyn HirAnalysisDb, name: IdentId) -> Option<TyId> {
        let hir_db = db.as_hir_db();

        let (hir_field_list, field_list) = self.record_field_list(db)?;

        let field_idx = hir_field_list.field_idx(hir_db, name)?;
        let ty = field_list.ty(db, field_idx);

        if ty.is_star_kind(db) {
            ty.apply_subst(db, &mut self.subst)
        } else {
            TyId::invalid(db, InvalidCause::Other)
        }
        .into()
    }

    fn record_field_list<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(HirFieldDefListId, &'db AdtFieldList)> {
        let hir_db = db.as_hir_db();

        let adt_def = self.adt_def(db)?;
        match adt_def.adt_ref(db).data(db) {
            AdtRef::Struct(s) => (s.fields(hir_db), &adt_def.fields(db)[0]).into(),
            AdtRef::Contract(c) => (c.fields(hir_db), &adt_def.fields(db)[0]).into(),

            _ => None,
        }
    }

    fn record_field_scope(&self, db: &dyn HirAnalysisDb, name: IdentId) -> Option<ScopeId> {
        let field_idx = self.record_field_idx(db, name)?;
        let adt_ref = self.adt_ref(db)?;

        let parent = FieldParent::Item(adt_ref.as_item(db));
        Some(ScopeId::Field(parent, field_idx))
    }

    fn record_labels(&self, db: &dyn HirAnalysisDb) -> FxHashSet<IdentId> {
        let hir_db = db.as_hir_db();
        let Some(adt_ref) = self.adt_ref(db) else {
            return FxHashSet::default();
        };
        let fields = match adt_ref.data(db) {
            AdtRef::Struct(s) => s.fields(hir_db),
            AdtRef::Contract(c) => c.fields(hir_db),

            _ => return FxHashSet::default(),
        };

        fields
            .data(hir_db)
            .iter()
            .filter_map(|field| field.name.to_opt())
            .collect()
    }

    fn kind_name(&mut self, db: &dyn HirAnalysisDb) -> String {
        if let Some(adt_ref) = self.adt_ref(db) {
            adt_ref.kind_name(db).to_string()
        } else if self.is_func(db) {
            "fn".to_string()
        } else {
            self.ty(db).pretty_print(db).to_string()
        }
    }

    fn initializer_hint(&self, db: &dyn HirAnalysisDb) -> Option<String> {
        let hir_db = db.as_hir_db();

        let expected_sub_pat = if self.adt_ref(db).is_some() {
            let AdtRef::Struct(s) = self.adt_ref(db)?.data(db) else {
                return None;
            };

            s.format_initializer_args(db.as_hir_db())
        } else {
            return None;
        };

        let path = self.path.pretty_print(hir_db);
        Some(format!("{}{}", path, expected_sub_pat))
    }
}

impl RecordLike for ResolvedVariant {
    fn is_record(&self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.variant_kind(db), HirVariantKind::Record(..))
    }

    fn record_field_ty(&mut self, db: &dyn HirAnalysisDb, name: IdentId) -> Option<TyId> {
        let hir_db = db.as_hir_db();

        let (hir_field_list, field_list) = self.record_field_list(db)?;
        let field_idx = hir_field_list.field_idx(hir_db, name)?;
        let ty = field_list.ty(db, field_idx);

        Some(ty.apply_subst(db, &mut self.subst))
    }

    fn record_field_list<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(HirFieldDefListId, &'db AdtFieldList)> {
        match self.variant_kind(db) {
            hir::hir_def::VariantKind::Record(fields) => {
                (fields, &self.adt_def(db).fields(db)[self.idx]).into()
            }

            _ => None,
        }
    }

    fn record_field_scope(&self, db: &dyn HirAnalysisDb, name: IdentId) -> Option<ScopeId> {
        let field_idx = self.record_field_idx(db, name)?;
        let parent = FieldParent::Variant(self.enum_.into(), self.idx);
        Some(ScopeId::Field(parent, field_idx))
    }

    fn record_labels(&self, db: &dyn HirAnalysisDb) -> FxHashSet<IdentId> {
        let hir_db = db.as_hir_db();

        let fields = match self.variant_kind(db) {
            hir::hir_def::VariantKind::Record(fields) => fields,
            _ => return FxHashSet::default(),
        };

        fields
            .data(hir_db)
            .iter()
            .filter_map(|field| field.name.to_opt())
            .collect()
    }

    fn kind_name(&mut self, db: &dyn HirAnalysisDb) -> String {
        let hir_db = db.as_hir_db();
        match self.enum_.variants(hir_db).data(hir_db)[self.idx].kind {
            HirVariantKind::Unit => "unit variant",
            HirVariantKind::Tuple(_) => "tuple variant",
            HirVariantKind::Record(_) => "record variant",
        }
        .to_string()
    }

    fn initializer_hint(&self, db: &dyn HirAnalysisDb) -> Option<String> {
        let hir_db = db.as_hir_db();
        let expected_sub_pat =
            self.enum_.variants(hir_db).data(hir_db)[self.idx].format_initializer_args(hir_db);

        let path = self.path.pretty_print(hir_db);
        Some(format!("{}{}", path, expected_sub_pat))
    }
}
