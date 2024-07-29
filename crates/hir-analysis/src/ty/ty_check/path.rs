use std::collections::hash_map::Entry;

use common::indexmap::IndexSet;
use either::Either;
use hir::{
    hir_def::{
        scope_graph::{FieldParent, ScopeId},
        Enum, FieldDefListId as HirFieldDefListId, IdentId, ItemKind, PathId,
        VariantKind as HirVariantKind,
    },
    span::{path::LazyPathSpan, DynLazySpan},
};
use if_chain::if_chain;
use rustc_hash::{FxHashMap, FxHashSet};

use super::{env::LocalBinding, TyChecker};
use crate::{
    name_resolution::{
        diagnostics::NameResDiag, is_scope_visible_from, resolve_path_early,
        resolve_path_tail_in_scope, resolve_query, EarlyNameQueryId, EarlyResolvedPath, NameDomain,
        NameRes, NameResBucket, NameResKind, QueryDirective,
    },
    ty::{
        adt_def::{lower_adt, AdtDef, AdtField, AdtRef, AdtRefId},
        canonical::Canonicalized,
        diagnostics::{BodyDiag, FuncBodyDiag, TyLowerDiag},
        func_def::lower_func,
        trait_def::TraitDef,
        trait_lower::lower_trait,
        ty_check::method_selection::{select_method_candidate, Candidate},
        ty_def::{InvalidCause, TyData, TyId},
        ty_lower::{collect_generic_params, lower_hir_ty, GenericParamOwnerId},
        unify::UnificationTable,
    },
    HirAnalysisDb,
};

pub(super) fn resolve_path<'db>(
    tc: &mut TyChecker<'db>,
    path: PathId<'db>,
    span: LazyPathSpan<'db>,
    mode: ResolutionMode,
) -> ResolvedPathInBody<'db> {
    PathResolver::new(tc, path, span.clone(), mode).resolve_path()
}

impl<'db> TyId<'db> {
    pub(crate) fn adt_ref(&self, db: &'db dyn HirAnalysisDb) -> Option<AdtRefId<'db>> {
        self.adt_def(db).map(|def| def.adt_ref(db))
    }

    pub(crate) fn adt_def(&self, db: &'db dyn HirAnalysisDb) -> Option<AdtDef<'db>> {
        let base = self.decompose_ty_app(db).0;
        match base.data(db) {
            TyData::TyBase(base) => base.adt(),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ResolvedVariant<'db> {
    enum_: Enum<'db>,
    idx: usize,
    args: Vec<TyId<'db>>,
    path: PathId<'db>,
}

impl<'db> ResolvedVariant<'db> {
    pub(super) fn ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let adt = lower_adt(db, AdtRefId::from_enum(db, self.enum_));

        let mut ty = TyId::adt(db, adt);
        for arg in &self.args {
            ty = TyId::app(db, ty, *arg);
        }
        ty
    }

    pub(super) fn adt_def(&self, db: &'db dyn HirAnalysisDb) -> AdtDef<'db> {
        lower_adt(db, AdtRefId::from_enum(db, self.enum_))
    }

    pub(super) fn variant_kind(&self, db: &'db dyn HirAnalysisDb) -> HirVariantKind<'db> {
        self.enum_.variants(db.as_hir_db()).data(db.as_hir_db())[self.idx].kind
    }

    pub(super) fn args(&self) -> &[TyId<'db>] {
        &self.args
    }

    fn new(
        db: &'db dyn HirAnalysisDb,
        table: &mut UnificationTable<'db>,
        enum_: Enum<'db>,
        idx: usize,
        path: PathId<'db>,
    ) -> Self {
        let adt = lower_adt(db, AdtRefId::from_enum(db, enum_));
        let args = adt
            .params(db)
            .iter()
            .map(|param| table.new_var_from_param(*param))
            .collect();

        Self {
            enum_,
            idx,
            args,
            path,
        }
    }
}

struct PathResolver<'db, 'tc> {
    pub(super) tc: &'tc mut TyChecker<'db>,
    path: PathId<'db>,
    span: LazyPathSpan<'db>,
    mode: ResolutionMode,
}

impl<'db, 'env> PathResolver<'db, 'env> {
    fn new(
        tc: &'env mut TyChecker<'db>,
        path: PathId<'db>,
        span: LazyPathSpan<'db>,
        mode: ResolutionMode,
    ) -> Self {
        Self {
            tc,
            path,
            span,
            mode,
        }
    }

    fn resolve_path(&mut self) -> ResolvedPathInBody<'db> {
        let hir_db = self.tc.db.as_hir_db();

        if let Some(ident) = self.path.as_ident(hir_db) {
            self.resolve_ident(ident)
        } else {
            resolve_path_early(self.tc.db, self.path, self.tc.env.scope()).map_or_else(
                || ResolvedPathInBody::Invalid,
                |res| self.resolve_path_late(res),
            )
        }
    }

    // xxx only used for single segment path
    fn resolve_ident(&mut self, ident: IdentId<'db>) -> ResolvedPathInBody<'db> {
        let hir_db = self.tc.db.as_hir_db();

        match self.mode {
            ResolutionMode::ExprValue => self.resolve_ident_expr(ident),

            ResolutionMode::RecordInit => {
                resolve_path_early(self.tc.db, self.path, self.tc.env.scope()).map_or_else(
                    || ResolvedPathInBody::Invalid,
                    |res| self.resolve_path_late(res),
                )
            }

            ResolutionMode::Pat => {
                let resolved = resolve_path_early(self.tc.db, self.path, self.tc.env.scope())
                    .map_or_else(
                        || ResolvedPathInBody::Invalid,
                        |res| self.resolve_path_late(res),
                    );

                match resolved {
                    ResolvedPathInBody::Variant(..) => resolved,
                    ResolvedPathInBody::Ty(ref ty) if ty.is_record(self.tc.db) => resolved,
                    _ => {
                        if let Some(ident) = self.path.ident(hir_db).to_opt() {
                            // xxx generic args?
                            ResolvedPathInBody::NewBinding(ident)
                        } else {
                            resolved
                        }
                    }
                }
            }
        }
    }

    // xxx only used for single-segment path in `ResolutionMode::ExprValue`
    fn resolve_ident_expr(&mut self, ident: IdentId<'db>) -> ResolvedPathInBody<'db> {
        let mut current_idx = self.tc.env.current_block_idx();
        loop {
            let env = self.tc.env.get_block(current_idx);
            if let Some(binding) = env.lookup_var(ident) {
                return ResolvedPathInBody::Binding(ident, binding);
            }

            let scope = env.scope;
            let directive = QueryDirective::new().disallow_lex();
            let query = EarlyNameQueryId::new(self.tc.db, ident, scope, directive);
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

        let query = EarlyNameQueryId::new(
            self.tc.db,
            ident,
            self.tc.body().scope(),
            QueryDirective::default(),
        );
        let bucket = resolve_query(self.tc.db, query);

        let resolved = self.resolve_bucket(bucket);
        match resolved {
            ResolvedPathInBody::Invalid => ResolvedPathInBody::NewBinding(ident),
            resolved => resolved,
        }
    }

    fn resolve_path_late(&mut self, early: EarlyResolvedPath<'db>) -> ResolvedPathInBody<'db> {
        match early {
            EarlyResolvedPath::Full(bucket) => self.resolve_bucket(bucket),

            // Try to resolve the partially resolved path as an enum variant.
            EarlyResolvedPath::Partial { path, res } => self.resolve_partial(res, path),
        }
    }

    fn resolve_partial(
        &mut self,
        res: NameRes<'db>,
        resolved: PathId<'db>,
    ) -> ResolvedPathInBody<'db> {
        let db = self.tc.db;
        let hir_db = db.as_hir_db();

        let receiver_ty = match self.resolve_name_res(&res) {
            ResolvedPathInBody::Ty(ty) => ty,

            ResolvedPathInBody::Func(..) => {
                // If the partial path is not resolved as an enum variant or method call,
                // report an error.
                let diag = TyLowerDiag::AssocTy(self.span.clone().into());
                return ResolvedPathInBody::Diag(FuncBodyDiag::Ty(diag.into()));
            }
            ResolvedPathInBody::Const(ty) => ty,
            ResolvedPathInBody::Trait(trait_) => {
                // xxx return self.resolve_trait_partial(trait_, unresolved_from);
                todo!()
            }

            ResolvedPathInBody::Variant(_)
            | ResolvedPathInBody::Binding(_, _)
            | ResolvedPathInBody::NewBinding(..) => return ResolvedPathInBody::Invalid,

            res @ (ResolvedPathInBody::Diag(_) | ResolvedPathInBody::Invalid) => return res,
        };

        // Check the possibility of resolving the partial path as an enum variant.
        if_chain! {
            if self.path.parent(hir_db) == Some(resolved);
            if let Some(adt_ref) = receiver_ty.adt_ref(db);
            if let AdtRef::Enum(enum_) = adt_ref.data(db);
            let scope = enum_.scope();
            if let Some(early) = resolve_path_tail_in_scope(self.tc.db, self.path, scope);
            if let resolved @ ResolvedPathInBody::Variant(..) = self.resolve_path_late(early);
            then {
                return resolved;
            }
        }

        if self.path.parent(hir_db) == Some(resolved) {
            let Some(name) = self.path.ident(hir_db).to_opt() else {
                return ResolvedPathInBody::Invalid;
            };
            // xxx generic args
            if let Some(resolved) = self.select_method_candidate(receiver_ty, name) {
                return resolved;
            }
        }

        // If the partial path is not resolved as an enum variant or method call,
        // report an error.
        let diag = TyLowerDiag::AssocTy(self.span.clone().into());
        ResolvedPathInBody::Diag(FuncBodyDiag::Ty(diag.into()))
    }

    fn resolve_trait_partial(
        &mut self,
        trait_: TraitDef<'db>,
        unresolved_from: usize,
    ) -> ResolvedPathInBody<'db> {
        let hir_db = self.tc.db.as_hir_db();

        if unresolved_from + 1 != self.path.len(hir_db) {
            let diag = TyLowerDiag::AssocTy(self.span.clone().into());
            return ResolvedPathInBody::Diag(FuncBodyDiag::Ty(diag.into()));
        }

        let Some(name) = self.path.ident(hir_db).to_opt() else {
            return ResolvedPathInBody::Invalid;
        };

        // xxx generic args
        let Some(trait_method) = trait_.methods(self.tc.db).get(&name) else {
            let span = self.span.segment(unresolved_from).into();
            let diag = BodyDiag::method_not_found(self.tc.db, span, name, Either::Right(trait_));
            return ResolvedPathInBody::Diag(diag.into());
        };

        let ty = TyId::func(self.tc.db, trait_method.0);
        ResolvedPathInBody::Func(self.tc.table.instantiate_to_term(ty))
    }

    fn select_method_candidate(
        &mut self,
        receiver_ty: TyId<'db>,
        name: IdentId<'db>,
    ) -> Option<ResolvedPathInBody<'db>> {
        let db = self.tc.db;
        let hir_db = self.tc.db.as_hir_db();

        let canonical_r_ty = Canonicalized::new(db, receiver_ty);
        let candidate = match select_method_candidate(
            db,
            (canonical_r_ty.value, self.span.clone().into()),
            (name, self.span.segment(self.path.len(hir_db) - 1).into()),
            self.tc.env.scope(),
            self.tc.env.assumptions(),
        ) {
            Ok(candidate) => candidate,
            Err(diag) => return Some(ResolvedPathInBody::Diag(diag)),
        };

        let trait_cand = match candidate {
            Candidate::InherentMethod(func_def) => {
                let mut method_ty = TyId::func(db, func_def);
                for &arg in receiver_ty.generic_args(db) {
                    method_ty = TyId::app(db, method_ty, arg);
                }

                return Some(ResolvedPathInBody::Func(
                    self.tc.table.instantiate_to_term(method_ty),
                ));
            }

            Candidate::TraitMethod(cand) | Candidate::NeedsConfirmation(cand) => cand,
        };

        let method = trait_cand.method;
        let inst = canonical_r_ty.extract_solution(&mut self.tc.table, trait_cand.inst);

        if matches!(candidate, Candidate::NeedsConfirmation(_)) {
            self.tc
                .env
                .register_confirmation(inst, self.span.clone().into());
        }

        let method_ty = method.instantiate_with_inst(&mut self.tc.table, receiver_ty, inst);
        Some(ResolvedPathInBody::Func(
            self.tc.table.instantiate_to_term(method_ty),
        ))
    }

    fn resolve_bucket(&mut self, bucket: &NameResBucket<'db>) -> ResolvedPathInBody<'db> {
        match self.mode {
            ResolutionMode::ExprValue => {
                match bucket.pick(NameDomain::VALUE) {
                    Ok(res) => self.resolve_name_res(res),
                    Err(_) => {
                        if let Ok(res) = bucket.pick(NameDomain::TYPE) {
                            self.resolve_name_res(res)
                        } else {
                            // This error is already reported in the name resolution phase.
                            ResolvedPathInBody::Invalid
                        }
                    }
                }
            }

            ResolutionMode::RecordInit | ResolutionMode::Pat => {
                if let Ok(res) = bucket.pick(NameDomain::VALUE) {
                    let res = self.resolve_name_res(res);
                    if !matches!(
                        res,
                        ResolvedPathInBody::Diag(_) | ResolvedPathInBody::Invalid
                    ) {
                        return res;
                    }
                }

                match bucket.pick(NameDomain::TYPE) {
                    Ok(res) => self.resolve_name_res(res),
                    Err(_) => ResolvedPathInBody::Invalid,
                }
            }
        }
    }

    fn resolve_name_res(&mut self, res: &NameRes<'db>) -> ResolvedPathInBody<'db> {
        let db = self.tc.db;
        let hir_db = db.as_hir_db();
        match res.kind {
            NameResKind::Scope(ScopeId::Item(ItemKind::Struct(struct_))) => {
                let adt = lower_adt(db, AdtRefId::from_struct(db, struct_));
                let ty = TyId::adt(db, adt);
                ResolvedPathInBody::Ty(self.tc.table.instantiate_to_term(ty))
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Enum(enum_))) => {
                let adt = lower_adt(db, AdtRefId::from_enum(db, enum_));
                let ty = TyId::adt(db, adt);
                ResolvedPathInBody::Ty(self.tc.table.instantiate_to_term(ty))
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Contract(contract_))) => {
                let adt = lower_adt(db, AdtRefId::from_contract(db, contract_));
                let ty = TyId::adt(db, adt);
                ResolvedPathInBody::Ty(self.tc.table.instantiate_to_term(ty))
            }

            NameResKind::Scope(ScopeId::Variant(parent, idx)) => {
                let enum_: Enum = parent.try_into().unwrap();
                let variant_def = &enum_.variants(hir_db).data(hir_db)[idx];
                if self.mode == ResolutionMode::ExprValue
                    && matches!(variant_def.kind, HirVariantKind::Tuple(_))
                {
                    let adt_ref = AdtRefId::new(db, enum_.into());
                    let receiver_ty = self
                        .tc
                        .table
                        .instantiate_to_term(TyId::adt(db, lower_adt(db, adt_ref)));
                    let name = variant_def.name.to_opt().unwrap();
                    self.select_method_candidate(receiver_ty, name).unwrap()
                } else {
                    ResolvedPathInBody::Variant(ResolvedVariant::new(
                        db,
                        &mut self.tc.table,
                        enum_,
                        idx,
                        self.path,
                    ))
                }
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Func(func))) => {
                let func_def = lower_func(db, func).unwrap();
                let ty = TyId::func(db, func_def);

                ResolvedPathInBody::Func(self.tc.table.instantiate_to_term(ty))
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Impl(impl_))) => {
                match impl_.ty(hir_db).to_opt() {
                    Some(hir_ty) => {
                        let ty = lower_hir_ty(db, hir_ty, self.tc.env.scope());
                        let ty = self.tc.table.instantiate_to_term(ty);
                        ResolvedPathInBody::Ty(ty)
                    }

                    None => ResolvedPathInBody::Invalid,
                }
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Trait(trait_))) => {
                ResolvedPathInBody::Trait(lower_trait(db, trait_))
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::ImplTrait(impl_trait))) => {
                match impl_trait.ty(hir_db).to_opt() {
                    Some(hir_ty) => {
                        let ty = lower_hir_ty(db, hir_ty, self.tc.env.scope());
                        let ty = self.tc.table.instantiate_to_term(ty);
                        ResolvedPathInBody::Ty(ty)
                    }

                    None => ResolvedPathInBody::Invalid,
                }
            }

            NameResKind::Scope(ScopeId::Item(ItemKind::Const(const_))) => {
                let ty = if let Some(ty) = const_.ty(hir_db).to_opt() {
                    lower_hir_ty(db, ty, self.tc.env.scope())
                } else {
                    TyId::invalid(db, InvalidCause::Other)
                };

                ResolvedPathInBody::Const(ty)
            }

            NameResKind::Scope(ScopeId::GenericParam(parent, idx)) => {
                let owner = GenericParamOwnerId::from_item_opt(db, parent).unwrap();
                let param_set = collect_generic_params(db, owner);
                let Some(ty) = param_set.param_by_original_idx(db, idx) else {
                    return ResolvedPathInBody::Invalid;
                };

                ResolvedPathInBody::Ty(self.tc.table.instantiate_to_term(ty))
            }

            NameResKind::Prim(prim) => {
                let ty = TyId::from_hir_prim_ty(db, prim);
                ResolvedPathInBody::Ty(self.tc.table.instantiate_to_term(ty))
            }

            _ => ResolvedPathInBody::Invalid,
        }
    }
}

#[derive(Clone, Debug)]
pub(super) enum ResolvedPathInBody<'db> {
    Ty(TyId<'db>),

    /// The path is resolved to function.
    /// In case of the function is associated functions, the function type is
    /// automatically instantiated in the in the process of path resolution.
    Func(TyId<'db>),
    Trait(TraitDef<'db>),
    Const(TyId<'db>),
    Variant(ResolvedVariant<'db>),
    Binding(IdentId<'db>, LocalBinding<'db>),
    NewBinding(IdentId<'db>),
    Diag(FuncBodyDiag<'db>),
    Invalid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum ResolutionMode {
    ExprValue,
    RecordInit,
    Pat,
}

pub(super) struct RecordInitChecker<'tc, 'db, 'a, T> {
    pub(super) tc: &'tc mut TyChecker<'db>,
    data: &'a mut T,
    already_given: FxHashMap<IdentId<'db>, DynLazySpan<'db>>,
    invalid_field_given: bool,
}

impl<'tc, 'db, 'a, T> RecordInitChecker<'tc, 'db, 'a, T>
where
    T: RecordLike<'db>,
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
        label: Option<IdentId<'db>>,
        field_span: DynLazySpan<'db>,
    ) -> Result<TyId<'db>, FuncBodyDiag<'db>> {
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
        initializer_span: DynLazySpan<'db>,
        allow_missing_field: bool,
    ) -> Result<(), FuncBodyDiag<'db>> {
        if !self.invalid_field_given && !allow_missing_field {
            let expected_labels = self.data.record_labels(self.tc.db);
            let found = self.already_given.keys().copied().collect::<FxHashSet<_>>();
            let missing_fields: IndexSet<IdentId> =
                expected_labels.difference(&found).copied().collect();

            if !missing_fields.is_empty() {
                let diag = BodyDiag::MissingRecordFields {
                    primary: initializer_span,
                    missing_fields: missing_fields.into_iter().collect(),
                    hint: self.data.initializer_hint(self.tc.db),
                };

                return Err(diag.into());
            }
        }

        Ok(())
    }
}

pub(crate) trait RecordLike<'db> {
    fn is_record(&self, db: &'db dyn HirAnalysisDb) -> bool;

    fn record_field_ty(&self, db: &'db dyn HirAnalysisDb, name: IdentId<'db>) -> Option<TyId<'db>>;

    fn record_field_list(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(HirFieldDefListId<'db>, &'db AdtField<'db>)>;

    fn record_field_idx(&self, db: &'db dyn HirAnalysisDb, name: IdentId<'db>) -> Option<usize> {
        let (hir_field_list, _) = self.record_field_list(db)?;
        hir_field_list.field_idx(db.as_hir_db(), name)
    }

    fn record_field_scope(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<ScopeId<'db>>;

    fn record_labels(&self, db: &'db dyn HirAnalysisDb) -> FxHashSet<IdentId<'db>>;

    fn initializer_hint(&self, db: &'db dyn HirAnalysisDb) -> Option<String>;

    fn kind_name(&self, db: &'db dyn HirAnalysisDb) -> String;
}

impl<'db> RecordLike<'db> for TyId<'db> {
    fn is_record(&self, db: &'db dyn HirAnalysisDb) -> bool {
        let Some(adt_ref) = self.adt_ref(db) else {
            return false;
        };

        matches!(adt_ref.data(db), AdtRef::Struct(..))
    }

    fn record_field_ty(&self, db: &'db dyn HirAnalysisDb, name: IdentId<'db>) -> Option<TyId<'db>> {
        let args = self.generic_args(db);
        let hir_db = db.as_hir_db();

        let (hir_field_list, field_list) = self.record_field_list(db)?;

        let field_idx = hir_field_list.field_idx(hir_db, name)?;
        let field_ty = field_list.ty(db, field_idx).instantiate(db, args);

        if field_ty.is_star_kind(db) {
            field_ty
        } else {
            TyId::invalid(db, InvalidCause::Other)
        }
        .into()
    }

    fn record_field_list(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(HirFieldDefListId<'db>, &'db AdtField<'db>)> {
        let hir_db = db.as_hir_db();

        let adt_def = self.adt_def(db)?;
        match adt_def.adt_ref(db).data(db) {
            AdtRef::Struct(s) => (s.fields(hir_db), &adt_def.fields(db)[0]).into(),
            AdtRef::Contract(c) => (c.fields(hir_db), &adt_def.fields(db)[0]).into(),

            _ => None,
        }
    }

    fn record_field_scope(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<ScopeId<'db>> {
        let field_idx = self.record_field_idx(db, name)?;
        let adt_ref = self.adt_ref(db)?;

        let parent = FieldParent::Item(adt_ref.as_item(db));
        Some(ScopeId::Field(parent, field_idx))
    }

    fn record_labels(&self, db: &'db dyn HirAnalysisDb) -> FxHashSet<IdentId<'db>> {
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

    fn kind_name(&self, db: &'db dyn HirAnalysisDb) -> String {
        if let Some(adt_ref) = self.adt_ref(db) {
            adt_ref.kind_name(db).to_string()
        } else if self.is_func(db) {
            "fn".to_string()
        } else {
            self.pretty_print(db).to_string()
        }
    }

    fn initializer_hint(&self, db: &'db dyn HirAnalysisDb) -> Option<String> {
        let hir_db = db.as_hir_db();

        if self.adt_ref(db).is_some() {
            let AdtRef::Struct(s) = self.adt_ref(db)?.data(db) else {
                return None;
            };

            let name = s.name(hir_db).unwrap().data(hir_db);
            let init_args = s.format_initializer_args(db.as_hir_db());
            Some(format!("{}{}", name, init_args))
        } else {
            None
        }
    }
}

impl<'db> RecordLike<'db> for ResolvedVariant<'db> {
    fn is_record(&self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.variant_kind(db), HirVariantKind::Record(..))
    }

    fn record_field_ty(&self, db: &'db dyn HirAnalysisDb, name: IdentId<'db>) -> Option<TyId<'db>> {
        let args = self.ty(db).generic_args(db);
        let hir_db = db.as_hir_db();

        let (hir_field_list, field_list) = self.record_field_list(db)?;
        let field_idx = hir_field_list.field_idx(hir_db, name)?;

        Some(field_list.ty(db, field_idx).instantiate(db, args))
    }

    fn record_field_list(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(HirFieldDefListId<'db>, &'db AdtField<'db>)> {
        match self.variant_kind(db) {
            hir::hir_def::VariantKind::Record(fields) => {
                (fields, &self.adt_def(db).fields(db)[self.idx]).into()
            }

            _ => None,
        }
    }

    fn record_field_scope(
        &self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
    ) -> Option<ScopeId<'db>> {
        let field_idx = self.record_field_idx(db, name)?;
        let parent = FieldParent::Variant(self.enum_.into(), self.idx);
        Some(ScopeId::Field(parent, field_idx))
    }

    fn record_labels(&self, db: &'db dyn HirAnalysisDb) -> FxHashSet<IdentId<'db>> {
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

    fn kind_name(&self, db: &'db dyn HirAnalysisDb) -> String {
        let hir_db = db.as_hir_db();
        match self.enum_.variants(hir_db).data(hir_db)[self.idx].kind {
            HirVariantKind::Unit => "unit variant",
            HirVariantKind::Tuple(_) => "tuple variant",
            HirVariantKind::Record(_) => "record variant",
        }
        .to_string()
    }

    fn initializer_hint(&self, db: &'db dyn HirAnalysisDb) -> Option<String> {
        let hir_db = db.as_hir_db();
        let expected_sub_pat =
            self.enum_.variants(hir_db).data(hir_db)[self.idx].format_initializer_args(hir_db);

        let path = self.path.pretty_print(hir_db);
        Some(format!("{}{}", path, expected_sub_pat))
    }
}
