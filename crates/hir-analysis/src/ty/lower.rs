use either::Either;
use hir::{
    hir_def::{
        scope_graph::ScopeId, FieldDefListId, GenericArg, GenericArgListId, GenericParam,
        GenericParamOwner, IdentId, ItemKind, KindBound as HirKindBound, Partial, PathId,
        TypeAlias as HirTypeAlias, TypeId as HirTyId, TypeKind as HirTyKind, VariantDefListId,
        WherePredicate,
    },
    visitor::prelude::*,
};

use crate::{
    name_resolution::{resolve_path_early, EarlyResolvedPath, NameDomain, NameResKind},
    ty::{
        diagnostics::{GenericParamDiagAccumulator, TyLowerDiag, TypeAliasDefDiagAccumulator},
        visitor::TyDiagCollector,
    },
    HirAnalysisDb,
};

use super::ty::{
    AdtDef, AdtField, AdtRef, AdtRefId, InvalidCause, Kind, Subst, TyData, TyId, TyParam,
};

#[salsa::tracked]
pub fn lower_hir_ty(db: &dyn HirAnalysisDb, ty: HirTyId, scope: ScopeId) -> TyId {
    TyBuilder::new(db, scope).lower_ty(ty)
}

#[salsa::tracked]
pub fn lower_adt(db: &dyn HirAnalysisDb, adt: AdtRefId) -> AdtDef {
    AdtTyBuilder::new(db, adt).build()
}

#[salsa::tracked]
pub(crate) fn collect_generic_params(
    db: &dyn HirAnalysisDb,
    owner: GenericParamOwnerId,
) -> GenericParamTypeSet {
    let (set, diags) = GenericParamCollector::new(db, owner.data(db)).finalize();
    diags.into_iter().for_each(|diag| {
        GenericParamDiagAccumulator::push(db, diag);
    });

    set
}

#[salsa::tracked(return_ref, recovery_fn = recover_lower_type_alias_cycle)]
pub(crate) fn lower_type_alias(db: &dyn HirAnalysisDb, alias: HirTypeAlias) -> TyAlias {
    let params = collect_generic_params(db, GenericParamOwnerId::new(db, alias.into()));

    let Some(hir_ty) = alias.ty(db.as_hir_db()).to_opt() else {
        return TyAlias {
            alias,
            alias_to: TyId::invalid(db, InvalidCause::Other),
            params: params.params,
        };
    };

    let ty = lower_hir_ty(db, hir_ty, alias.scope());
    let alias_to = if ty.is_mono_type(db) {
        let collector = TyDiagCollector::new(db, alias.scope());
        let diags = collector.collect(hir_ty, alias.lazy_span().ty());
        if diags.is_empty() {
            ty
        } else {
            diags.into_iter().for_each(|diag| {
                TypeAliasDefDiagAccumulator::push(db, diag);
            });
            TyId::invalid(db, InvalidCause::Other)
        }
    } else {
        TypeAliasDefDiagAccumulator::push(
            db,
            TyLowerDiag::not_fully_applied_type(alias.lazy_span().ty().into()),
        );
        TyId::invalid(db, InvalidCause::Other)
    };

    TyAlias {
        alias,
        alias_to,
        params: params.params,
    }
}

fn recover_lower_type_alias_cycle(
    db: &dyn HirAnalysisDb,
    _cycle: &salsa::Cycle,
    alias: HirTypeAlias,
) -> TyAlias {
    let diag = TyLowerDiag::type_alias_cycle(alias.lazy_span().ty().into());
    TypeAliasDefDiagAccumulator::push(db, diag);

    let alias_to = TyId::invalid(db, InvalidCause::Other);
    let params = collect_generic_params(db, GenericParamOwnerId::new(db, alias.into()));
    TyAlias {
        alias,
        alias_to,
        params: params.params,
    }
}

/// Represents a lowered type alias. `TyAlias` itself isn't a type, but
/// can be instantiated to a `TyId` by substituting its type
/// parameters with actual types.
///
/// NOTE: `TyAlias` can't become an alias to partial applied types, i.e., the
/// right hand side of the alias declaration must be a fully applied type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TyAlias {
    alias: HirTypeAlias,
    alias_to: TyId,
    params: Vec<TyId>,
}

impl TyAlias {
    fn apply_subst(&self, db: &dyn HirAnalysisDb, arg_tys: &[TyId]) -> TyId {
        if arg_tys.len() != self.params.len() {
            return TyId::invalid(
                db,
                InvalidCause::TypeAliasArgumentMismatch {
                    alias: self.alias,
                    n_given_args: arg_tys.len(),
                },
            );
        }
        let mut subst = Subst::new();

        for (&param, &arg) in self.params.iter().zip(arg_tys.iter()) {
            let arg = if param.kind(db) != arg.kind(db) {
                TyId::invalid(
                    db,
                    InvalidCause::kind_mismatch(param.kind(db).into(), arg.kind(db)),
                )
            } else {
                arg
            };
            subst.insert(db, param, arg);
        }

        self.alias_to.apply_subst(db, &subst)
    }
}

struct TyBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId,
}

impl<'db> TyBuilder<'db> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, scope: ScopeId) -> Self {
        Self { db, scope }
    }

    pub(super) fn lower_ty(&mut self, ty: HirTyId) -> TyId {
        match ty.data(self.db.as_hir_db()) {
            HirTyKind::Ptr(pointee) => self.lower_ptr(*pointee),

            HirTyKind::Path(path, args) => self.lower_path(*path, *args),

            HirTyKind::SelfType(args) => self.lower_self_ty(*args),

            HirTyKind::Tuple(elems) => self.lower_tuple(elems),

            HirTyKind::Array(_, _) => {
                todo!()
            }
        }
    }

    pub(super) fn lower_path(&mut self, path: Partial<PathId>, args: GenericArgListId) -> TyId {
        let path_ty = path
            .to_opt()
            .map(|path| match self.resolve_path(path) {
                Either::Left(res) => self.lower_resolved_path(res),
                Either::Right(ty) => Either::Left(ty),
            })
            .unwrap_or_else(|| Either::Left(TyId::invalid(self.db, InvalidCause::Other)));

        let arg_tys: Vec<_> = args
            .data(self.db.as_hir_db())
            .iter()
            .map(|arg| self.lower_generic_arg(arg))
            .collect();

        match path_ty {
            Either::Left(ty) => arg_tys
                .into_iter()
                .fold(ty, |acc, arg| TyId::app(self.db, acc, arg)),

            Either::Right(alias) => alias.apply_subst(self.db, &arg_tys),
        }
    }

    pub(super) fn lower_self_ty(&mut self, args: GenericArgListId) -> TyId {
        let res = self.resolve_path(PathId::self_ty(self.db.as_hir_db()));
        let (scope, res) = match res {
            Either::Left(res @ NameResKind::Scope(scope)) => (scope, res),
            Either::Left(NameResKind::Prim(prim)) => return TyId::from_hir_prim_ty(self.db, prim),
            Either::Right(ty) => return ty,
        };

        let (target_hir_ty, target_scope) = match scope {
            ScopeId::Item(item) => match item {
                ItemKind::Enum(_) | ItemKind::Struct(_) | ItemKind::Contract(_) => {
                    return self.lower_resolved_path(res).unwrap_left()
                }

                ItemKind::Trait(trait_) => {
                    let params = collect_generic_params(
                        self.db,
                        GenericParamOwnerId::new(self.db, trait_.into()),
                    );
                    return params.trait_self.unwrap();
                }

                ItemKind::Impl(impl_) => (impl_.ty(self.db.as_hir_db()), impl_.scope()),
                ItemKind::ImplTrait(impl_trait) => {
                    (impl_trait.ty(self.db.as_hir_db()), impl_trait.scope())
                }
                _ => return TyId::invalid(self.db, InvalidCause::Other),
            },

            _ => unreachable!(),
        };

        let target_ty = target_hir_ty
            .to_opt()
            .map(|hir_ty| lower_hir_ty(self.db, hir_ty, target_scope))
            .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other));

        let db = self.db;
        args.data(self.db.as_hir_db())
            .iter()
            .map(|arg| self.lower_generic_arg(arg))
            .fold(target_ty, |acc, arg| TyId::app(db, acc, arg))
    }

    fn lower_ptr(&mut self, pointee: Partial<HirTyId>) -> TyId {
        let pointee = pointee
            .to_opt()
            .map(|pointee| lower_hir_ty(self.db, pointee, self.scope))
            .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other));

        let ptr = TyId::ptr(self.db);
        TyId::app(self.db, ptr, pointee)
    }

    fn lower_tuple(&mut self, elems: &[Partial<HirTyId>]) -> TyId {
        let len = elems.len();
        let tuple = TyId::tuple(self.db, len);
        elems.iter().fold(tuple, |acc, elem| {
            let elem_ty = elem
                .to_opt()
                .map(|elem| lower_hir_ty(self.db, elem, self.scope))
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other));
            if !elem_ty.is_mono_type(self.db) {
                return TyId::invalid(self.db, InvalidCause::NotFullyApplied);
            }

            TyId::app(self.db, acc, elem_ty)
        })
    }

    fn lower_resolved_path(&mut self, kind: NameResKind) -> Either<TyId, &'db TyAlias> {
        let scope = match kind {
            NameResKind::Scope(scope) => scope,
            NameResKind::Prim(prim_ty) => {
                return Either::Left(TyId::from_hir_prim_ty(self.db, prim_ty))
            }
        };

        let item = match scope {
            ScopeId::Item(item) => item,
            ScopeId::GenericParam(item, idx) => {
                let owner = GenericParamOwner::from_item_opt(item).unwrap();
                let owner_id = GenericParamOwnerId::new(self.db, owner);

                let params = collect_generic_params(self.db, owner_id);
                return Either::Left(params.params[idx]);
            }
            _ => unreachable!(),
        };

        match item {
            ItemKind::Enum(enum_) => {
                let adt_ref = AdtRefId::from_enum(self.db, enum_);
                let adt = lower_adt(self.db, adt_ref);
                Either::Left(TyId::adt(self.db, adt))
            }
            ItemKind::Struct(struct_) => {
                let adt_ref = AdtRefId::from_struct(self.db, struct_);
                let adt = lower_adt(self.db, adt_ref);
                Either::Left(TyId::adt(self.db, adt))
            }
            ItemKind::Contract(contract) => {
                let adt_ref = AdtRefId::from_contract(self.db, contract);
                let adt = lower_adt(self.db, adt_ref);
                Either::Left(TyId::adt(self.db, adt))
            }
            ItemKind::TypeAlias(alias) => Either::Right(lower_type_alias(self.db, alias)),
            // This should be handled in the name resolution.
            _ => Either::Left(TyId::invalid(self.db, InvalidCause::Other)),
        }
    }

    fn lower_generic_arg(&mut self, arg: &GenericArg) -> TyId {
        match arg {
            GenericArg::Type(ty_arg) => ty_arg
                .ty
                .to_opt()
                .map(|ty| lower_hir_ty(self.db, ty, self.scope))
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),

            GenericArg::Const(_) => todo!(),
        }
    }

    /// If the path is resolved to a type, return the resolution. Otherwise,
    /// returns the `TyId::Invalid` with proper `InvalidCause`.
    fn resolve_path(&mut self, path: PathId) -> Either<NameResKind, TyId> {
        match resolve_path_early(self.db, path, self.scope) {
            EarlyResolvedPath::Full(bucket) => match bucket.pick(NameDomain::Type) {
                Ok(res) => Either::Left(res.kind),

                // This error is already handled by the name resolution.
                Err(_) => Either::Right(TyId::invalid(self.db, InvalidCause::Other)),
            },

            EarlyResolvedPath::Partial { .. } => {
                Either::Right(TyId::invalid(self.db, InvalidCause::AssocTy))
            }
        }
    }
}

struct AdtTyBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    adt: AdtRefId,
    params: Vec<TyId>,
    variants: Vec<AdtField>,
}

impl<'db> AdtTyBuilder<'db> {
    fn new(db: &'db dyn HirAnalysisDb, adt: AdtRefId) -> Self {
        Self {
            db,
            adt,
            params: Vec::new(),
            variants: Vec::new(),
        }
    }

    fn build(mut self) -> AdtDef {
        self.collect_generic_params();
        self.collect_variants();
        AdtDef::new(self.db, self.adt, self.params, self.variants)
    }

    fn collect_generic_params(&mut self) {
        let owner = match self.adt.data(self.db) {
            AdtRef::Contract(_) => return,
            AdtRef::Enum(enum_) => enum_.into(),
            AdtRef::Struct(struct_) => struct_.into(),
        };
        let owner_id = GenericParamOwnerId::new(self.db, owner);

        self.params = collect_generic_params(self.db, owner_id).params.clone();
    }

    fn collect_variants(&mut self) {
        match self.adt.data(self.db) {
            AdtRef::Struct(struct_) => {
                self.collect_field_types(struct_.fields(self.db.as_hir_db()));
            }

            AdtRef::Contract(contract) => {
                self.collect_field_types(contract.fields(self.db.as_hir_db()))
            }

            AdtRef::Enum(enum_) => {
                self.collect_enum_variant_types(enum_.variants(self.db.as_hir_db()))
            }
        };
    }

    fn collect_field_types(&mut self, fields: FieldDefListId) {
        let scope = self.adt.scope(self.db);

        fields.data(self.db.as_hir_db()).iter().for_each(|field| {
            let variant = AdtField::new(field.name, vec![field.ty], scope);
            self.variants.push(variant);
        })
    }

    fn collect_enum_variant_types(&mut self, variants: VariantDefListId) {
        let scope = self.adt.scope(self.db);

        variants
            .data(self.db.as_hir_db())
            .iter()
            .for_each(|variant| {
                // TODO: FIX here when record variant is introduced.
                let tys = match variant.ty {
                    Some(ty) => {
                        vec![Some(ty).into()]
                    }
                    None => vec![],
                };

                let variant = AdtField::new(variant.name, tys, scope);
                self.variants.push(variant)
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct GenericParamTypeSet {
    pub(crate) params: Vec<TyId>,
    pub(crate) trait_self: Option<TyId>,
}

struct GenericParamCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    parent: GenericParamOwner,
    params: Vec<TyParamPrecursor>,
    /// The self type of the trait.
    trait_self: TyParamPrecursor,
    current_idx: ParamLoc,
    diags: Vec<TyLowerDiag>,
}

impl<'db> GenericParamCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, parent: GenericParamOwner) -> Self {
        let trait_self = TyParamPrecursor {
            name: Partial::Absent,
            idx: None,
            kind: Kind::Star,
            kind_span: None,
        };

        Self {
            db,
            parent,
            params: Vec::new(),
            trait_self: trait_self,
            current_idx: ParamLoc::Idx(0),
            diags: Vec::new(),
        }
    }

    fn finalize(mut self) -> (GenericParamTypeSet, Vec<TyLowerDiag>) {
        let param_list = self.parent.params(self.db.as_hir_db());
        let param_list_span = self.parent.params_span();
        self.visit_generic_param_list(
            &mut VisitorCtxt::new(self.db.as_hir_db(), self.parent.scope(), param_list_span),
            param_list,
        );

        if let Some(where_clause_owner) = self.parent.where_clause_owner() {
            let where_clause = where_clause_owner.where_clause(self.db.as_hir_db());
            let where_clause_span = where_clause_owner.where_clause_span();
            self.visit_where_clause(
                &mut VisitorCtxt::new(self.db.as_hir_db(), self.parent.scope(), where_clause_span),
                where_clause,
            );
        };

        let params = self
            .params
            .into_iter()
            .map(|param| param.into_ty(self.db))
            .collect();
        let trait_self = matches!(self.parent, GenericParamOwner::Trait(_))
            .then(|| self.trait_self.into_ty(self.db));
        let params_set = GenericParamTypeSet { params, trait_self };

        (params_set, self.diags)
    }

    fn param_idx_from_ty(&self, ty: Option<HirTyId>) -> ParamLoc {
        let Some(ty) = ty else {
            return ParamLoc::NotParam;
        };

        let path = match ty.data(self.db.as_hir_db()) {
            HirTyKind::Path(Partial::Present(path), args) => {
                if args.is_empty(self.db.as_hir_db()) {
                    *path
                } else {
                    return ParamLoc::NotParam;
                }
            }

            HirTyKind::SelfType(args) => {
                return if matches!(self.parent.into(), ItemKind::Trait(_))
                    && args.is_empty(self.db.as_hir_db())
                {
                    ParamLoc::TraitSelf
                } else {
                    ParamLoc::NotParam
                };
            }

            _ => return ParamLoc::NotParam,
        };

        match resolve_path_early(self.db, path, self.parent.scope()) {
            EarlyResolvedPath::Full(bucket) => match bucket.pick(NameDomain::Type) {
                Ok(res) => match res.kind {
                    NameResKind::Scope(ScopeId::GenericParam(item, idx)) => {
                        debug_assert_eq!(item, ItemKind::from(self.parent));
                        ParamLoc::Idx(idx)
                    }
                    _ => ParamLoc::NotParam,
                },
                Err(_) => ParamLoc::NotParam,
            },

            EarlyResolvedPath::Partial { .. } => ParamLoc::NotParam,
        }
    }
}

impl<'db> Visitor for GenericParamCollector<'db> {
    fn visit_generic_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyGenericParamSpan>,
        param: &GenericParam,
    ) {
        match param {
            GenericParam::Type(param) => {
                let name = param.name;
                let idx = self.current_idx.unwrap_idx();

                let kind = Kind::Star;
                self.params.push(TyParamPrecursor {
                    name,
                    idx: Some(idx),
                    kind,
                    kind_span: None,
                });
            }

            GenericParam::Const(_) => {
                todo!()
            }
        }

        walk_generic_param(self, ctxt, param);
        self.current_idx = ParamLoc::Idx(self.current_idx.unwrap_idx() + 1);
    }

    fn visit_where_predicate(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyWherePredicateSpan>,
        pred: &WherePredicate,
    ) {
        self.current_idx = self.param_idx_from_ty(pred.ty.to_opt());
        walk_where_predicate(self, ctxt, pred)
    }

    fn visit_ty(&mut self, _: &mut VisitorCtxt<'_, LazyTySpan>, _: HirTyId) {
        // Remove `walk_ty` because 1. we don't need to visit the type and 2. we
        // want to avoid unnecessary overhead of visiting type.
    }

    fn visit_kind_bound(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyKindBoundSpan>,
        bound: &HirKindBound,
    ) {
        let kind = lower_kind(bound);
        let param = match self.current_idx {
            ParamLoc::Idx(idx) => &mut self.params[idx],
            ParamLoc::TraitSelf => &mut self.trait_self,
            ParamLoc::NotParam => {
                self.diags.push(TyLowerDiag::KindBoundNotAllowed(
                    ctxt.span().unwrap().into(),
                ));
                return;
            }
        };

        if let Some(first_defined_span) = &param.kind_span {
            if param.kind != kind {
                self.diags.push(TyLowerDiag::DuplicateKindBound(
                    ctxt.span().unwrap().into(),
                    first_defined_span.clone().into(),
                ));
            }
        } else {
            param.kind = kind;
            param.kind_span = Some(ctxt.span().unwrap().into());
        }
    }
}

struct TyParamPrecursor {
    name: Partial<IdentId>,
    idx: Option<usize>,
    kind: Kind,
    kind_span: Option<LazyKindBoundSpan>,
}

impl TyParamPrecursor {
    pub fn into_ty(self, db: &dyn HirAnalysisDb) -> TyId {
        let Partial::Present(name) = self.name else {
            return TyId::invalid(db, InvalidCause::Other);
        };

        let param = TyParam {
            name,
            idx: self.idx,
            kind: self.kind,
        };

        TyId::new(db, TyData::TyParam(param))
    }
}

fn lower_kind(kind: &HirKindBound) -> Kind {
    match kind {
        HirKindBound::Mono => Kind::Star,
        HirKindBound::Abs(lhs, rhs) => match (lhs, rhs) {
            (Partial::Present(lhs), Partial::Present(rhs)) => {
                Kind::Abs(Box::new(lower_kind(lhs)), Box::new(lower_kind(rhs)))
            }
            (Partial::Present(lhs), Partial::Absent) => {
                Kind::Abs(Box::new(lower_kind(lhs)), Box::new(Kind::Any))
            }
            (Partial::Absent, Partial::Present(rhs)) => {
                Kind::Abs(Box::new(Kind::Any), Box::new(lower_kind(rhs)))
            }
            (Partial::Absent, Partial::Absent) => {
                Kind::Abs(Box::new(Kind::Any), Box::new(Kind::Any))
            }
        },
    }
}

#[derive(Debug, Clone, Copy)]
enum ParamLoc {
    Idx(usize),
    TraitSelf,
    NotParam,
}

impl ParamLoc {
    fn unwrap_idx(&self) -> usize {
        match self {
            ParamLoc::Idx(idx) => *idx,
            _ => panic!(),
        }
    }
}

#[salsa::tracked]
pub(crate) struct GenericParamOwnerId {
    data: GenericParamOwner,
}
