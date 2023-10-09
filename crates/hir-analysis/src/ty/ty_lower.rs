use std::collections::BTreeSet;

use either::Either;
use hir::hir_def::{
    kw, scope_graph::ScopeId, FieldDefListId, GenericArg, GenericArgListId, GenericParam,
    GenericParamOwner, IdentId, IngotId, ItemKind, KindBound as HirKindBound, Partial, PathId,
    TupleTypeId, TypeAlias as HirTypeAlias, TypeBound, TypeId as HirTyId, TypeKind as HirTyKind,
    VariantDefListId, VariantKind,
};
use rustc_hash::FxHashMap;
use salsa::function::Configuration;

use crate::{
    name_resolution::{resolve_path_early, EarlyResolvedPath, NameDomain, NameResKind},
    HirAnalysisDb,
};

use super::ty_def::{
    AdtDef, AdtField, AdtRef, AdtRefId, InvalidCause, Kind, TyData, TyId, TyParam,
};

#[salsa::tracked]
pub fn lower_hir_ty(db: &dyn HirAnalysisDb, ty: HirTyId, scope: ScopeId) -> TyId {
    TyBuilder::new(db, scope).lower_ty(ty)
}

#[salsa::tracked]
pub fn lower_adt(db: &dyn HirAnalysisDb, adt: AdtRefId) -> AdtDef {
    AdtTyBuilder::new(db, adt).build()
}

#[salsa::tracked(return_ref)]
pub(crate) fn collect_generic_params(
    db: &dyn HirAnalysisDb,
    owner: GenericParamOwnerId,
) -> GenericParamTypeSet {
    GenericParamCollector::new(db, owner.data(db)).finalize()
}

#[salsa::tracked(return_ref, recovery_fn = recover_lower_type_alias_cycle)]
pub(crate) fn lower_type_alias(
    db: &dyn HirAnalysisDb,
    alias: HirTypeAlias,
) -> Result<TyAlias, AliasCycle> {
    let params = collect_generic_params(db, GenericParamOwnerId::new(db, alias.into()));

    let Some(hir_ty) = alias.ty(db.as_hir_db()).to_opt() else {
        return Ok(TyAlias {
            alias,
            alias_to: TyId::invalid(db, InvalidCause::Other),
            params: params.params.clone(),
        });
    };

    let alias_to = lower_hir_ty(db, hir_ty, alias.scope());
    let alias_to = if alias_to.contains_invalid(db) {
        TyId::invalid(db, InvalidCause::Other)
    } else {
        alias_to
    };
    Ok(TyAlias {
        alias,
        alias_to,
        params: params.params.clone(),
    })
}

fn recover_lower_type_alias_cycle(
    db: &dyn HirAnalysisDb,
    cycle: &salsa::Cycle,
    _alias: HirTypeAlias,
) -> Result<TyAlias, AliasCycle> {
    let alias_cycle = cycle
        .participant_keys()
        .filter_map(|key| {
            let ingredient_index = key.ingredient_index();
            // This is temporary fragile solution to filter out the cycle participants, the
            // correctness of this strategy is based on the fact that cycle participants are
            // only `lower_type_alias` and `lower_hir_ty` functions.
            // TODO: We can refactor here if https://github.com/salsa-rs/salsa/pull/461 is approved.
            if matches!(
                db.cycle_recovery_strategy(ingredient_index),
                salsa::cycle::CycleRecoveryStrategy::Fallback
            ) {
                Some(lower_type_alias::key_from_id(key.key_index()))
            } else {
                None
            }
        })
        .collect();
    Err(AliasCycle(alias_cycle))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct AliasCycle(BTreeSet<HirTypeAlias>);

impl AliasCycle {
    pub(super) fn representative(&self) -> HirTypeAlias {
        *self.0.iter().next().unwrap()
    }

    pub(super) fn participants(&self) -> impl Iterator<Item = HirTypeAlias> + '_ {
        self.0.iter().skip(1).copied()
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
        if arg_tys.len() < self.params.len() {
            return TyId::invalid(
                db,
                InvalidCause::UnboundTypeAliasParam {
                    alias: self.alias,
                    n_given_args: arg_tys.len(),
                },
            );
        }
        let mut subst = FxHashMap::default();

        for (&param, &arg) in self.params.iter().zip(arg_tys.iter()) {
            let arg = if param.kind(db) != arg.kind(db) {
                TyId::invalid(
                    db,
                    InvalidCause::kind_mismatch(param.kind(db).into(), arg.kind(db)),
                )
            } else {
                arg
            };
            subst.insert(param, arg);
        }

        self.alias_to.apply_subst(db, &mut subst)
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

            HirTyKind::Tuple(tuple_id) => self.lower_tuple(*tuple_id),

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

        let arg_tys = lower_generic_arg_list(self.db, args, self.scope);
        match path_ty {
            Either::Left(ty) => arg_tys
                .into_iter()
                .fold(ty, |acc, arg| TyId::app(self.db, acc, arg)),

            Either::Right(alias) => {
                if alias.alias_to.contains_invalid(self.db) {
                    return TyId::invalid(self.db, InvalidCause::Other);
                }

                let ty = alias.apply_subst(self.db, &arg_tys);
                if !ty.is_invalid(self.db) {
                    let param_num = alias.params.len();
                    arg_tys[param_num..]
                        .iter()
                        .fold(ty, |acc, arg| TyId::app(self.db, acc, *arg))
                } else {
                    ty
                }
            }
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
            .map(|arg| lower_generic_arg(self.db, arg, self.scope))
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

    fn lower_tuple(&mut self, tuple_id: TupleTypeId) -> TyId {
        let elems = tuple_id.data(self.db.as_hir_db());
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
            ItemKind::TypeAlias(alias) => match lower_type_alias(self.db, alias) {
                Ok(alias) => Either::Right(alias),
                Err(_) => Either::Left(TyId::invalid(self.db, InvalidCause::Other)),
            },
            // This should be handled in the name resolution.
            _ => Either::Left(TyId::invalid(self.db, InvalidCause::Other)),
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

pub(super) fn lower_generic_arg(db: &dyn HirAnalysisDb, arg: &GenericArg, scope: ScopeId) -> TyId {
    match arg {
        GenericArg::Type(ty_arg) => ty_arg
            .ty
            .to_opt()
            .map(|ty| lower_hir_ty(db, ty, scope))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),

        GenericArg::Const(_) => todo!(),
    }
}

pub(super) fn lower_generic_arg_list(
    db: &dyn HirAnalysisDb,
    args: GenericArgListId,
    scope: ScopeId,
) -> Vec<TyId> {
    args.data(db.as_hir_db())
        .iter()
        .map(|arg| lower_generic_arg(db, arg, scope))
        .collect()
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
                let tys = match variant.kind {
                    VariantKind::Tuple(tuple_id) => tuple_id.data(self.db.as_hir_db()).clone(),

                    VariantKind::Record(fields) => fields
                        .data(self.db.as_hir_db())
                        .iter()
                        .map(|field| field.ty)
                        .collect(),

                    VariantKind::Unit => vec![],
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
    owner: GenericParamOwner,
    params: Vec<TyParamPrecursor>,
    /// The self type of the trait.
    trait_self: TyParamPrecursor,
}

impl<'db> GenericParamCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, owner: GenericParamOwner) -> Self {
        let trait_self = TyParamPrecursor {
            name: Partial::Present(kw::SELF_TY),
            idx: None,
            kind: None,
        };

        Self {
            db,
            owner,
            params: Vec::new(),
            trait_self,
        }
    }

    fn collect_generic_params(&mut self) {
        let hir_db = self.db.as_hir_db();
        let param_list = self.owner.params(hir_db);
        for (idx, param) in param_list.data(hir_db).iter().enumerate() {
            match param {
                GenericParam::Type(param) => {
                    let name = param.name;

                    let kind = self.find_kind_from_bound(param.bounds.as_slice());
                    self.params.push(TyParamPrecursor {
                        name,
                        idx: Some(idx),
                        kind,
                    });
                }

                GenericParam::Const(_) => {
                    todo!()
                }
            }
        }
    }

    fn collect_kind_in_where_clause(&mut self) {
        let Some(where_clause_owner) = self.owner.where_clause_owner() else {
            return;
        };

        let hir_db = self.db.as_hir_db();
        let where_clause = where_clause_owner.where_clause(hir_db);
        for pred in where_clause.data(hir_db) {
            match self.param_idx_from_ty(pred.ty.to_opt()) {
                ParamLoc::Idx(idx) => {
                    if self.params[idx].kind.is_none() {
                        self.params[idx].kind = self.find_kind_from_bound(pred.bounds.as_slice());
                    }
                }

                ParamLoc::TraitSelf => {
                    if self.trait_self.kind.is_none() {
                        self.trait_self.kind = self.find_kind_from_bound(pred.bounds.as_slice());
                    }
                }

                ParamLoc::NonParam => {}
            };
        }
    }

    fn finalize(mut self) -> GenericParamTypeSet {
        self.collect_generic_params();
        self.collect_kind_in_where_clause();

        let params = self
            .params
            .into_iter()
            .map(|param| param.into_ty(self.db))
            .collect();
        let trait_self = matches!(self.owner, GenericParamOwner::Trait(_))
            .then(|| self.trait_self.into_ty(self.db));

        GenericParamTypeSet { params, trait_self }
    }

    fn find_kind_from_bound(&self, bounds: &[TypeBound]) -> Option<Kind> {
        for bound in bounds {
            if let TypeBound::Kind(Partial::Present(k)) = bound {
                return Some(lower_kind(k));
            }
        }

        None
    }

    fn param_idx_from_ty(&self, ty: Option<HirTyId>) -> ParamLoc {
        let Some(ty) = ty else {
            return ParamLoc::NonParam;
        };

        let path = match ty.data(self.db.as_hir_db()) {
            HirTyKind::Path(Partial::Present(path), args) => {
                if args.is_empty(self.db.as_hir_db()) {
                    *path
                } else {
                    return ParamLoc::NonParam;
                }
            }

            HirTyKind::SelfType(args) => {
                return if matches!(self.owner.into(), ItemKind::Trait(_))
                    && args.is_empty(self.db.as_hir_db())
                {
                    ParamLoc::TraitSelf
                } else {
                    ParamLoc::NonParam
                };
            }

            _ => return ParamLoc::NonParam,
        };

        match resolve_path_early(self.db, path, self.owner.scope()) {
            EarlyResolvedPath::Full(bucket) => match bucket.pick(NameDomain::Type) {
                Ok(res) => match res.kind {
                    NameResKind::Scope(ScopeId::GenericParam(item, idx)) => {
                        debug_assert_eq!(item, ItemKind::from(self.owner));
                        ParamLoc::Idx(idx)
                    }
                    _ => ParamLoc::NonParam,
                },
                Err(_) => ParamLoc::NonParam,
            },

            EarlyResolvedPath::Partial { .. } => ParamLoc::NonParam,
        }
    }
}

enum ParamLoc {
    TraitSelf,
    Idx(usize),
    NonParam,
}

#[derive(Debug)]
struct TyParamPrecursor {
    name: Partial<IdentId>,
    idx: Option<usize>,
    kind: Option<Kind>,
}

impl TyParamPrecursor {
    pub fn into_ty(self, db: &dyn HirAnalysisDb) -> TyId {
        let Partial::Present(name) = self.name else {
            return TyId::invalid(db, InvalidCause::Other);
        };

        let param = TyParam {
            name,
            idx: self.idx,
            kind: self.kind.unwrap_or(Kind::Star),
        };

        TyId::new(db, TyData::TyParam(param))
    }
}

pub(super) fn lower_kind(kind: &HirKindBound) -> Kind {
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

#[salsa::interned]
pub(crate) struct GenericParamOwnerId {
    pub(super) data: GenericParamOwner,
}

impl GenericParamOwnerId {
    pub(super) fn scope(self, db: &dyn HirAnalysisDb) -> ScopeId {
        self.data(db).scope()
    }

    pub(super) fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        self.data(db).top_mod(db.as_hir_db()).ingot(db.as_hir_db())
    }
}
