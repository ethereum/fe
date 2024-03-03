use std::collections::BTreeSet;

use either::Either;
use hir::hir_def::{
    kw, scope_graph::ScopeId, FieldDefListId, Func, GenericArg, GenericArgListId, GenericParam,
    GenericParamListId, GenericParamOwner, IdentId, IngotId, ItemKind, KindBound as HirKindBound,
    Partial, PathId, TupleTypeId, TypeAlias as HirTypeAlias, TypeBound, TypeId as HirTyId,
    TypeKind as HirTyKind, VariantDefListId, VariantKind, WhereClauseId,
};
use rustc_hash::FxHashMap;
use salsa::function::Configuration;

use super::{
    const_ty::{ConstTyData, ConstTyId},
    ty_def::{
        AdtDef, AdtFieldList, AdtRef, AdtRefId, FuncDef, InvalidCause, Kind, TyData, TyId, TyParam,
    },
};
use crate::{
    name_resolution::{resolve_path_early, EarlyResolvedPath, NameDomain, NameResKind},
    HirAnalysisDb,
};

/// Lowers the given HirTy to `TyId`.
#[salsa::tracked(recovery_fn=recover_lower_hir_ty_cycle)]
pub fn lower_hir_ty(db: &dyn HirAnalysisDb, ty: HirTyId, scope: ScopeId) -> TyId {
    TyBuilder::new(db, scope).lower_ty(ty)
}

fn recover_lower_hir_ty_cycle(
    db: &dyn HirAnalysisDb,
    _cycle: &salsa::Cycle,
    _ty: HirTyId,
    _scope: ScopeId,
) -> TyId {
    TyId::invalid(db, InvalidCause::RecursiveConstParamTy)
}

/// Lower HIR ADT definition(`struct/enum/contract`) to [`AdtDef`].
#[salsa::tracked]
pub fn lower_adt(db: &dyn HirAnalysisDb, adt: AdtRefId) -> AdtDef {
    AdtTyBuilder::new(db, adt).build()
}

/// Lower func to [`FuncDef`]. This function returns `None` iff the function
/// name is `Partial::Absent`.
#[salsa::tracked]
pub fn lower_func(db: &dyn HirAnalysisDb, func: Func) -> Option<FuncDef> {
    let name = func.name(db.as_hir_db()).to_opt()?;
    let params_set = collect_generic_params(db, GenericParamOwnerId::new(db, func.into()));

    let args = match func.params(db.as_hir_db()) {
        Partial::Present(args) => args
            .data(db.as_hir_db())
            .iter()
            .map(|arg| {
                arg.ty
                    .to_opt()
                    .map(|ty| lower_hir_ty(db, ty, func.scope()))
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
            })
            .collect(),
        Partial::Absent => vec![],
    };

    let ret_ty = func
        .ret_ty(db.as_hir_db())
        .map(|ty| lower_hir_ty(db, ty, func.scope()))
        .unwrap_or_else(|| TyId::unit(db));

    Some(FuncDef::new(db, func, name, params_set, args, ret_ty))
}

/// Collects the generic parameters of the given generic parameter owner.
#[salsa::tracked]
pub(crate) fn collect_generic_params(
    db: &dyn HirAnalysisDb,
    owner: GenericParamOwnerId,
) -> GenericParamTypeSet {
    GenericParamCollector::new(db, owner.data(db)).finalize()
}

/// Lowers the given type alias to [`TyAlias`].
#[salsa::tracked(return_ref, recovery_fn = recover_lower_type_alias_cycle)]
pub(crate) fn lower_type_alias(
    db: &dyn HirAnalysisDb,
    alias: HirTypeAlias,
) -> Result<TyAlias, AliasCycle> {
    let param_set = collect_generic_params(db, GenericParamOwnerId::new(db, alias.into()));

    let Some(hir_ty) = alias.ty(db.as_hir_db()).to_opt() else {
        return Ok(TyAlias {
            alias,
            alias_to: TyId::invalid(db, InvalidCause::Other),
            param_set,
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
        param_set,
    })
}

#[doc(hidden)]
#[salsa::tracked(return_ref)]
pub(crate) fn evaluate_params_precursor(
    db: &dyn HirAnalysisDb,
    set: GenericParamTypeSet,
) -> Vec<TyId> {
    set.params_precursor(db)
        .iter()
        .enumerate()
        .map(|(i, p)| p.evaluate(db, set.scope(db), i))
        .collect()
}

fn recover_lower_type_alias_cycle(
    db: &dyn HirAnalysisDb,
    cycle: &salsa::Cycle,
    _alias: HirTypeAlias,
) -> Result<TyAlias, AliasCycle> {
    use once_cell::sync::Lazy;
    use regex::Regex;

    // Filter out the cycle participants that are not type aliases.
    // This is inefficient workaround because it's not possible to obtain the
    // ingredient index of the tracked function. Please refer to https://github.com/salsa-rs/salsa/pull/461 for more details.
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"lower_type_alias\(?(\d)\)").unwrap());
    let alias_cycle = cycle
        .all_participants(db)
        .into_iter()
        .filter_map(|participant| {
            let caps = RE.captures(&participant)?;
            let id = caps.get(1)?;
            let id = salsa::Id::from_u32(id.as_str().parse().ok()?);
            Some(lower_type_alias::key_from_id(id))
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
    param_set: GenericParamTypeSet,
}

impl TyAlias {
    fn params<'db>(&self, db: &'db dyn HirAnalysisDb) -> &'db [TyId] {
        self.param_set.params(db)
    }

    fn apply_subst(&self, db: &dyn HirAnalysisDb, arg_tys: &[TyId]) -> TyId {
        if arg_tys.len() < self.params(db).len() {
            return TyId::invalid(
                db,
                InvalidCause::UnboundTypeAliasParam {
                    alias: self.alias,
                    n_given_args: arg_tys.len(),
                },
            );
        }
        let mut subst = FxHashMap::default();

        for (&param, &arg) in self.params(db).iter().zip(arg_tys.iter()) {
            let arg = if param.kind(db) != arg.kind(db) {
                TyId::invalid(db, InvalidCause::kind_mismatch(param.kind(db).into(), arg))
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

            HirTyKind::Array(hir_elem_ty, len) => {
                let elem_ty = self.lower_opt_hir_ty(*hir_elem_ty);
                let len_ty = ConstTyId::from_opt_body(self.db, *len);
                let len_ty = TyId::const_ty(self.db, len_ty);
                let array = TyId::array(self.db);
                let array_1 = TyId::app(self.db, array, elem_ty);
                TyId::app(self.db, array_1, len_ty)
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
                    let param_num = alias.params(self.db).len();
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
            Either::Left(NameResKind::Prim(prim)) => {
                let ty = TyId::from_hir_prim_ty(self.db, prim);
                return args
                    .data(self.db.as_hir_db())
                    .iter()
                    .map(|arg| lower_generic_arg(self.db, arg, self.scope))
                    .fold(ty, |acc, arg| TyId::app(self.db, acc, arg));
            }

            Either::Right(ty) => {
                return args
                    .data(self.db.as_hir_db())
                    .iter()
                    .map(|arg| lower_generic_arg(self.db, arg, self.scope))
                    .fold(ty, |acc, arg| TyId::app(self.db, acc, arg));
            }
        };

        let ty = match scope {
            ScopeId::Item(item) => match item {
                ItemKind::Enum(_) | ItemKind::Struct(_) | ItemKind::Contract(_) => {
                    self.lower_resolved_path(res).unwrap_left()
                }

                ItemKind::Trait(trait_) => {
                    let params = collect_generic_params(
                        self.db,
                        GenericParamOwnerId::new(self.db, trait_.into()),
                    );
                    params.trait_self(self.db).unwrap()
                }

                ItemKind::Impl(impl_) => {
                    let hir_ty = impl_.ty(self.db.as_hir_db());
                    self.lower_opt_hir_ty(hir_ty)
                }

                ItemKind::ImplTrait(impl_trait) => {
                    let hir_ty = impl_trait.ty(self.db.as_hir_db());
                    self.lower_opt_hir_ty(hir_ty)
                }

                _ => TyId::invalid(self.db, InvalidCause::Other),
            },

            _ => unreachable!(),
        };

        args.data(self.db.as_hir_db())
            .iter()
            .map(|arg| lower_generic_arg(self.db, arg, self.scope))
            .fold(ty, |acc, arg| TyId::app(self.db, acc, arg))
    }

    fn lower_ptr(&mut self, pointee: Partial<HirTyId>) -> TyId {
        let pointee = self.lower_opt_hir_ty(pointee);

        let ptr = TyId::ptr(self.db);
        TyId::app(self.db, ptr, pointee)
    }

    fn lower_tuple(&mut self, tuple_id: TupleTypeId) -> TyId {
        let elems = tuple_id.data(self.db.as_hir_db());
        let len = elems.len();
        let tuple = TyId::tuple(self.db, len);
        elems.iter().fold(tuple, |acc, &elem| {
            let elem_ty = self.lower_opt_hir_ty(elem);
            if !elem_ty.has_star_kind(self.db) {
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
                return Either::Left(params.param_by_original_idx(self.db, idx).unwrap());
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
            ItemKind::Func(func) => {
                let func = lower_func(self.db, func).unwrap();
                Either::Left(TyId::func(self.db, func))
            }

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

    fn lower_opt_hir_ty(&self, hir_ty: Partial<HirTyId>) -> TyId {
        hir_ty
            .to_opt()
            .map(|hir_ty| lower_hir_ty(self.db, hir_ty, self.scope))
            .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other))
    }
}

pub(super) fn lower_generic_arg(db: &dyn HirAnalysisDb, arg: &GenericArg, scope: ScopeId) -> TyId {
    match arg {
        GenericArg::Type(ty_arg) => ty_arg
            .ty
            .to_opt()
            .map(|ty| lower_hir_ty(db, ty, scope))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),

        GenericArg::Const(const_arg) => {
            let const_ty = ConstTyId::from_opt_body(db, const_arg.body);
            TyId::const_ty(db, const_ty)
        }
    }
}

pub(crate) fn lower_generic_arg_list(
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
    params: GenericParamTypeSet,
    variants: Vec<AdtFieldList>,
}

impl<'db> AdtTyBuilder<'db> {
    fn new(db: &'db dyn HirAnalysisDb, adt: AdtRefId) -> Self {
        Self {
            db,
            adt,
            params: GenericParamTypeSet::empty(db, adt.scope(db)),
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

        self.params = collect_generic_params(self.db, owner_id);
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

        let fields = fields
            .data(self.db.as_hir_db())
            .iter()
            .map(|field| field.ty)
            .collect();

        self.variants.push(AdtFieldList::new(fields, scope));
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

                let variant = AdtFieldList::new(tys, scope);
                self.variants.push(variant)
            })
    }
}

#[doc(hidden)]
#[salsa::interned]
pub struct GenericParamTypeSet {
    params_precursor: Vec<TyParamPrecursor>,
    // pub trait_self: Option<TyId>,
    scope: ScopeId,
    offset_to_original: usize,
}

impl GenericParamTypeSet {
    pub(crate) fn params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        evaluate_params_precursor(db, self)
    }

    pub(crate) fn original_params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        let offset = self.offset_to_original(db);
        &self.params(db)[offset..]
    }

    pub(crate) fn empty(db: &dyn HirAnalysisDb, scope: ScopeId) -> Self {
        Self::new(db, Vec::new(), scope, 0)
    }

    pub(super) fn trait_self(&self, db: &dyn HirAnalysisDb) -> Option<TyId> {
        let params = self.params_precursor(db);
        let cand = params.first()?;

        if cand.is_trait_self() {
            Some(cand.evaluate(db, self.scope(db), 0))
        } else {
            None
        }
    }

    pub(super) fn param_by_original_idx(
        &self,
        db: &dyn HirAnalysisDb,
        original_idx: usize,
    ) -> Option<TyId> {
        let idx = self.offset_to_original(db) + original_idx;
        self.params_precursor(db)
            .get(idx)
            .map(|p| p.evaluate(db, self.scope(db), idx))
    }
}

struct GenericParamCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner,
    params: Vec<TyParamPrecursor>,
    offset_to_original: usize,
}

impl<'db> GenericParamCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, owner: GenericParamOwner) -> Self {
        let params = match owner {
            GenericParamOwner::Trait(_) => {
                vec![TyParamPrecursor::trait_self(None)]
            }

            GenericParamOwner::Func(func) if func.is_associated_func(db.as_hir_db()) => {
                let parent = owner.parent(db.as_hir_db()).unwrap();
                collect_generic_params(db, GenericParamOwnerId::new(db, parent))
                    .params_precursor(db)
            }

            _ => vec![],
        };

        let offset_to_original = params.len();
        Self {
            db,
            owner,
            params,
            offset_to_original,
        }
    }

    fn collect_generic_params(&mut self) {
        let hir_db = self.db.as_hir_db();
        let param_list = self.owner.params(hir_db);
        for (idx, param) in param_list.data(hir_db).iter().enumerate() {
            let idx = idx + self.offset_to_original;

            match param {
                GenericParam::Type(param) => {
                    let name = param.name;

                    let kind = self.extract_kind(param.bounds.as_slice());
                    self.params
                        .push(TyParamPrecursor::ty_param(name, idx, kind));
                }

                GenericParam::Const(param) => {
                    let name = param.name;
                    let hir_ty = param.ty.to_opt();

                    self.params
                        .push(TyParamPrecursor::const_ty_param(name, idx, hir_ty))
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
                    if self.params[idx].kind.is_none() && !self.params[idx].is_const_ty {
                        self.params[idx].kind = self.extract_kind(pred.bounds.as_slice());
                    }
                }

                ParamLoc::TraitSelf => {
                    let kind = self.extract_kind(pred.bounds.as_slice());
                    let trait_self = self.trait_self_ty_mut().unwrap();

                    if trait_self.kind.is_none() {
                        trait_self.kind = kind;
                    }
                }

                ParamLoc::NonParam => {}
            };
        }
    }

    fn finalize(mut self) -> GenericParamTypeSet {
        self.collect_generic_params();
        self.collect_kind_in_where_clause();

        GenericParamTypeSet::new(
            self.db,
            self.params,
            self.owner.scope(),
            self.offset_to_original,
        )
    }

    fn extract_kind(&self, bounds: &[TypeBound]) -> Option<Kind> {
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
                    NameResKind::Scope(ScopeId::GenericParam(scope, idx))
                        if self.owner.scope().item() == scope =>
                    {
                        ParamLoc::Idx(idx + self.offset_to_original)
                    }
                    _ => ParamLoc::NonParam,
                },
                Err(_) => ParamLoc::NonParam,
            },

            EarlyResolvedPath::Partial { .. } => ParamLoc::NonParam,
        }
    }

    fn trait_self_ty_mut(&mut self) -> Option<&mut TyParamPrecursor> {
        let cand = self.params.get_mut(0)?;
        cand.is_trait_self().then_some(cand)
    }
}

enum ParamLoc {
    TraitSelf,
    Idx(usize),
    NonParam,
}

#[doc(hidden)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyParamPrecursor {
    name: Partial<IdentId>,
    original_idx: Option<usize>,
    kind: Option<Kind>,
    const_ty_ty: Option<HirTyId>,
    is_const_ty: bool,
}

impl TyParamPrecursor {
    fn evaluate(&self, db: &dyn HirAnalysisDb, scope: ScopeId, lowered_idx: usize) -> TyId {
        let Partial::Present(name) = self.name else {
            return TyId::invalid(db, InvalidCause::Other);
        };

        let kind = self.kind.clone().unwrap_or(Kind::Star);

        if self.original_idx.is_none() {
            let param = TyParam::trait_self(kind);
            return TyId::new(db, TyData::TyParam(param));
        }

        let param = TyParam::normal_param(name, lowered_idx, kind);

        if !self.is_const_ty {
            return TyId::new(db, TyData::TyParam(param));
        }

        let const_ty_ty = match self.const_ty_ty {
            Some(ty) => {
                let ty = lower_hir_ty(db, ty, scope);
                if !(ty.contains_invalid(db) || ty.is_integral(db) || ty.is_bool(db)) {
                    TyId::invalid(db, InvalidCause::InvalidConstParamTy { ty })
                } else {
                    ty
                }
            }

            None => TyId::invalid(db, InvalidCause::Other),
        };

        let const_ty = ConstTyId::new(db, ConstTyData::TyParam(param, const_ty_ty));

        TyId::new(db, TyData::ConstTy(const_ty))
    }

    fn ty_param(name: Partial<IdentId>, idx: usize, kind: Option<Kind>) -> Self {
        Self {
            name,
            original_idx: idx.into(),
            kind,
            const_ty_ty: None,
            is_const_ty: false,
        }
    }

    fn const_ty_param(name: Partial<IdentId>, idx: usize, ty: Option<HirTyId>) -> Self {
        Self {
            name,
            original_idx: idx.into(),
            kind: None,
            const_ty_ty: ty,
            is_const_ty: true,
        }
    }

    fn trait_self(kind: Option<Kind>) -> Self {
        let name = Partial::Present(kw::SELF_TY);
        Self {
            name,
            original_idx: None,
            kind,
            const_ty_ty: None,
            is_const_ty: false,
        }
    }

    fn is_trait_self(&self) -> bool {
        self.original_idx.is_none()
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

    pub(super) fn where_clause(self, db: &dyn HirAnalysisDb) -> Option<WhereClauseId> {
        self.data(db)
            .where_clause_owner()
            .map(|owner| owner.where_clause(db.as_hir_db()))
    }

    pub(super) fn params(self, db: &dyn HirAnalysisDb) -> GenericParamListId {
        self.data(db).params(db.as_hir_db())
    }

    pub(super) fn from_item_opt(db: &dyn HirAnalysisDb, item: ItemKind) -> Option<Self> {
        let owner = GenericParamOwner::from_item_opt(item)?;
        Self::new(db, owner).into()
    }
}
