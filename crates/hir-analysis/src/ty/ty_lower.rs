use hir::hir_def::{
    scope_graph::ScopeId, GenericArg, GenericArgListId, GenericParam, GenericParamOwner, IdentId,
    ItemKind, KindBound as HirKindBound, Partial, PathId, TypeAlias as HirTypeAlias, TypeBound,
    TypeId as HirTyId, TypeKind as HirTyKind,
};
use salsa::Update;
use smallvec::smallvec;

use super::{
    const_ty::{ConstTyData, ConstTyId},
    ty_def::{InvalidCause, Kind, TyData, TyId, TyParam},
};
use crate::name_resolution::{
    resolve_ident_to_bucket, resolve_path, NameDomain, NameResKind, PathRes,
};
use crate::{ty::binder::Binder, HirAnalysisDb};

/// Lowers the given HirTy to `TyId`.
#[salsa::tracked]
pub fn lower_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
) -> TyId<'db> {
    match ty.data(db) {
        HirTyKind::Ptr(pointee) => {
            let pointee = lower_opt_hir_ty(db, scope, *pointee);
            let ptr = TyId::ptr(db);
            TyId::app(db, ptr, pointee)
        }

        HirTyKind::Path(path) => lower_path(db, scope, *path),

        HirTyKind::SelfType(args) => {
            let path = PathId::self_ty(db, *args);
            match resolve_path(db, path, scope, None, false) {
                Ok(PathRes::Ty(ty)) => ty,
                Ok(_) => unreachable!(),
                Err(_) => TyId::invalid(db, InvalidCause::Other),
            }
        }

        HirTyKind::Tuple(tuple_id) => {
            let elems = tuple_id.data(db);
            let len = elems.len();
            let tuple = TyId::tuple(db, len);
            elems.iter().fold(tuple, |acc, &elem| {
                let elem_ty = lower_opt_hir_ty(db, scope, elem);
                if !elem_ty.has_star_kind(db) {
                    return TyId::invalid(db, InvalidCause::NotFullyApplied);
                }

                TyId::app(db, acc, elem_ty)
            })
        }

        HirTyKind::Array(hir_elem_ty, len) => {
            let elem_ty = lower_opt_hir_ty(db, scope, *hir_elem_ty);
            let len_ty = ConstTyId::from_opt_body(db, *len);
            let len_ty = TyId::const_ty(db, len_ty);
            let array = TyId::array(db, elem_ty);
            TyId::app(db, array, len_ty)
        }

        HirTyKind::Never => TyId::never(db),
    }
}

fn lower_opt_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    hir_ty: Partial<HirTyId<'db>>,
) -> TyId<'db> {
    hir_ty
        .to_opt()
        .map(|hir_ty| lower_hir_ty(db, hir_ty, scope))
        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
}

fn lower_path<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: Partial<PathId<'db>>,
) -> TyId<'db> {
    let Some(path) = path.to_opt() else {
        return TyId::invalid(db, InvalidCause::Other);
    };
    match resolve_path(db, path, scope, None, false) {
        Ok(PathRes::Ty(ty) | PathRes::Func(ty)) => ty,
        // Other cases should be reported as errors by nameres
        _ => TyId::invalid(db, InvalidCause::Other),
    }
}

fn lower_const_ty_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: HirTyId<'db>,
) -> TyId<'db> {
    let hir_db = db;
    let HirTyKind::Path(path) = ty.data(hir_db) else {
        return TyId::invalid(db, InvalidCause::InvalidConstParamTy);
    };

    if !path
        .to_opt()
        .map(|p| p.generic_args(hir_db).is_empty(hir_db))
        .unwrap_or(true)
    {
        return TyId::invalid(db, InvalidCause::InvalidConstParamTy);
    }
    let ty = lower_path(db, scope, *path);

    if ty.has_invalid(db) || ty.is_integral(db) || ty.is_bool(db) {
        ty
    } else {
        TyId::invalid(db, InvalidCause::InvalidConstParamTy)
    }
}

/// Collects the generic parameters of the given generic parameter owner.
#[salsa::tracked]
pub(crate) fn collect_generic_params<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> GenericParamTypeSet<'db> {
    GenericParamCollector::new(db, owner).finalize()
}

/// Lowers the given type alias to [`TyAlias`].
#[salsa::tracked(return_ref, cycle_fn=lower_type_alias_cycle_recover, cycle_initial=lower_type_alias_cycle_initial)]
pub(crate) fn lower_type_alias<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
) -> TyAlias<'db> {
    let param_set = collect_generic_params(db, alias.into());

    let Some(hir_ty) = alias.ty(db).to_opt() else {
        return TyAlias {
            alias,
            alias_to: Binder::bind(TyId::invalid(db, InvalidCause::Other)),
            param_set,
        };
    };

    let alias_to = lower_hir_ty(db, hir_ty, alias.scope());
    let alias_to = if let TyData::Invalid(InvalidCause::AliasCycle(cycle)) = alias_to.data(db) {
        if cycle.contains(&alias) {
            alias_to
        } else {
            let mut cycle = cycle.clone();
            cycle.push(alias);
            TyId::invalid(db, InvalidCause::AliasCycle(cycle))
        }
    } else if alias_to.has_invalid(db) {
        // Should be reported by TypeAliasAnalysisPass
        TyId::invalid(db, InvalidCause::Other)
    } else {
        alias_to
    };
    TyAlias {
        alias,
        alias_to: Binder::bind(alias_to),
        param_set,
    }
}

fn lower_type_alias_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
) -> TyAlias<'db> {
    TyAlias {
        alias,
        alias_to: Binder::bind(TyId::invalid(
            db,
            InvalidCause::AliasCycle(smallvec![alias]),
        )),
        param_set: GenericParamTypeSet::empty(db, alias.scope()),
    }
}

fn lower_type_alias_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &TyAlias<'db>,
    _count: u32,
    _alias: HirTypeAlias<'db>,
) -> salsa::CycleRecoveryAction<TyAlias<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

#[doc(hidden)]
#[salsa::tracked(return_ref)]
pub(crate) fn evaluate_params_precursor<'db>(
    db: &'db dyn HirAnalysisDb,
    set: GenericParamTypeSet<'db>,
) -> Vec<TyId<'db>> {
    set.params_precursor(db)
        .iter()
        .enumerate()
        .map(|(i, p)| p.evaluate(db, set.scope(db), i))
        .collect()
}

/// Represents a lowered type alias. `TyAlias` itself isn't a type, but
/// can be instantiated to a `TyId` by substituting its type
/// parameters with actual types.
///
/// NOTE: `TyAlias` can't become an alias to partial applied types, i.e., the
/// right hand side of the alias declaration must be a fully applied type.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub(crate) struct TyAlias<'db> {
    alias: HirTypeAlias<'db>,
    pub alias_to: Binder<TyId<'db>>,
    param_set: GenericParamTypeSet<'db>,
}

impl<'db> TyAlias<'db> {
    pub fn params(&self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set.params(db)
    }
}

pub(super) fn lower_generic_arg<'db>(
    db: &'db dyn HirAnalysisDb,
    arg: &GenericArg<'db>,
    scope: ScopeId<'db>,
) -> TyId<'db> {
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

pub(crate) fn lower_generic_arg_list<'db>(
    db: &'db dyn HirAnalysisDb,
    args: GenericArgListId<'db>,
    scope: ScopeId<'db>,
) -> Vec<TyId<'db>> {
    args.data(db)
        .iter()
        .map(|arg| lower_generic_arg(db, arg, scope))
        .collect()
}

#[salsa::interned]
#[derive(Debug)]
pub struct GenericParamTypeSet<'db> {
    #[return_ref]
    pub(crate) params_precursor: Vec<TyParamPrecursor<'db>>,
    pub(crate) scope: ScopeId<'db>,
    offset_to_explicit: usize,
}

impl<'db> GenericParamTypeSet<'db> {
    pub(crate) fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        evaluate_params_precursor(db, self)
    }

    pub(crate) fn explicit_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        let offset = self.offset_to_explicit(db);
        &self.params(db)[offset..]
    }

    pub(crate) fn empty(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Self {
        Self::new(db, Vec::new(), scope, 0)
    }

    pub(crate) fn trait_self(&self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let params = self.params_precursor(db);
        let cand = params.first()?;

        if cand.is_trait_self() {
            Some(cand.evaluate(db, self.scope(db), 0))
        } else {
            None
        }
    }

    pub(super) fn offset_to_explicit_params_position(&self, db: &dyn HirAnalysisDb) -> usize {
        self.offset_to_explicit(db)
    }

    pub(crate) fn param_by_original_idx(
        &self,
        db: &'db dyn HirAnalysisDb,
        original_idx: usize,
    ) -> Option<TyId<'db>> {
        let idx = self.offset_to_explicit(db) + original_idx;
        self.params_precursor(db)
            .get(idx)
            .map(|p| p.evaluate(db, self.scope(db), idx))
    }
}

struct GenericParamCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    params: Vec<TyParamPrecursor<'db>>,
    offset_to_original: usize,
}

impl<'db> GenericParamCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, owner: GenericParamOwner<'db>) -> Self {
        let params = match owner {
            GenericParamOwner::Trait(_) => {
                vec![TyParamPrecursor::trait_self(db, None)]
            }

            GenericParamOwner::Func(func) if func.is_associated_func(db) => {
                let parent = owner.parent(db).unwrap();
                collect_generic_params(db, parent)
                    .params_precursor(db)
                    .to_vec()
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
        let hir_db = self.db;
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

        let hir_db = self.db;
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

    fn finalize(mut self) -> GenericParamTypeSet<'db> {
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

        let hir_db = self.db;

        let path = match ty.data(self.db) {
            HirTyKind::Path(Partial::Present(path)) => {
                if path.is_bare_ident(hir_db) {
                    *path
                } else {
                    return ParamLoc::NonParam;
                }
            }

            HirTyKind::SelfType(args) => {
                return if matches!(self.owner.into(), ItemKind::Trait(_)) && args.is_empty(hir_db) {
                    ParamLoc::TraitSelf
                } else {
                    ParamLoc::NonParam
                };
            }

            _ => return ParamLoc::NonParam,
        };

        let bucket = resolve_ident_to_bucket(self.db, path, self.owner.scope());
        match bucket.pick(NameDomain::TYPE) {
            Ok(res) => match res.kind {
                NameResKind::Scope(ScopeId::GenericParam(scope, idx))
                    if scope == self.owner.scope().item() =>
                {
                    ParamLoc::Idx(idx + self.offset_to_original)
                }
                _ => ParamLoc::NonParam,
            },
            _ => ParamLoc::NonParam,
        }
    }

    fn trait_self_ty_mut(&mut self) -> Option<&mut TyParamPrecursor<'db>> {
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
pub struct TyParamPrecursor<'db> {
    name: Partial<IdentId<'db>>,
    original_idx: Option<usize>,
    kind: Option<Kind>,
    const_ty_ty: Option<HirTyId<'db>>,
    is_const_ty: bool,
}

impl<'db> TyParamPrecursor<'db> {
    fn evaluate(
        &self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        lowered_idx: usize,
    ) -> TyId<'db> {
        let Partial::Present(name) = self.name else {
            return TyId::invalid(db, InvalidCause::Other);
        };

        let kind = self.kind.clone().unwrap_or(Kind::Star);

        if self.original_idx.is_none() {
            let param = TyParam::trait_self(db, kind, scope);
            return TyId::new(db, TyData::TyParam(param));
        }

        let param = TyParam::normal_param(name, lowered_idx, kind, scope);

        if !self.is_const_ty {
            return TyId::new(db, TyData::TyParam(param));
        }

        let const_ty_ty = match self.const_ty_ty {
            Some(ty) => lower_const_ty_ty(db, scope, ty),
            None => TyId::invalid(db, InvalidCause::Other),
        };

        let const_ty = ConstTyId::new(db, ConstTyData::TyParam(param, const_ty_ty));
        TyId::new(db, TyData::ConstTy(const_ty))
    }

    fn ty_param(name: Partial<IdentId<'db>>, idx: usize, kind: Option<Kind>) -> Self {
        Self {
            name,
            original_idx: idx.into(),
            kind,
            const_ty_ty: None,
            is_const_ty: false,
        }
    }

    fn const_ty_param(name: Partial<IdentId<'db>>, idx: usize, ty: Option<HirTyId<'db>>) -> Self {
        Self {
            name,
            original_idx: idx.into(),
            kind: None,
            const_ty_ty: ty,
            is_const_ty: true,
        }
    }

    fn trait_self(db: &'db dyn HirAnalysisDb, kind: Option<Kind>) -> Self {
        let name = Partial::Present(IdentId::make_self_ty(db));
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
