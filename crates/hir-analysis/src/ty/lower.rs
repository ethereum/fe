use either::Either;
use hir::hir_def::{
    kw, scope_graph::ScopeId, FieldDefListId, GenericArg, GenericArgListId, GenericParam,
    GenericParam as HirGenericParam, GenericParamOwner, ItemKind, Partial, PathId,
    TypeAlias as HirTypeAlias, TypeId as HirTyId, TypeKind as HirTyKind, VariantDefListId,
};

use crate::{
    name_resolution::{
        resolve_path_early, resolve_segments_early, EarlyResolvedPath, NameDomain, NameResKind,
    },
    ty::{
        diagnostics::{TyLowerDiag, TypeAliasDefDiagAccumulator},
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

#[salsa::tracked(return_ref, recovery_fn = recover_lower_type_alias_cycle)]
pub(crate) fn lower_type_alias(db: &dyn HirAnalysisDb, alias: HirTypeAlias) -> TyAlias {
    let params = lower_generic_param_list(db, alias.into());

    let Some(hir_ty) = alias.ty(db.as_hir_db()).to_opt() else {
        return TyAlias {
            alias,
            alias_to: TyId::invalid(db, InvalidCause::Other),
            params,
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
        params,
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
    let params = lower_generic_param_list(db, alias.into());
    TyAlias {
        alias,
        alias_to,
        params,
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
            .map(|path| {
                let res = resolve_path_early(self.db, path, self.scope);
                self.lower_resolved_path(&res)
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
        let res = resolve_segments_early(self.db, &[Partial::Present(kw::SELF_TY)], self.scope);
        let self_ty = self.lower_resolved_path(&res).unwrap_left();
        let arg_tys: Vec<_> = args
            .data(self.db.as_hir_db())
            .iter()
            .map(|arg| self.lower_generic_arg(arg))
            .collect();

        arg_tys
            .into_iter()
            .fold(self_ty, |acc, arg| TyId::app(self.db, acc, arg))
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

    fn lower_resolved_path(&mut self, path: &EarlyResolvedPath) -> Either<TyId, &'db TyAlias> {
        let res = match path {
            EarlyResolvedPath::Full(bucket) => match bucket.pick(NameDomain::Type) {
                Ok(res) => res,

                // This error is already handled by the name resolution.
                Err(_) => return Either::Left(TyId::invalid(self.db, InvalidCause::Other)),
            },

            EarlyResolvedPath::Partial { .. } => {
                return Either::Left(TyId::invalid(self.db, InvalidCause::AssocTy));
            }
        };

        let scope = match res.kind {
            NameResKind::Scope(scope) => scope,
            NameResKind::Prim(prim_ty) => {
                return Either::Left(TyId::from_hir_prim_ty(self.db, prim_ty))
            }
        };

        let item = match scope {
            ScopeId::Item(item) => item,
            ScopeId::GenericParam(item, idx) => {
                let params = GenericParamOwner::from_item_opt(item)
                    .unwrap()
                    .params(self.db.as_hir_db());
                let ty = lower_generic_param(self.db, &params.data(self.db.as_hir_db())[idx], idx);
                return Either::Left(ty);
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
        self.params = lower_generic_param_list(self.db, self.adt.as_item(self.db));
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

fn lower_generic_param_list(db: &dyn HirAnalysisDb, item: ItemKind) -> Vec<TyId> {
    let Some(params_owner) = GenericParamOwner::from_item_opt(item) else {
        return Vec::new();
    };

    params_owner
        .params(db.as_hir_db())
        .data(db.as_hir_db())
        .iter()
        .enumerate()
        .map(|(idx, param)| lower_generic_param(db, param, idx))
        .collect()
}

fn lower_generic_param(db: &dyn HirAnalysisDb, param: &HirGenericParam, idx: usize) -> TyId {
    match param {
        GenericParam::Type(param) => {
            if let Some(name) = param.name.to_opt() {
                let ty_param = TyParam {
                    name,
                    idx,
                    kind: Kind::Star,
                };
                TyId::new(db, TyData::TyParam(ty_param))
            } else {
                TyId::invalid(db, InvalidCause::Other)
            }
        }
        GenericParam::Const(_) => {
            todo!()
        }
    }
}
