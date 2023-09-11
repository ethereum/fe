use either::Either;
use hir::{
    hir_def::{
        kw, scope_graph::ScopeId, FieldDefListId, GenericArg, GenericArgListId, GenericParam,
        ItemKind, Partial, PathId, TypeAlias as HirTypeAlias, TypeId as HirTyId,
        TypeKind as HirTyKind, VariantDefListId,
    },
    span::{types::LazyTySpan, DynLazySpan},
    visitor::prelude::{
        LazyFieldDefListSpan, LazyGenericArgSpan, LazyPathTypeSpan, LazyPtrTypeSpan,
        LazyTupleTypeSpan, LazyVariantDefListSpan,
    },
};

use crate::{
    name_resolution::{
        resolve_path_early, resolve_segments_early, EarlyResolvedPath, NameDomain, NameResKind,
    },
    ty::diagnostics::AdtDefDiagAccumulator,
    HirAnalysisDb,
};

use super::{
    diagnostics::TyLowerDiag,
    ty::{AdtDef, AdtRef, AdtRefId, AdtVariant, Kind, TyData, TyId, TyParam},
};

#[salsa::tracked]
pub fn lower_adt(db: &dyn HirAnalysisDb, adt: AdtRefId) -> TyId {
    let (ty, diags) = AdtTyBuilder::new(db, adt).build();
    for diag in diags {
        AdtDefDiagAccumulator::push(db, diag)
    }
    ty
}

#[salsa::tracked]
pub fn lower_type_alias(_db: &dyn HirAnalysisDb, _alias: HirTypeAlias) -> TyAlias {
    todo!()
}

/// Represents a lowered type alias. `TyAlias` itself isn't a type, but
/// can be instantiated to a `TyId` by substituting its type
/// parameters with actual types.
///
/// NOTE: `TyAlias` can't become an alias to partial applied types, i.e., the
/// right hand side of the alias declaration must be a fully applied type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyAlias {
    alias_to: TyId,
    params: Vec<TyId>,
}

impl TyAlias {
    fn subst_with(&self, _db: &dyn HirAnalysisDb, _substs: &[TyId]) -> TyId {
        todo!()
    }
}

pub(crate) struct TyBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId,
    diags: Vec<TyLowerDiag>,
}

impl<'db> TyBuilder<'db> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, scope: ScopeId) -> Self {
        Self {
            db,
            scope,
            diags: Vec::new(),
        }
    }

    pub(super) fn lower_ty(&mut self, ty: HirTyId, span: LazyTySpan) -> TyId {
        match ty.data(self.db.as_hir_db()) {
            HirTyKind::Ptr(pointee) => self.lower_ptr(*pointee, span.into_ptr_type()),

            HirTyKind::Path(path, args) => self.lower_path(*path, *args, span.into_path_type()),

            HirTyKind::SelfType => self.lower_self_ty(span),

            HirTyKind::Tuple(elems) => self.lower_tuple(elems, span.into_tuple_type()),

            HirTyKind::Array(_, _) => {
                todo!()
            }
        }
    }

    pub(super) fn lower_path(
        &mut self,
        path: Partial<PathId>,
        args: GenericArgListId,
        span: LazyPathTypeSpan,
    ) -> TyId {
        let path_ty = path
            .to_opt()
            .map(|path| {
                let res = resolve_path_early(self.db, path, self.scope);
                self.lower_resolved_path(&res, span.path().into())
            })
            .unwrap_or_else(|| Either::Left(TyId::invalid(self.db)));

        let generic_arg_span = span.generic_args();

        let arg_tys: Vec<_> = args
            .data(self.db.as_hir_db())
            .iter()
            .enumerate()
            .map(|(idx, arg)| self.lower_generic_arg(arg, generic_arg_span.arg(idx)))
            .collect();

        match path_ty {
            Either::Left(ty) => arg_tys.into_iter().enumerate().fold(ty, |acc, (idx, arg)| {
                self.ty_app(acc, arg, generic_arg_span.arg(idx).into())
            }),

            Either::Right(alias) => alias.subst_with(self.db, &arg_tys),
        }
    }

    pub(super) fn lower_self_ty(&mut self, span: LazyTySpan) -> TyId {
        let res = resolve_segments_early(self.db, &[Partial::Present(kw::SELF_TY)], self.scope);
        self.lower_resolved_path(&res, span.into()).unwrap_left()
    }

    fn lower_ptr(&mut self, pointee: Partial<HirTyId>, span: LazyPtrTypeSpan) -> TyId {
        let pointee = pointee
            .to_opt()
            .map(|pointee| self.lower_ty(pointee, span.pointee()))
            .unwrap_or_else(|| TyId::invalid(self.db));

        let ptr = TyId::ptr(self.db);
        self.ty_app(ptr, pointee, span.into())
    }

    fn lower_tuple(&mut self, elems: &[Partial<HirTyId>], span: LazyTupleTypeSpan) -> TyId {
        let len = elems.len();
        let tuple = TyId::tuple(self.db, len);
        elems.iter().enumerate().fold(tuple, |acc, (idx, elem)| {
            let elem_ty = elem
                .to_opt()
                .map(|elem| self.lower_ty(elem, span.elem_ty(idx)))
                .unwrap_or_else(|| TyId::invalid(self.db));
            let (elem_ty, diag) =
                verify_fully_applied_type(self.db, elem_ty, span.elem_ty(idx).into());
            if let Some(diag) = diag {
                self.diags.push(diag);
            }

            self.ty_app(acc, elem_ty, span.elem_ty(idx).into())
        })
    }

    /// Perform type level application.
    /// If type application is not possible for the given `abs`/`arg` pair,
    /// diagnostics are accumulated then returns` TyId::invalid()`.
    fn ty_app(&mut self, abs: TyId, arg: TyId, span: DynLazySpan) -> TyId {
        if let Some(ty) = TyId::apply(self.db, abs, arg) {
            ty
        } else {
            self.diags
                .push(TyLowerDiag::kind_mismatch(self.db, abs, arg, span));
            TyId::invalid(self.db)
        }
    }

    fn lower_resolved_path(
        &mut self,
        path: &EarlyResolvedPath,
        span: DynLazySpan,
    ) -> Either<TyId, TyAlias> {
        let res = match path {
            EarlyResolvedPath::Full(bucket) => match bucket.pick(NameDomain::Type) {
                Ok(res) => res,

                // This error is already handled by the name resolution.
                Err(_) => return Either::Left(TyId::invalid(self.db)),
            },

            EarlyResolvedPath::Partial { .. } => {
                // TODO: Fix here when we add an associated type.
                self.diags.push(TyLowerDiag::assoc_ty(span));
                return Either::Left(TyId::invalid(self.db));
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
                return Either::Left(lower_generic_param(self.db, item, idx));
            }
            _ => unreachable!(),
        };

        match item {
            ItemKind::Enum(enum_) => {
                let adt_ref = AdtRefId::from_enum(self.db, enum_);
                Either::Left(lower_adt(self.db, adt_ref))
            }
            ItemKind::Struct(struct_) => {
                let adt_ref = AdtRefId::from_struct(self.db, struct_);
                Either::Left(lower_adt(self.db, adt_ref))
            }
            ItemKind::Contract(contract) => {
                let adt_ref = AdtRefId::from_contract(self.db, contract);
                Either::Left(lower_adt(self.db, adt_ref))
            }
            ItemKind::TypeAlias(alias) => Either::Right(lower_type_alias(self.db, alias)),
            _ => {
                self.diags.push(TyLowerDiag::invalid_type(span));
                Either::Left(TyId::invalid(self.db))
            }
        }
    }

    fn lower_generic_arg(&mut self, arg: &GenericArg, span: LazyGenericArgSpan) -> TyId {
        match arg {
            GenericArg::Type(ty_arg) => ty_arg
                .ty
                .to_opt()
                .map(|ty| self.lower_ty(ty, span.into_type_arg().ty()))
                .unwrap_or_else(|| TyId::invalid(self.db)),

            GenericArg::Const(_) => todo!(),
        }
    }
}

struct AdtTyBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    adt: AdtRefId,
    params: Vec<TyId>,
    variants: Vec<AdtVariant>,
    diags: Vec<TyLowerDiag>,
}

impl<'db> AdtTyBuilder<'db> {
    fn new(db: &'db dyn HirAnalysisDb, adt: AdtRefId) -> Self {
        Self {
            db,
            adt,
            params: Vec::new(),
            variants: Vec::new(),
            diags: Vec::new(),
        }
    }

    fn build(mut self) -> (TyId, Vec<TyLowerDiag>) {
        self.collect_params();
        self.collect_variants();

        let adt_def = AdtDef::new(self.db, self.adt, self.params, self.variants);
        (TyId::adt(self.db, adt_def), self.diags)
    }

    fn collect_params(&mut self) {
        let hir_db = self.db.as_hir_db();
        let params = match self.adt.data(self.db) {
            AdtRef::Struct(struct_) => struct_.generic_params(hir_db),
            AdtRef::Enum(enum_) => enum_.generic_params(hir_db),
            AdtRef::Contract(_) => return,
        };

        for idx in 0..params.len(hir_db) {
            let param = lower_generic_param(self.db, self.adt.as_item(self.db), idx);
            self.params.push(param);
        }
    }

    fn collect_variants(&mut self) {
        match self.adt.data(self.db) {
            AdtRef::Struct(struct_) => {
                let span = struct_.lazy_span();
                self.collect_field_types(struct_.fields(self.db.as_hir_db()), span.fields());
            }

            AdtRef::Contract(contract) => {
                let span = contract.lazy_span();
                self.collect_field_types(contract.fields(self.db.as_hir_db()), span.fields())
            }

            AdtRef::Enum(enum_) => {
                let span = enum_.lazy_span();
                self.collect_enum_variant_types(
                    enum_.variants(self.db.as_hir_db()),
                    span.variants(),
                )
            }
        };
    }

    fn collect_field_types(&mut self, fields: FieldDefListId, span: LazyFieldDefListSpan) {
        fields
            .data(self.db.as_hir_db())
            .iter()
            .enumerate()
            .for_each(|(i, field)| {
                let ty = match field.ty.to_opt() {
                    Some(ty) => {
                        let mut builder = TyBuilder::new(self.db, self.adt.data(self.db).scope());
                        let ty_span = span.field(i).ty();

                        let ty = builder.lower_ty(ty, ty_span.clone());
                        let ty = self.verify_fully_applied_type(ty, ty_span.into());

                        self.diags.extend(builder.diags);
                        ty
                    }

                    None => TyId::invalid(self.db),
                };

                let variant = AdtVariant {
                    name: field.name,
                    tys: vec![ty],
                };
                self.variants.push(variant);
            })
    }

    fn collect_enum_variant_types(
        &mut self,
        variants: VariantDefListId,
        span: LazyVariantDefListSpan,
    ) {
        variants
            .data(self.db.as_hir_db())
            .iter()
            .enumerate()
            .for_each(|(i, variant)| {
                let tys = match variant.ty {
                    Some(ty) => {
                        let mut builder = TyBuilder::new(self.db, self.adt.scope(self.db));
                        let ty_span = span.variant(i).ty();

                        let ty = builder.lower_ty(ty, ty_span.clone());
                        let ty = self.verify_fully_applied_type(ty, ty_span.into());

                        self.diags.extend(builder.diags);
                        vec![ty]
                    }

                    None => vec![],
                };

                let variant = AdtVariant {
                    name: variant.name,
                    tys,
                };
                self.variants.push(variant)
            })
    }

    /// Verifies that the type is fully applied type.
    /// If the `ty` is not a fully applied type, diagnostics are
    /// accumulated then returns `TyId::invalid()`, otherwise returns given
    /// `ty`.
    fn verify_fully_applied_type(&mut self, ty: TyId, span: DynLazySpan) -> TyId {
        let (ty, diag) = verify_fully_applied_type(self.db, ty, span);
        if let Some(diag) = diag {
            self.diags.push(diag);
        }
        ty
    }
}

fn verify_fully_applied_type(
    db: &dyn HirAnalysisDb,
    ty: TyId,
    span: DynLazySpan,
) -> (TyId, Option<TyLowerDiag>) {
    if ty.is_mono_type(db) {
        (ty, None)
    } else {
        (
            TyId::invalid(db),
            TyLowerDiag::not_fully_applied_type(span).into(),
        )
    }
}

fn lower_generic_param(db: &dyn HirAnalysisDb, item: ItemKind, idx: usize) -> TyId {
    let params = match item {
        ItemKind::Struct(struct_) => struct_.generic_params(db.as_hir_db()),
        ItemKind::Enum(enum_) => enum_.generic_params(db.as_hir_db()),
        _ => unreachable!(),
    };

    let param = &params.data(db.as_hir_db())[idx];
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
                TyId::new(db, TyData::Invalid)
            }
        }
        GenericParam::Const(_) => {
            todo!()
        }
    }
}
