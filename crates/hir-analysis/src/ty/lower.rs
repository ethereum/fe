use either::Either;
use hir::{
    hir_def::{
        kw, scope_graph::ScopeId, FieldDef, FieldDefListId, GenericArg, GenericArgListId,
        GenericParam, ItemKind, Partial, PathId, TypeAlias as HirTypeAlias, TypeId as HirTyId,
        TypeKind as HirTyKind, VariantDefListId,
    },
    visitor::prelude::*,
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
    ty::{AdtDef, AdtRef, AdtRefId, AdtVariant, InvalidCause, Kind, TyData, TyId, TyParam},
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
pub fn analyze_adt(db: &dyn HirAnalysisDb, adt: AdtRefId) {
    let mut analyzer = AdtDefAnalysisVisitor {
        db,
        accumulated: Vec::new(),
        scope: adt.scope(db),
    };
    let item = adt.as_item(db);

    let mut ctxt = VisitorCtxt::with_item(db.as_hir_db(), item);
    analyzer.visit_item(&mut ctxt, item);

    for diag in analyzer.accumulated {
        AdtDefDiagAccumulator::push(db, diag);
    }
}

#[salsa::tracked]
pub(crate) fn lower_type_alias(_db: &dyn HirAnalysisDb, _alias: HirTypeAlias) -> TyAlias {
    todo!()
}

/// Represents a lowered type alias. `TyAlias` itself isn't a type, but
/// can be instantiated to a `TyId` by substituting its type
/// parameters with actual types.
///
/// NOTE: `TyAlias` can't become an alias to partial applied types, i.e., the
/// right hand side of the alias declaration must be a fully applied type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TyAlias {
    alias_to: TyId,
    params: Vec<TyId>,
}

pub(super) fn collect_ty_lower_diags(
    db: &dyn HirAnalysisDb,
    ty: HirTyId,
    span: LazyTySpan,
    scope: ScopeId,
) -> Vec<TyLowerDiag> {
    let mut ctxt = VisitorCtxt::new(db.as_hir_db(), span);
    let mut accumulator = TyDiagAccumulator {
        db,
        accumulated: Vec::new(),
        scope,
    };

    accumulator.visit_ty(&mut ctxt, ty);
    accumulator.accumulated
}

impl TyAlias {
    fn subst_with(&self, _db: &dyn HirAnalysisDb, _substs: &[TyId]) -> TyId {
        todo!()
    }
}

struct TyDiagAccumulator<'db> {
    db: &'db dyn HirAnalysisDb,
    accumulated: Vec<TyLowerDiag>,
    scope: ScopeId,
}

impl<'db> TyDiagAccumulator<'db> {
    fn accumulate(&mut self, cause: InvalidCause, span: LazyTySpan) {
        let span: DynLazySpan = span.into();
        match cause {
            InvalidCause::NotFullyApplied => {
                let diag = TyLowerDiag::not_fully_applied_type(span);
                self.accumulated.push(diag);
            }

            InvalidCause::KindMismatch { abs, arg } => {
                let diag = TyLowerDiag::kind_mismatch(self.db, abs, arg, span);
                self.accumulated.push(diag);
            }

            InvalidCause::AssocTy => {
                let diag = TyLowerDiag::assoc_ty(span);
                self.accumulated.push(diag);
            }

            // NOTE: We can `InvalidCause::Other` because it's already reported by other passes.
            InvalidCause::Other => {}
        }
    }
}

impl<'db> Visitor for TyDiagAccumulator<'db> {
    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'_, LazyTySpan>, hir_ty: HirTyId) {
        let ty = lower_hir_ty(self.db, hir_ty, self.scope);
        if let Some(cause) = ty.invalid_cause(self.db) {
            self.accumulate(cause, ctxt.span().unwrap());
        }

        walk_ty(self, ctxt, hir_ty);
    }
}

struct AdtDefAnalysisVisitor<'db> {
    db: &'db dyn HirAnalysisDb,
    accumulated: Vec<TyLowerDiag>,
    scope: ScopeId,
}

impl<'db> AdtDefAnalysisVisitor<'db> {
    // This method ensures that field/variant types are fully applied.
    fn verify_fully_applied(&mut self, ty: HirTyId, span: DynLazySpan) {
        let ty = lower_hir_ty(self.db, ty, self.scope);
        if !ty.is_mono_type(self.db) {
            self.accumulated
                .push(TyLowerDiag::not_fully_applied_type(span));
        }
    }
}

impl<'db> Visitor for AdtDefAnalysisVisitor<'db> {
    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'_, LazyTySpan>, hir_ty: HirTyId) {
        self.accumulated.extend(collect_ty_lower_diags(
            self.db,
            hir_ty,
            ctxt.span().unwrap(),
            self.scope,
        ));

        // We don't call `walk_ty` to make sure that we don't visit ty
        // recursively, which is visited by `collect_ty_lower_diags`.
    }

    fn visit_field_def(&mut self, ctxt: &mut VisitorCtxt<'_, LazyFieldDefSpan>, field: &FieldDef) {
        if let Some(ty) = field.ty.to_opt() {
            self.verify_fully_applied(ty, ctxt.span().unwrap().ty().into());
        }

        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyVariantDefSpan>,
        variant: &hir::hir_def::VariantDef,
    ) {
        if let Some(ty) = variant.ty {
            self.verify_fully_applied(ty, ctxt.span().unwrap().ty().into());
        }

        walk_variant_def(self, ctxt, variant);
    }

    // TODO: We need to check cycle type.
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

            HirTyKind::SelfType => self.lower_self_ty(),

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

            Either::Right(alias) => alias.subst_with(self.db, &arg_tys),
        }
    }

    pub(super) fn lower_self_ty(&mut self) -> TyId {
        let res = resolve_segments_early(self.db, &[Partial::Present(kw::SELF_TY)], self.scope);
        self.lower_resolved_path(&res).unwrap_left()
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

    fn lower_resolved_path(&mut self, path: &EarlyResolvedPath) -> Either<TyId, TyAlias> {
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
                return Either::Left(lower_generic_param(self.db, item, idx));
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
    variants: Vec<AdtVariant>,
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
        self.collect_params();
        self.collect_variants();
        AdtDef::new(self.db, self.adt, self.params, self.variants)
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
            let variant = AdtVariant::new(field.name, vec![field.ty], scope);
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

                let variant = AdtVariant::new(variant.name, tys, scope);
                self.variants.push(variant)
            })
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
                TyId::invalid(db, InvalidCause::Other)
            }
        }
        GenericParam::Const(_) => {
            todo!()
        }
    }
}
