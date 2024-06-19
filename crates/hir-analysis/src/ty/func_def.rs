use hir::{
    hir_def::{scope_graph::ScopeId, Enum, Func, IdentId, IngotId, Partial},
    span::DynLazySpan,
};

use super::{binder::Binder, ty_def::TyId, ty_lower::GenericParamTypeSet};
use crate::{
    ty::{
        ty_def::InvalidCause,
        ty_lower::{collect_generic_params, lower_hir_ty, GenericParamOwnerId},
    },
    HirAnalysisDb,
};

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
                let ty = arg
                    .ty
                    .to_opt()
                    .map(|ty| lower_hir_ty(db, ty, func.scope()))
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other));
                Binder::bind(ty)
            })
            .collect(),
        Partial::Absent => vec![],
    };

    let ret_ty = func
        .ret_ty(db.as_hir_db())
        .map(|ty| lower_hir_ty(db, ty, func.scope()))
        .unwrap_or_else(|| TyId::unit(db));

    Some(FuncDef::new(
        db,
        func.into(),
        name,
        params_set,
        args,
        Binder::bind(ret_ty),
    ))
}

#[salsa::tracked]
pub struct FuncDef {
    pub hir_def: HirFuncDefKind,

    pub name: IdentId,

    pub params_set: GenericParamTypeSet,

    /// Argument types of the function.
    #[return_ref]
    pub arg_tys: Vec<Binder<TyId>>,

    /// Return types of the function.
    pub ret_ty: Binder<TyId>,
}

impl FuncDef {
    pub fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        self.hir_def(db).ingot(db)
    }

    pub fn name_span(self, db: &dyn HirAnalysisDb) -> DynLazySpan {
        self.hir_def(db).name_span()
    }

    pub fn param_list_span(self, db: &dyn HirAnalysisDb) -> DynLazySpan {
        self.hir_def(db).param_list_span()
    }

    pub fn scope(self, db: &dyn HirAnalysisDb) -> ScopeId {
        self.hir_def(db).scope()
    }

    pub fn params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.params_set(db).params(db)
    }

    pub fn explicit_params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        self.params_set(db).explicit_params(db)
    }

    pub fn receiver_ty(self, db: &dyn HirAnalysisDb) -> Option<Binder<TyId>> {
        self.is_method(db)
            .then(|| self.arg_tys(db).first().copied().unwrap())
    }

    pub fn is_method(self, db: &dyn HirAnalysisDb) -> bool {
        self.hir_def(db).is_method(db)
    }

    pub fn offset_to_explicit_params_position(self, db: &dyn HirAnalysisDb) -> usize {
        self.params_set(db).offset_to_explicit_params_position(db)
    }

    pub fn hir_func_def(self, db: &dyn HirAnalysisDb) -> Option<Func> {
        if let HirFuncDefKind::Func(func) = self.hir_def(db) {
            Some(func)
        } else {
            None
        }
    }

    pub fn param_span(self, db: &dyn HirAnalysisDb, idx: usize) -> DynLazySpan {
        self.hir_def(db).param_span(idx)
    }

    pub fn param_label(self, db: &dyn HirAnalysisDb, idx: usize) -> Option<IdentId> {
        self.hir_def(db).param_label(db, idx)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From)]
pub enum HirFuncDefKind {
    Func(Func),
    VariantCtor(Enum, usize),
}

impl HirFuncDefKind {
    pub fn name_span(self) -> DynLazySpan {
        match self {
            Self::Func(func) => func.lazy_span().name_moved().into(),
            Self::VariantCtor(enum_, idx) => enum_
                .lazy_span()
                .variants_moved()
                .variant_moved(idx)
                .name_moved()
                .into(),
        }
    }

    pub fn is_method(self, db: &dyn HirAnalysisDb) -> bool {
        match self {
            Self::Func(func) => func.is_method(db.as_hir_db()),
            Self::VariantCtor(..) => false,
        }
    }

    pub fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        let top_mod = match self {
            Self::Func(func) => func.top_mod(db.as_hir_db()),
            Self::VariantCtor(enum_, ..) => enum_.top_mod(db.as_hir_db()),
        };

        top_mod.ingot(db.as_hir_db())
    }

    pub fn scope(self) -> ScopeId {
        match self {
            Self::Func(func) => func.scope(),
            Self::VariantCtor(enum_, idx) => ScopeId::Variant(enum_.into(), idx),
        }
    }

    pub fn param_list_span(self) -> DynLazySpan {
        match self {
            Self::Func(func) => func.lazy_span().params_moved().into(),
            Self::VariantCtor(enum_, idx) => enum_
                .lazy_span()
                .variants_moved()
                .variant(idx)
                .tuple_type()
                .into(),
        }
    }

    pub fn param_label(self, db: &dyn HirAnalysisDb, idx: usize) -> Option<IdentId> {
        let Self::Func(func) = self else {
            return None;
        };

        func.params(db.as_hir_db())
            .to_opt()?
            .data(db.as_hir_db())
            .get(idx)?
            .label_eagerly()
    }

    pub fn param_span(self, idx: usize) -> DynLazySpan {
        match self {
            Self::Func(func) => func.lazy_span().params_moved().param(idx).into(),
            Self::VariantCtor(enum_, variant_idx) => enum_
                .lazy_span()
                .variants_moved()
                .variant_moved(variant_idx)
                .tuple_type_moved()
                .elem_ty_moved(idx)
                .into(),
        }
    }
}
