//! Explicit origins for nominal handles manufactured without an input referent.
use super::semantics::UnresolvedCapability;
use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            assoc_const::AssocConstUse,
            const_ty::{ConstTyId, const_ty_or_abstract_from_assoc_const_use},
            provider::{
                EffectHandleResolution, ProviderAddressSpace, effect_space_from_const_ty,
                resolve_effect_handle,
            },
            trait_resolution::PredicateListId,
            ty_def::TyId,
        },
    },
    hir_def::{IdentId, scope_graph::ScopeId},
};

/// A semantic address-space contract can remain symbolic in a generic body.
/// Unknown spaces may alias every concrete space; they never default to memory.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HandleAddressSpace<'db> {
    /// Native capabilities inherit the caller's referent space at instantiation.
    Unspecified,
    Known(ProviderAddressSpace),
    Declared {
        value: TyId<'db>,
        scope: ScopeId<'db>,
    },
}

impl<'db> HandleAddressSpace<'db> {
    pub fn known(self) -> Option<ProviderAddressSpace> {
        match self {
            Self::Known(space) => Some(space),
            Self::Declared { .. } | Self::Unspecified => None,
        }
    }

    fn from_const(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>, value: ConstTyId<'db>) -> Self {
        effect_space_from_const_ty(db, scope, value).map_or_else(
            || Self::Declared {
                value: TyId::const_ty(db, value),
                scope,
            },
            Self::Known,
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OpaqueHandleContract<'db> {
    pub handle_ty: TyId<'db>,
    pub target_ty: TyId<'db>,
    pub address_space: HandleAddressSpace<'db>,
}

impl<'db> OpaqueHandleContract<'db> {
    pub fn for_ty(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        ty: TyId<'db>,
    ) -> Result<Option<Self>, UnresolvedCapability<'db>> {
        if let Some(target_ty) = ty.as_ptr(db) {
            return Ok(Some(Self {
                handle_ty: ty,
                target_ty,
                address_space: HandleAddressSpace::Known(ProviderAddressSpace::Memory),
            }));
        }
        match resolve_effect_handle(db, scope, assumptions, ty) {
            EffectHandleResolution::NotHandle => Ok(None),
            EffectHandleResolution::Resolved {
                impl_instance,
                target_ty,
                ..
            } => {
                let inst = impl_instance.trait_inst();
                let name = IdentId::new(db, "SPACE");
                let expected = inst
                    .def(db)
                    .const_(db, name)
                    .and_then(|constant| constant.ty_binder(db))
                    .map(|binder| binder.instantiate(db, inst.args(db)))
                    .ok_or(UnresolvedCapability(ty))?;
                let value = const_ty_or_abstract_from_assoc_const_use(
                    db,
                    AssocConstUse::new(scope, assumptions, inst, name),
                    expected,
                )
                .ok_or(UnresolvedCapability(ty))?;
                let address_space = HandleAddressSpace::from_const(db, scope, value);
                Ok(Some(Self {
                    handle_ty: ty,
                    target_ty,
                    address_space,
                }))
            }
            EffectHandleResolution::Invalid(_) => Err(UnresolvedCapability(ty)),
        }
    }
}
