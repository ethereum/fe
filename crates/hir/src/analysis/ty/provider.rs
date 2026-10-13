use common::indexmap::IndexMap;
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{SemConstValue, sem_const_from_ty},
        ty::{
            corelib::resolve_lib_type_path,
            normalize::normalize_ty,
            trait_def::{ResolvedImplInstance, TraitInstId, resolve_trait_impl_instance},
            trait_resolution::{PredicateListId, Selection, TraitSolveCx},
            ty_check::EffectParamSite,
            ty_def::TyId,
        },
    },
    hir_def::{Contract, Func, IdentId, scope_graph::ScopeId},
};

use super::resolve_default_root_effect_ty;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub enum AddressSpace {
    Memory,
    Storage,
    Transient,
    Calldata,
    Code,
}

impl AddressSpace {
    pub fn is_byte_addressed(self) -> bool {
        matches!(self, Self::Memory | Self::Calldata | Self::Code)
    }

    pub fn pretty(self) -> &'static str {
        match self {
            Self::Memory => "memory",
            Self::Storage => "storage",
            Self::Transient => "transient storage",
            Self::Calldata => "calldata",
            Self::Code => "code",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderKind {
    /// The provider is an object owned by the root, passed by value.
    RootObject,
    /// The provider names a place: a capability's target or a contract
    /// field.
    Place,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderTransport {
    ByValue,
    ByPlace,
    ByTempPlace,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ProviderSemantics<'db> {
    pub provider_ty: TyId<'db>,
    pub kind: ProviderKind,
    pub address_space: Option<AddressSpace>,
    pub target_ty: Option<TyId<'db>>,
    pub transport: ProviderTransport,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum RootProviderSiteKind {
    Func,
    Contract,
    ContractInit,
    ContractRecvArm,
}

/// The item whose root-provider registrations a root provider comes from.
///
/// Every init or recv-arm site of a contract shares the contract's
/// registrations, so a root provider's identity names the contract rather than
/// the arm that materialized it. Callees specialized to it do not depend on the
/// arm.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum RootProviderScope<'db> {
    Func(Func<'db>),
    Contract(Contract<'db>),
}

impl<'db> RootProviderScope<'db> {
    pub fn of_site(site: EffectParamSite<'db>) -> Self {
        match site {
            EffectParamSite::Func(func) => Self::Func(func),
            EffectParamSite::Contract(contract)
            | EffectParamSite::ContractInit { contract }
            | EffectParamSite::ContractRecvArm { contract, .. } => Self::Contract(contract),
            EffectParamSite::Closure(_) => unreachable!("a closure has no root providers"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct RootProviderRegistration<'db> {
    pub idx: u32,
    pub site_kind: RootProviderSiteKind,
    pub provider_ty: TyId<'db>,
}

impl RootProviderRegistration<'_> {
    /// A function's root providers are numbered down from the top, clear of
    /// its effects' providers, which share their effects' slots.
    pub fn func_provider_idx(&self) -> u32 {
        u32::MAX - self.idx
    }
}

pub fn registered_root_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
) -> &'db [RootProviderRegistration<'db>] {
    match site {
        EffectParamSite::Func(func) => registered_root_providers_for_func(db, func).as_slice(),
        EffectParamSite::Closure(_) => &[],
        EffectParamSite::Contract(contract) => {
            registered_root_providers_for_contract(db, contract, RootProviderSiteKind::Contract)
                .as_slice()
        }
        EffectParamSite::ContractInit { contract } => {
            registered_root_providers_for_contract(db, contract, RootProviderSiteKind::ContractInit)
                .as_slice()
        }
        EffectParamSite::ContractRecvArm { contract, .. } => {
            registered_root_providers_for_contract(
                db,
                contract,
                RootProviderSiteKind::ContractRecvArm,
            )
            .as_slice()
        }
    }
}

#[salsa::tracked(return_ref)]
fn registered_root_providers_for_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<RootProviderRegistration<'db>> {
    registered_root_providers_for_scope(db, RootProviderSiteKind::Func, func.scope())
}

#[salsa::tracked(return_ref)]
fn registered_root_providers_for_contract<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    site_kind: RootProviderSiteKind,
) -> Vec<RootProviderRegistration<'db>> {
    registered_root_providers_for_scope(db, site_kind, contract.scope())
}

fn registered_root_providers_for_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    site_kind: RootProviderSiteKind,
    scope: ScopeId<'db>,
) -> Vec<RootProviderRegistration<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    let Some(provider_ty) = resolve_default_root_effect_ty(db, scope, assumptions) else {
        return Vec::new();
    };
    vec![RootProviderRegistration {
        idx: 0,
        site_kind,
        provider_ty,
    }]
}

pub fn provider_semantics<'db>(
    db: &'db dyn HirAnalysisDb,
    provider_ty: TyId<'db>,
) -> ProviderSemantics<'db> {
    let (kind, target_ty) = match provider_ty.as_capability(db) {
        Some((_, inner)) => (ProviderKind::Place, Some(inner)),
        None => (ProviderKind::RootObject, None),
    };
    ProviderSemantics {
        provider_ty,
        kind,
        address_space: Some(AddressSpace::Memory),
        target_ty,
        transport: ProviderTransport::ByValue,
    }
}

pub fn provider_semantics_for_specialized_call<'db>(
    db: &'db dyn HirAnalysisDb,
    provider_ty: TyId<'db>,
    target_ty: Option<TyId<'db>>,
    address_space: Option<AddressSpace>,
    transport: ProviderTransport,
) -> ProviderSemantics<'db> {
    let mut semantics = provider_semantics(db, provider_ty);
    // A capability names its target's place, and so does a root object
    // specialized to its own type.
    if provider_ty.as_capability(db).is_some() || target_ty == Some(provider_ty) {
        if let Some(target_ty) = target_ty {
            semantics.kind = ProviderKind::Place;
            semantics.target_ty = Some(target_ty);
        }
        if let Some(address_space) = address_space {
            semantics.address_space = Some(address_space);
        }
    }
    semantics.transport = transport;
    semantics
}

/// The address space the elements of a collection whose elements are places
/// (`core::ops::StateIndex`) live in: its implementation's `SPACE`.
#[salsa::tracked]
pub fn state_index_space<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    collection_ty: TyId<'db>,
) -> Option<AddressSpace> {
    let state_index = super::corelib::resolve_core_trait(db, scope, &["ops", "StateIndex"])?;
    let inst = TraitInstId::new(db, state_index, vec![collection_ty], IndexMap::new());
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    let Selection::Unique(resolved) = resolve_trait_impl_instance(db, solve_cx, inst) else {
        return None;
    };
    effect_space_from_resolved_trait_const(db, scope, resolved)
}

/// How a collection whose elements are places (`core::ops::StateIndex`)
/// packs its elements into lanes: its `Lanes` codec, when that codes the
/// element type (`LaneCodec<Entry>`) in at most 128 bits. An element of a
/// wider codec takes whole slots.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct StateIndexLanes<'db> {
    pub codec: TyId<'db>,
    pub bits: u16,
}

#[salsa::tracked]
pub fn state_index_lanes<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    collection_ty: TyId<'db>,
) -> Option<StateIndexLanes<'db>> {
    let state_index = super::corelib::resolve_core_trait(db, scope, &["ops", "StateIndex"])?;
    let inst = TraitInstId::new_simple(db, state_index, vec![collection_ty]);
    let assoc = |name: &str| {
        let projected = inst
            .trait_ref(db)
            .project_assoc_ty(db, IdentId::new(db, name.to_string()))?;
        Some(normalize_ty(db, projected, scope, assumptions))
    };
    let (codec, entry) = (assoc("Lanes")?, assoc("Entry")?);
    if codec == TyId::unit(db) {
        return None;
    }
    let lane_codec = super::corelib::resolve_core_trait(db, scope, &["ops", "LaneCodec"])?;
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    let Selection::Unique(resolved) = resolve_trait_impl_instance(
        db,
        solve_cx,
        TraitInstId::new_simple(db, lane_codec, vec![codec, entry]),
    ) else {
        return None;
    };
    let bits = super::const_ty::const_ty_from_resolved_trait_const(
        db,
        resolved,
        IdentId::new(db, "BITS".to_string()),
    )?
    .evaluate(db, Some(TyId::u256(db)))
    .integer_value(db)?;
    let bits = u16::try_from(bits).ok()?;
    (bits > 0 && bits <= 128).then_some(StateIndexLanes { codec, bits })
}

pub(crate) fn effect_space_from_resolved_trait_const<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    resolved: ResolvedImplInstance<'db>,
) -> Option<AddressSpace> {
    let space_ident = IdentId::new(db, "SPACE".to_string());
    let const_ty = super::const_ty::const_ty_from_resolved_trait_const(db, resolved, space_ident)?;
    effect_space_from_const_ty(db, scope, const_ty)
}

pub(crate) fn effect_space_from_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    const_ty: super::const_ty::ConstTyId<'db>,
) -> Option<AddressSpace> {
    let evaluated = const_ty.evaluate(db, None);
    let value = sem_const_from_ty(db, TyId::const_ty(db, evaluated))?;
    let SemConstValue::Enum { ty, variant, .. } = value.value(db) else {
        return None;
    };
    let enum_ = ty.as_enum(db)?;

    // A `StateIndex` collection's `StateSpace`.
    if resolve_lib_type_path(db, scope, "core::effect_ref::StateSpace")
        .and_then(|space| space.adt_def(db))
        .is_none_or(|space| super::adt_def::AdtRef::Enum(enum_) != space.adt_ref(db))
    {
        return None;
    }

    match enum_
        .variants(db)
        .nth(variant.0 as usize)?
        .name(db)?
        .data(db)
        .as_str()
    {
        "Storage" => Some(AddressSpace::Storage),
        "TransientStorage" => Some(AddressSpace::Transient),
        _ => None,
    }
}
