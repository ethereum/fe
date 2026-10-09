use crate::hir_def::{
    Enum, GenericParamOwner, IdentId, ItemKind, Partial, Struct, TypeId as HirTyId, VariantKind,
    scope_graph::ScopeId,
};
use crate::span::DynLazySpan;
use common::ingot::Ingot;
use salsa::Update;

use super::{
    binder::Binder,
    const_ty::ConstBodyLowering,
    corelib::resolve_core_trait,
    provider::{ProviderAddressSpace, effect_space_from_resolved_trait_const},
    trait_def::{ResolvedImplInstance, impls_for_trait_def},
    trait_resolution::{PredicateListId, constraint::collect_constraints},
    ty_def::{InvalidCause, PrimTy, TyBase, TyData, TyId},
    ty_lower::{
        CompleteSubst, GenericParamTypeSet, ParamDomainId, ParamSchemaId, lower_hir_ty_in_mode,
    },
};
use crate::analysis::HirAnalysisDb;

/// Represents a ADT type definition.
#[salsa::tracked]
#[derive(Debug)]
pub struct AdtDef<'db> {
    pub adt_ref: AdtRef<'db>,

    /// Type parameters of the ADT.
    #[return_ref]
    pub param_set: GenericParamTypeSet<'db>,

    /// Fields of the ADT, if the ADT is an enum, this represents variants.
    /// Otherwise, `fields[0]` represents all fields of the struct.
    #[return_ref]
    pub fields: Vec<AdtField<'db>>,
}

/// The state space `adt` is pinned to: the `SPACE` of its
/// `core::ops::PlaceIndex` implementation when that is storage or transient
/// storage. Implementing the trait with a state space is the pinning
/// declaration; an implementation with `SPACE = memory` leaves the type an
/// ordinary value.
#[salsa::tracked(cycle_fn = adt_pin_cycle_recover, cycle_initial = adt_pin_cycle_initial)]
pub(crate) fn adt_pin<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
) -> Option<ProviderAddressSpace> {
    let scope = adt.scope(db);
    let place_index = resolve_core_trait(db, scope, &["ops", "PlaceIndex"])?;
    impls_for_trait_def(db, adt.ingot(db), place_index)
        .iter()
        .filter(|implementor| implementor.self_ty(db).adt_def(db) == Some(adt))
        .find_map(|implementor| {
            let resolved = ResolvedImplInstance::identity(db, *implementor);
            effect_space_from_resolved_trait_const(db, scope, resolved).filter(|space| {
                matches!(
                    space,
                    ProviderAddressSpace::Storage | ProviderAddressSpace::Transient
                )
            })
        })
}

fn adt_pin_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: AdtDef<'db>,
) -> Option<ProviderAddressSpace> {
    None
}

fn adt_pin_cycle_recover<'db>(
    _: &'db dyn HirAnalysisDb,
    _: &Option<ProviderAddressSpace>,
    _: u32,
    _: AdtDef<'db>,
) -> salsa::CycleRecoveryAction<Option<ProviderAddressSpace>> {
    salsa::CycleRecoveryAction::Iterate
}

/// The pinned type `ty` is or holds in a field, or in a tuple or array
/// element; see `TyId::pinned_part`.
#[salsa::tracked(cycle_fn = ty_pinned_part_cycle_recover, cycle_initial = ty_pinned_part_cycle_initial)]
pub(crate) fn ty_pinned_part<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<TyId<'db>> {
    match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => {
            if adt.pin(db).is_some() {
                return Some(ty);
            }
            // A type not applied to all its arguments has no values.
            let args = ty.generic_args(db);
            if args.len() != adt.params(db).len() {
                return None;
            }
            adt.fields(db)
                .iter()
                .flat_map(|variant| variant.iter_types(db))
                .find_map(|field| field.instantiate(db, args).pinned_part(db))
        }
        TyData::TyBase(TyBase::Prim(PrimTy::Tuple(_) | PrimTy::Array)) => ty
            .generic_args(db)
            .iter()
            .find_map(|elem| elem.pinned_part(db)),
        _ => None,
    }
}

fn ty_pinned_part_cycle_initial<'db>(_: &'db dyn HirAnalysisDb, _: TyId<'db>) -> Option<TyId<'db>> {
    None
}

fn ty_pinned_part_cycle_recover<'db>(
    _: &'db dyn HirAnalysisDb,
    _: &Option<TyId<'db>>,
    _: u32,
    _: TyId<'db>,
) -> salsa::CycleRecoveryAction<Option<TyId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

impl<'db> AdtDef<'db> {
    pub(crate) fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        self.adt_ref(db).name(db)
    }

    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> DynLazySpan<'db> {
        self.adt_ref(db).name_span(db)
    }

    pub(crate) fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).params(db)
    }

    /// The state space this type is pinned to; see `adt_pin`.
    pub fn pin(self, db: &'db dyn HirAnalysisDb) -> Option<ProviderAddressSpace> {
        adt_pin(db, self)
    }

    pub(crate) fn is_struct(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.adt_ref(db), AdtRef::Struct(_))
    }

    pub fn scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.adt_ref(db).scope()
    }

    pub(crate) fn variant_ty_span(
        self,
        db: &'db dyn HirAnalysisDb,
        field_idx: usize,
        ty_idx: usize,
    ) -> DynLazySpan<'db> {
        match self.adt_ref(db) {
            AdtRef::Enum(e) => {
                let span = e.variant_span(field_idx);
                match e
                    .variants(db)
                    .nth(field_idx)
                    .expect("variant not found")
                    .kind(db)
                {
                    VariantKind::Tuple(_) => span.tuple_type().elem_ty(ty_idx).into(),
                    VariantKind::Record(_) => span.fields().field(ty_idx).ty().into(),
                    VariantKind::Unit => unreachable!(),
                }
            }

            AdtRef::Struct(s) => s.span().fields().field(ty_idx).ty().into(),
        }
    }

    pub(crate) fn ingot(self, db: &'db dyn HirAnalysisDb) -> Ingot<'db> {
        match self.adt_ref(db) {
            AdtRef::Enum(e) => e.top_mod(db).ingot(db),
            AdtRef::Struct(s) => s.top_mod(db).ingot(db),
        }
    }

    pub(crate) fn as_generic_param_owner(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> GenericParamOwner<'db> {
        self.adt_ref(db).generic_owner()
    }
}

/// This struct represents a field of an ADT. If the ADT is an enum, this
/// represents a variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtField<'db> {
    /// Field types as HIR type refs. To allow recursive types, these are kept
    /// at the HIR level and lowered on demand.
    tys: Vec<Partial<HirTyId<'db>>>,

    /// Scope of the containing ADT item.
    scope: ScopeId<'db>,
}
impl<'db> AdtField<'db> {
    fn assumptions(&self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        match self.scope {
            ScopeId::Item(ItemKind::Struct(struct_)) => {
                collect_constraints(db, GenericParamOwner::Struct(struct_)).instantiate_identity()
            }
            ScopeId::Item(ItemKind::Enum(enum_)) => {
                collect_constraints(db, GenericParamOwner::Enum(enum_)).instantiate_identity()
            }
            _ => PredicateListId::empty_list(db),
        }
    }

    pub fn ty(&self, db: &'db dyn HirAnalysisDb, i: usize) -> Binder<'db, TyId<'db>> {
        self.ty_in_mode(db, i, ConstBodyLowering::Eager)
    }

    /// Deferred const bodies keep anonymous bodies and their captures for
    /// concrete-demand diagnostics.
    fn ty_in_mode(
        &self,
        db: &'db dyn HirAnalysisDb,
        i: usize,
        const_bodies: ConstBodyLowering,
    ) -> Binder<'db, TyId<'db>> {
        let ty = if let Some(hir_ty) = self.tys[i].to_opt() {
            lower_hir_ty_in_mode(db, hir_ty, self.scope, self.assumptions(db), const_bodies)
        } else {
            TyId::invalid(db, InvalidCause::ParseError)
        };

        match GenericParamOwner::from_item_opt(self.scope.item()) {
            Some(owner) => Binder::bind(owner, ty),
            None => Binder::closed(ty),
        }
    }

    /// Iterates all field types of this variant.
    pub fn iter_types<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<'db, TyId<'db>>> + 'a {
        (0..self.num_types()).map(move |i| self.ty(db, i))
    }

    pub fn num_types(&self) -> usize {
        self.tys.len()
    }

    pub(crate) fn new(tys: Vec<Partial<HirTyId<'db>>>, scope: ScopeId<'db>) -> Self {
        Self { tys, scope }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, salsa::Supertype, Update)]
pub enum AdtRef<'db> {
    Enum(Enum<'db>),
    Struct(Struct<'db>),
}

impl<'db> AdtRef<'db> {
    pub fn try_from_item(item: ItemKind<'db>) -> Option<Self> {
        match item {
            ItemKind::Enum(x) => Some(x.into()),
            ItemKind::Struct(x) => Some(x.into()),
            _ => None,
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            Self::Enum(e) => e.scope(),
            Self::Struct(s) => s.scope(),
        }
    }

    pub fn as_item(self) -> ItemKind<'db> {
        match self {
            AdtRef::Enum(e) => e.into(),
            AdtRef::Struct(s) => s.into(),
        }
    }

    pub fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        match self {
            AdtRef::Enum(e) => e.name(db),
            AdtRef::Struct(s) => s.name(db),
        }
        .to_opt()
    }

    pub fn kind_name(self) -> &'static str {
        self.as_item().kind_name()
    }

    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> DynLazySpan<'db> {
        self.scope()
            .name_span(db)
            .unwrap_or_else(DynLazySpan::invalid)
    }

    pub fn is_must_use(self, db: &'db dyn HirAnalysisDb) -> bool {
        match self {
            AdtRef::Enum(enum_) => enum_.is_must_use(db),
            AdtRef::Struct(struct_) => struct_.is_must_use(db),
        }
    }

    pub fn is_view(self, db: &'db dyn HirAnalysisDb) -> bool {
        match self {
            AdtRef::Enum(enum_) => enum_.is_view(db),
            AdtRef::Struct(struct_) => struct_.is_view(db),
        }
    }

    /// Returns the semantic ADT definition for this reference.
    /// Thin wrapper over the tracked `lower_adt` query for ergonomic use at call sites.
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> AdtDef<'db> {
        crate::core::adt_lower::lower_adt(db, self)
    }

    pub(crate) fn generic_owner(self) -> GenericParamOwner<'db> {
        match self {
            AdtRef::Enum(e) => e.into(),
            AdtRef::Struct(s) => s.into(),
        }
    }
}

/// Struct for downstream diagnostics that refer to cycle members.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtCycleMember<'db> {
    pub adt: AdtDef<'db>,
    pub field_idx: usize,
    pub ty_idx: usize,
}

/// Instantiates an ADT field's type with `explicit_args`; omitted trailing
/// arguments keep their declaration formals.
pub fn instantiate_adt_field_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    variant_idx: usize,
    field_idx: usize,
    explicit_args: &[TyId<'db>],
) -> TyId<'db> {
    instantiate_adt_field_shape_in_mode(
        db,
        adt,
        variant_idx,
        field_idx,
        explicit_args,
        ConstBodyLowering::Eager,
    )
}

pub(crate) fn instantiate_adt_field_for_concrete_demand<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    variant_idx: usize,
    field_idx: usize,
    canonical_args: &[TyId<'db>],
    source_args: &[TyId<'db>],
) -> ConcreteTypeView<'db> {
    ConcreteTypeView {
        canonical: instantiate_adt_field_shape_in_mode(
            db,
            adt,
            variant_idx,
            field_idx,
            canonical_args,
            ConstBodyLowering::Eager,
        ),
        source: instantiate_adt_field_source_for_concrete_demand(
            db,
            adt,
            variant_idx,
            field_idx,
            source_args,
        ),
    }
}

/// The canonical field type supplies layout identity; the source view keeps
/// anonymous const bodies and their captures available for concrete errors.
#[derive(Clone, Copy)]
pub(crate) struct ConcreteTypeView<'db> {
    pub canonical: TyId<'db>,
    pub source: TyId<'db>,
}

impl<'db> ConcreteTypeView<'db> {
    pub(crate) fn new(canonical: TyId<'db>, source: TyId<'db>) -> Self {
        Self { canonical, source }
    }

    pub(crate) fn identity(ty: TyId<'db>) -> Self {
        Self::new(ty, ty)
    }
}

pub(crate) fn instantiate_adt_field_source_for_concrete_demand<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    variant_idx: usize,
    field_idx: usize,
    source_args: &[TyId<'db>],
) -> TyId<'db> {
    instantiate_adt_field_shape_in_mode(
        db,
        adt,
        variant_idx,
        field_idx,
        source_args,
        ConstBodyLowering::Deferred,
    )
}

fn instantiate_adt_field_shape_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    variant_idx: usize,
    field_idx: usize,
    explicit_args: &[TyId<'db>],
    const_bodies: ConstBodyLowering,
) -> TyId<'db> {
    let domain = ParamDomainId::full(db, ParamSchemaId::full(db, adt.as_generic_param_owner(db)));
    adt.fields(db)
        .get(variant_idx)
        .filter(|variant| field_idx < variant.num_types() && explicit_args.len() <= domain.len(db))
        .map_or_else(
            || TyId::invalid(db, InvalidCause::Other),
            |variant| {
                // Omitted trailing arguments keep their declaration formals.
                let subst = CompleteSubst::with_prefix(db, domain, explicit_args);
                variant
                    .ty_in_mode(db, field_idx, const_bodies)
                    .instantiate_subst(db, &subst)
                    .expect("ADT field uses its declaration domain")
            },
        )
}
