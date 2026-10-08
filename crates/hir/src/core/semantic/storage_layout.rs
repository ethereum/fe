use common::{
    indexmap::IndexMap,
    layout::{StorageFieldShape, StorageLane, storage_fields_layout},
};
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use ruint::aliases::U256;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{RuntimeSizeError, runtime_size_bytes_with_source},
        ty::{
            ProviderAddressSpace,
            adt_def::{
                AdtDef, AdtRef, ConcreteTypeView, instantiate_adt_field_for_concrete_demand,
            },
            const_ty::{ConcreteArrayLengthError, demand_concrete_array_length},
            provider::{
                ProviderLayoutFailure, ProviderLayoutResolution, place_index_space,
                resolve_effect_handle_layout,
            },
            trait_def::ImplementorId,
            trait_resolution::PredicateListId,
            ty_check::contract_field_slot,
            ty_def::{PrimTy, TyBase, TyData, TyId},
            ty_lower::lower_opt_hir_ty,
        },
    },
    hir_def::{
        Contract, EnumVariant, FieldParent, IdentId, IntegerId, VariantKind, scope_graph::ScopeId,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct ContractFieldId<'db> {
    pub contract: Contract<'db>,
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ContractLayoutReport<'db> {
    pub entries: Vec<ContractLayoutEntry<'db>>,
}

impl<'db> ContractLayoutReport<'db> {
    pub fn entries_for_field(
        &self,
        field: ContractFieldId<'db>,
    ) -> impl Iterator<Item = &ContractLayoutEntry<'db>> {
        self.entries
            .iter()
            .filter(move |entry| entry.field == field)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ContractLayoutEntry<'db> {
    pub field: ContractFieldId<'db>,
    pub path: ContractLayoutPath<'db>,
    pub ty: TyId<'db>,
    pub address_space: ProviderAddressSpace,
    pub value: ContractLayoutValue<'db>,
    /// For a scalar packed with others into one slot, its bytes in the slot.
    pub lane: Option<ContractLayoutLane>,
    pub kind: ContractLayoutEntryKind,
}

/// The bytes of a storage slot that hold a packed scalar, counted from the
/// low-order end like Solidity's `offset`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ContractLayoutLane {
    pub byte_offset: u8,
    pub byte_width: u8,
}

impl From<StorageLane> for ContractLayoutLane {
    fn from(lane: StorageLane) -> Self {
        Self {
            byte_offset: lane.byte_offset as u8,
            byte_width: lane.byte_width as u8,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutEntryKind {
    InlineField,
    /// A storage collection, whose place is its identity.
    Collection,
    EnumTag,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutValue<'db> {
    Scalar(IntegerId<'db>),
    Indexed {
        base: IntegerId<'db>,
        dimensions: Vec<usize>,
        strides: Vec<usize>,
        extent: usize,
    },
}

impl<'db> ContractLayoutValue<'db> {
    pub fn base(&self) -> IntegerId<'db> {
        match self {
            Self::Scalar(value) => *value,
            Self::Indexed { base, .. } => *base,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ContractLayoutPath<'db> {
    pub field: IdentId<'db>,
    pub segments: Vec<ContractLayoutPathSegment<'db>>,
}

impl<'db> ContractLayoutPath<'db> {
    pub fn display(&self, db: &'db dyn HirAnalysisDb) -> String {
        let mut path = self.field.data(db).to_string();
        for segment in &self.segments {
            match segment {
                ContractLayoutPathSegment::Member { name, .. } => {
                    path.push('.');
                    path.push_str(name.data(db));
                }
                ContractLayoutPathSegment::TupleElement(index) => {
                    path.push('.');
                    path.push_str(&index.to_string());
                }
                ContractLayoutPathSegment::Variant { name, .. } => {
                    path.push_str("::");
                    path.push_str(name.data(db));
                }
                ContractLayoutPathSegment::ArrayElement { dimension, .. } => {
                    path.push_str(&format!("[i{dimension}]"));
                }
                ContractLayoutPathSegment::EnumTag => path.push_str(".<tag>"),
            }
        }
        path
    }

    pub fn index_dimensions(&self) -> impl Iterator<Item = (u32, usize)> + '_ {
        self.segments.iter().filter_map(|segment| match segment {
            ContractLayoutPathSegment::ArrayElement { dimension, len } => Some((*dimension, *len)),
            _ => None,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutPathSegment<'db> {
    Member { name: IdentId<'db>, index: u32 },
    TupleElement(u32),
    Variant { name: IdentId<'db>, index: u32 },
    ArrayElement { dimension: u32, len: usize },
    EnumTag,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutError<'db> {
    InvalidFieldType,
    InvalidConcreteArrayLength {
        invalid: TyId<'db>,
    },
    InconsistentConcreteArrayLength {
        array: TyId<'db>,
    },
    AmbiguousProviderLayout,
    UnresolvedProviderTarget,
    UnresolvedProviderSpace,
    InvalidProviderRaw {
        failure: ProviderLayoutFailure,
    },
    NonRegularProviderCycle,
    /// `#[slot(e)]` whose `e` is not a constant `u256`.
    InvalidExplicitSlot,
    /// `#[slot(e)]` on a field that lives in code, which has no slots.
    ExplicitSlotInCode,
    /// `#[slot(e)]` whose slots overlap those of the explicitly placed `other`.
    ExplicitSlotOverlap {
        other: IdentId<'db>,
    },
    LayoutExtentOverflow,
    IncompleteAdtLayoutProjection {
        ty: TyId<'db>,
    },
}

impl ContractLayoutError<'_> {
    pub fn summary(&self) -> &'static str {
        match self {
            Self::InvalidFieldType => "field type is invalid",
            Self::InvalidConcreteArrayLength { .. } => "array length const evaluation failed",
            Self::InconsistentConcreteArrayLength { .. } => {
                "canonical and source array lengths disagree"
            }
            Self::AmbiguousProviderLayout => "provider layout selection is ambiguous",
            Self::UnresolvedProviderTarget => "provider target type is unresolved",
            Self::UnresolvedProviderSpace => "provider address space is unresolved",
            Self::InvalidProviderRaw { .. } => "provider raw transport is invalid",
            Self::NonRegularProviderCycle => "provider target recursion changes its type arguments",
            Self::InvalidExplicitSlot => "the explicit slot is not a constant `u256`",
            Self::ExplicitSlotInCode => "a code field has no slot",
            Self::ExplicitSlotOverlap { .. } => "the explicit slots overlap another field's",
            Self::LayoutExtentOverflow => "layout extent overflowed",
            Self::IncompleteAdtLayoutProjection { .. } => "layout projection is incomplete",
        }
    }
}

/// A contract field's layout before the contract places it: its address
/// space, the slots it spans and the parts that take them.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ValidatedFieldLayoutPlan<'db> {
    field: ContractFieldId<'db>,
    name: IdentId<'db>,
    is_mut: bool,
    is_provider: bool,
    address_space: ProviderAddressSpace,
    declared: TyId<'db>,
    target: TyId<'db>,
    /// The slot `#[slot(e)]` places the field at.
    explicit_slot: Option<U256>,
    slot_count: usize,
    /// The spaces the field's slot numbers are taken in: its own, and those
    /// of the collections in it, which keep their contents at their own
    /// slot numbers there (a `TSlot`'s value in transient storage).
    spaces: Vec<ProviderAddressSpace>,
    inline_leaves: Vec<InlineLayoutLeaf<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct FieldStorageLayout<'db> {
    pub field: ContractFieldId<'db>,
    pub name: IdentId<'db>,
    pub is_mut: bool,
    pub is_provider: bool,
    pub address_space: ProviderAddressSpace,
    /// The field's declared type.
    pub declared: TyId<'db>,
    /// The value the field holds: a provider's target, else `declared`.
    pub target: TyId<'db>,
    /// The field's first slot, or for a code field its first word.
    pub slot_offset: U256,
    pub slot_count: usize,
    inline_leaves: Vec<InlineLayoutLeaf<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct AllocatedContractStorageLayout<'db> {
    pub fields: IndexMap<IdentId<'db>, FieldStorageLayout<'db>>,
    /// The words the code fields take.
    pub code_slot_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ContractFieldLayoutResult<'db> {
    pub field: ContractFieldId<'db>,
    pub name: IdentId<'db>,
    pub result: Result<ValidatedFieldLayoutPlan<'db>, Vec<ContractLayoutError<'db>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ContractStorageLayoutResult<'db> {
    pub field_results: Vec<ContractFieldLayoutResult<'db>>,
    pub allocated: Option<AllocatedContractStorageLayout<'db>>,
}

impl<'db> ContractStorageLayoutResult<'db> {
    pub fn field(&self, name: &IdentId<'db>) -> Option<&FieldStorageLayout<'db>> {
        self.allocated.as_ref()?.fields.get(name)
    }

    pub fn field_errors(&self, name: &IdentId<'db>) -> Option<&[ContractLayoutError<'db>]> {
        self.field_results
            .iter()
            .find(|field| field.name == *name)?
            .result
            .as_ref()
            .err()
            .map(Vec::as_slice)
    }

    pub fn field_errors_for_id(
        &self,
        field: ContractFieldId<'db>,
    ) -> Option<&[ContractLayoutError<'db>]> {
        self.field_results
            .iter()
            .find(|result| result.field == field)?
            .result
            .as_ref()
            .err()
            .map(Vec::as_slice)
    }

    pub fn get(&self, name: &IdentId<'db>) -> Option<&FieldStorageLayout<'db>> {
        self.field(name)
    }

    pub fn values(&self) -> impl Iterator<Item = &FieldStorageLayout<'db>> {
        self.allocated
            .iter()
            .flat_map(|layout| layout.fields.values())
    }

    /// The fields whose layout is valid, by name: index, `is_mut`,
    /// `is_provider`, declared type and target.
    pub(crate) fn semantic_fields(
        &self,
    ) -> Vec<(
        IdentId<'db>,
        ContractFieldId<'db>,
        bool,
        bool,
        TyId<'db>,
        TyId<'db>,
    )> {
        self.field_results
            .iter()
            .filter_map(|field| {
                let plan = field.result.as_ref().ok()?;
                Some((
                    field.name,
                    plan.field,
                    plan.is_mut,
                    plan.is_provider,
                    plan.declared,
                    plan.target,
                ))
            })
            .collect()
    }
}

/// The bytes a scalar struct or tuple field takes in a packed storage slot:
/// booleans and integers narrower than a word, like Solidity's value types.
/// Everything else takes whole slots. Runtime lowering classifies scalars
/// the same way (`mir::runtime::storage_scalar_bytes`).
fn storage_packable_bytes(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<u32> {
    let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db) else {
        return None;
    };
    match prim {
        PrimTy::Bool | PrimTy::U8 | PrimTy::I8 => Some(1),
        PrimTy::U16 | PrimTy::I16 => Some(2),
        PrimTy::U32 | PrimTy::I32 => Some(4),
        PrimTy::U64 | PrimTy::I64 => Some(8),
        PrimTy::U128 | PrimTy::I128 => Some(16),
        _ => None,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
enum InlineLayoutLeafKind {
    Field,
    Collection,
    EnumTag,
}

/// A part of a field that takes slots: a scalar, a storage collection or an
/// enum tag, at `offset` slots from the field's first, repeated along each
/// enclosing array's dimension at its stride.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
struct InlineLayoutLeaf<'db> {
    path: Vec<ContractLayoutPathSegment<'db>>,
    ty: TyId<'db>,
    offset: usize,
    /// The bytes of slot `offset` holding a packed scalar.
    lane: Option<ContractLayoutLane>,
    dimensions: Vec<usize>,
    strides: Vec<usize>,
    kind: InlineLayoutLeafKind,
}

#[derive(Clone, Debug)]
struct WalkOutput<'db> {
    span: usize,
    leaves: Vec<InlineLayoutLeaf<'db>>,
    /// The bytes of a scalar that packs with its neighbours in a struct or
    /// tuple; `None` for values that take whole slots.
    packable_bytes: Option<u32>,
}

impl<'db> WalkOutput<'db> {
    fn empty() -> Self {
        Self {
            span: 0,
            leaves: Vec::new(),
            packable_bytes: None,
        }
    }

    fn leaf(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        path: &[ContractLayoutPathSegment<'db>],
        dimensions: &[usize],
        kind: InlineLayoutLeafKind,
    ) -> Self {
        Self {
            span: 1,
            leaves: vec![InlineLayoutLeaf {
                path: path.to_vec(),
                ty,
                offset: 0,
                lane: None,
                dimensions: dimensions.to_vec(),
                strides: vec![0; dimensions.len()],
                kind,
            }],
            packable_bytes: match kind {
                InlineLayoutLeafKind::Field => storage_packable_bytes(db, ty),
                InlineLayoutLeafKind::Collection | InlineLayoutLeafKind::EnumTag => None,
            },
        }
    }
}

/// Lays out one contract field's value: the slots each part takes, from
/// the field's first. A handle within the value takes the slots of its own
/// representation; its target lies wherever the handle points, so the walk
/// only checks that the target has a layout.
struct FieldWalker<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    errors: Vec<ContractLayoutError<'db>>,
    /// The spaces the collections walked keep their contents in.
    content_spaces: Vec<ProviderAddressSpace>,
    /// The handles whose targets are being checked, outermost first, with
    /// the implementation that selects each target.
    expanding: Vec<(TyId<'db>, ImplementorId<'db>)>,
}

/// How a handle's target relates to the targets being checked around it.
enum ProviderRecurrence {
    /// The handle is one already being checked: its target is checked there.
    BackEdge,
    /// The handle is new, or a finite rearrangement of one being checked.
    Expand,
    /// The same implementation recurs with growing arguments, which would
    /// make the targets an unbounded family of types.
    NonRegular,
}

/// Classifies handle `ty`, selected by `family`, against the handles being
/// checked. A repeated implementation is finite when `ty` permutes the
/// arguments of an earlier handle of it, or is one of its arguments.
fn provider_recurrence<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    family: ImplementorId<'db>,
    expanding: &[(TyId<'db>, ImplementorId<'db>)],
) -> ProviderRecurrence {
    fn is_subterm<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, ancestor: TyId<'db>) -> bool {
        ancestor
            .generic_args(db)
            .iter()
            .any(|arg| *arg == ty || is_subterm(db, ty, *arg))
    }
    fn is_permutation<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, ancestor: TyId<'db>) -> bool {
        let (base, args) = ty.decompose_ty_app(db);
        let (ancestor_base, ancestor_args) = ancestor.decompose_ty_app(db);
        let mut sorted = args.to_vec();
        let mut ancestor_sorted = ancestor_args.to_vec();
        sorted.sort();
        ancestor_sorted.sort();
        base == ancestor_base && sorted == ancestor_sorted
    }
    if expanding.iter().any(|(ancestor, _)| *ancestor == ty) {
        return ProviderRecurrence::BackEdge;
    }
    let mut same_family = expanding
        .iter()
        .filter(|(_, ancestor_family)| *ancestor_family == family)
        .peekable();
    if same_family.peek().is_some()
        && !same_family
            .any(|(ancestor, _)| is_permutation(db, ty, *ancestor) || is_subterm(db, ty, *ancestor))
    {
        ProviderRecurrence::NonRegular
    } else {
        ProviderRecurrence::Expand
    }
}

impl<'db> FieldWalker<'db> {
    fn push_error(&mut self, error: ContractLayoutError<'db>) {
        if self.errors.contains(&error) {
            return;
        }
        // A field's array length error is the one its diagnostic reports.
        let position = if matches!(
            error,
            ContractLayoutError::InvalidConcreteArrayLength { .. }
        ) {
            self.errors
                .iter()
                .position(|prior| {
                    !matches!(
                        prior,
                        ContractLayoutError::InvalidConcreteArrayLength { .. }
                    )
                })
                .unwrap_or(self.errors.len())
        } else {
            self.errors.len()
        };
        self.errors.insert(position, error);
    }

    fn walk_ty(
        &mut self,
        views: ConcreteTypeView<'db>,
        path: &[ContractLayoutPathSegment<'db>],
        dimensions: &[usize],
    ) -> WalkOutput<'db> {
        let ConcreteTypeView {
            canonical: ty,
            source,
        } = views;
        if let Some((kind, inner)) = ty.as_capability(self.db) {
            let Some((_, source_inner)) = source
                .as_capability(self.db)
                .filter(|(source_kind, _)| *source_kind == kind)
            else {
                self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
                return WalkOutput::empty();
            };
            self.walk_ty(ConcreteTypeView::new(inner, source_inner), path, dimensions)
        } else if let TyData::ConstTy(const_ty) = ty.data(self.db) {
            let TyData::ConstTy(source_const_ty) = source.data(self.db) else {
                self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
                return WalkOutput::empty();
            };
            self.walk_ty(
                ConcreteTypeView::new(const_ty.ty(self.db), source_const_ty.ty(self.db)),
                path,
                dimensions,
            )
        } else if ty.is_tuple(self.db) {
            let source_fields = source.field_types(self.db);
            let fields = ty.field_types(self.db);
            if !source.is_tuple(self.db) || fields.len() != source_fields.len() {
                self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
                return WalkOutput::empty();
            }
            let items = fields
                .into_iter()
                .zip(source_fields)
                .enumerate()
                .map(|(idx, (elem, source_elem))| {
                    (
                        ConcreteTypeView::new(elem, source_elem),
                        ContractLayoutPathSegment::TupleElement(idx as u32),
                    )
                })
                .collect();
            self.walk_sequence(items, path, dimensions, true)
        } else if ty.is_array(self.db) {
            self.walk_array(views, path, dimensions)
        } else if let Some(adt) = ty.adt_def(self.db) {
            self.check_provider_target(ty);
            let output = self.walk_adt(views, adt, path, dimensions);
            // A storage collection is reported at its place, which is its
            // identity, rather than by the slots its private fields take.
            if adt.adt_ref(self.db).is_storage_only(self.db) && output.span != 0 {
                if let Some(space) = place_index_space(
                    self.db,
                    self.scope,
                    PredicateListId::empty_list(self.db),
                    ty,
                ) && !self.content_spaces.contains(&space)
                {
                    self.content_spaces.push(space);
                }
                WalkOutput {
                    span: output.span,
                    ..WalkOutput::leaf(
                        self.db,
                        ty,
                        path,
                        dimensions,
                        InlineLayoutLeafKind::Collection,
                    )
                }
            } else {
                output
            }
        } else if ty.is_never(self.db)
            || ty.is_zero_sized(self.db)
            || matches!(
                ty.base_ty(self.db).data(self.db),
                TyData::TyBase(TyBase::Func(_) | TyBase::Contract(_))
            )
        {
            WalkOutput::empty()
        } else {
            WalkOutput::leaf(self.db, ty, path, dimensions, InlineLayoutLeafKind::Field)
        }
    }

    /// Checks that a handle within a field value has a target with a layout.
    fn check_provider_target(&mut self, ty: TyId<'db>) {
        let (impl_instance, target) = match resolve_effect_handle_layout(
            self.db,
            self.scope,
            PredicateListId::empty_list(self.db),
            ty,
        ) {
            ProviderLayoutResolution::NotHandle => return,
            ProviderLayoutResolution::Invalid(failure) => {
                self.push_error(contract_layout_error_for_provider_failure(failure));
                return;
            }
            ProviderLayoutResolution::Resolved {
                impl_instance,
                target,
                ..
            } => (impl_instance, target),
        };
        let family = impl_instance.selected();
        match provider_recurrence(self.db, ty, family, &self.expanding) {
            ProviderRecurrence::BackEdge => return,
            ProviderRecurrence::NonRegular => {
                self.push_error(ContractLayoutError::NonRegularProviderCycle);
                return;
            }
            ProviderRecurrence::Expand => {}
        }
        if target.has_invalid(self.db)
            || target.has_var(self.db)
            || ty_has_incomplete_adt_application(self.db, target)
        {
            self.push_error(ContractLayoutError::UnresolvedProviderTarget);
            return;
        }
        self.expanding.push((ty, family));
        self.walk_ty(ConcreteTypeView::identity(target), &[], &[]);
        self.expanding.pop();
    }

    /// Lays out a struct's or tuple's fields (`packed`) or an enum payload.
    /// Packed sequences place narrow scalars like Solidity places struct
    /// members, see `common::layout::storage_fields_layout`.
    fn walk_sequence(
        &mut self,
        items: Vec<(ConcreteTypeView<'db>, ContractLayoutPathSegment<'db>)>,
        path: &[ContractLayoutPathSegment<'db>],
        dimensions: &[usize],
        packed: bool,
    ) -> WalkOutput<'db> {
        let outputs: Vec<_> = items
            .into_iter()
            .map(|(views, segment)| {
                let mut item_path = path.to_vec();
                item_path.push(segment);
                self.walk_ty(views, &item_path, dimensions)
            })
            .collect();
        let placements = if packed {
            let shapes = outputs.iter().map(|output| match output.packable_bytes {
                Some(bytes) => StorageFieldShape::Scalar { bytes },
                None => StorageFieldShape::Aggregate {
                    slots: output.span as u64,
                },
            });
            match storage_fields_layout(shapes) {
                Ok(layout) => Some(layout),
                Err(_) => {
                    self.push_error(ContractLayoutError::LayoutExtentOverflow);
                    return WalkOutput::empty();
                }
            }
        } else {
            None
        };
        let mut span = 0usize;
        let mut leaves = Vec::new();
        for (idx, mut output) in outputs.into_iter().enumerate() {
            let (start, lane) = match &placements {
                Some(layout) => {
                    let placement = layout.placements[idx];
                    let Ok(slot) = usize::try_from(placement.slot) else {
                        self.push_error(ContractLayoutError::LayoutExtentOverflow);
                        continue;
                    };
                    (slot, placement.lane.map(ContractLayoutLane::from))
                }
                None => (span, None),
            };
            let Some(end) = start.checked_add(output.span) else {
                self.push_error(ContractLayoutError::LayoutExtentOverflow);
                continue;
            };
            for leaf in &mut output.leaves {
                let Some(offset) = leaf.offset.checked_add(start) else {
                    self.push_error(ContractLayoutError::LayoutExtentOverflow);
                    continue;
                };
                leaf.offset = offset;
                if lane.is_some() {
                    leaf.lane = lane;
                }
            }
            span = span.max(end);
            leaves.extend(output.leaves);
        }
        if let Some(layout) = &placements {
            let Ok(slots) = usize::try_from(layout.slots) else {
                self.push_error(ContractLayoutError::LayoutExtentOverflow);
                return WalkOutput::empty();
            };
            span = slots;
        }
        WalkOutput {
            span,
            leaves,
            packable_bytes: None,
        }
    }

    fn walk_array(
        &mut self,
        views: ConcreteTypeView<'db>,
        path: &[ContractLayoutPathSegment<'db>],
        dimensions: &[usize],
    ) -> WalkOutput<'db> {
        let ConcreteTypeView {
            canonical: ty,
            source,
        } = views;
        let (_, args) = ty.decompose_ty_app(self.db);
        let (_, source_args) = source.decompose_ty_app(self.db);
        if !source.is_array(self.db) || args.len() != 2 || source_args.len() != 2 {
            self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
            return WalkOutput::empty();
        }
        let element = ConcreteTypeView::new(args[0], source_args[0]);
        let len = match demand_concrete_array_length(self.db, args[1], source_args[1]) {
            Ok(len) => len.and_then(|len| len.to_usize()),
            Err(ConcreteArrayLengthError::Invalid(cause)) => {
                self.push_error(ContractLayoutError::InvalidConcreteArrayLength {
                    invalid: TyId::invalid(self.db, cause),
                });
                return WalkOutput::empty();
            }
            Err(ConcreteArrayLengthError::Mismatch) => {
                self.push_error(ContractLayoutError::InconsistentConcreteArrayLength { array: ty });
                return WalkOutput::empty();
            }
        };
        if (len.is_none() || len == Some(0))
            && let Err(RuntimeSizeError::InvalidType(cause)) =
                runtime_size_bytes_with_source(self.db, element)
        {
            self.push_error(ContractLayoutError::InvalidConcreteArrayLength {
                invalid: TyId::invalid(self.db, cause),
            });
            return WalkOutput::empty();
        }
        let Some(len) = len else {
            self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
            return WalkOutput::empty();
        };
        if len == 0 {
            return WalkOutput::empty();
        }
        let mut element_path = path.to_vec();
        element_path.push(ContractLayoutPathSegment::ArrayElement {
            dimension: dimensions.len() as u32,
            len,
        });
        let mut element_dimensions = dimensions.to_vec();
        element_dimensions.push(len);
        let mut output = self.walk_ty(element, &element_path, &element_dimensions);
        let Some(span) = output.span.checked_mul(len) else {
            self.push_error(ContractLayoutError::LayoutExtentOverflow);
            return WalkOutput::empty();
        };
        for leaf in &mut output.leaves {
            leaf.strides[dimensions.len()] = output.span;
        }
        WalkOutput {
            span,
            leaves: output.leaves,
            packable_bytes: None,
        }
    }

    fn walk_adt(
        &mut self,
        views: ConcreteTypeView<'db>,
        adt: AdtDef<'db>,
        path: &[ContractLayoutPathSegment<'db>],
        dimensions: &[usize],
    ) -> WalkOutput<'db> {
        let ConcreteTypeView {
            canonical: ty,
            source,
        } = views;
        if adt.recursive_cycle(self.db).is_some() {
            self.push_error(ContractLayoutError::InvalidFieldType);
            return WalkOutput::empty();
        }
        let args = ty.generic_args(self.db);
        let source_args = source.generic_args(self.db);
        if source.adt_def(self.db) != Some(adt)
            || args.len() != adt.params(self.db).len()
            || source_args.len() != args.len()
        {
            self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
            return WalkOutput::empty();
        }
        let db = self.db;
        let field = |variant: usize, field: usize| {
            instantiate_adt_field_for_concrete_demand(db, adt, variant, field, args, source_args)
        };
        match adt.adt_ref(self.db) {
            AdtRef::Struct(struct_) => {
                let mut items = Vec::new();
                for (idx, view) in FieldParent::Struct(struct_).fields(self.db).enumerate() {
                    let Some(name) = view.name(self.db) else {
                        self.push_error(ContractLayoutError::InvalidFieldType);
                        return WalkOutput::empty();
                    };
                    let index = idx as u32;
                    items.push((
                        field(0, idx),
                        ContractLayoutPathSegment::Member { name, index },
                    ));
                }
                self.walk_sequence(items, path, dimensions, true)
            }
            AdtRef::Enum(enum_) => {
                let mut tag_path = path.to_vec();
                tag_path.push(ContractLayoutPathSegment::EnumTag);
                let mut leaves = WalkOutput::leaf(
                    self.db,
                    ty,
                    &tag_path,
                    dimensions,
                    InlineLayoutLeafKind::EnumTag,
                )
                .leaves;
                let mut max_payload = 0usize;
                for (variant_idx, variant) in adt.fields(self.db).iter().enumerate() {
                    let variant_def = EnumVariant::new(enum_, variant_idx);
                    let Some(name) = variant_def.ident(self.db) else {
                        self.push_error(ContractLayoutError::InvalidFieldType);
                        return WalkOutput::empty();
                    };
                    let mut variant_path = path.to_vec();
                    variant_path.push(ContractLayoutPathSegment::Variant {
                        name,
                        index: variant_idx as u32,
                    });
                    let mut items = Vec::with_capacity(variant.num_types());
                    for field_idx in 0..variant.num_types() {
                        let index = field_idx as u32;
                        let segment = match variant_def.kind(self.db) {
                            VariantKind::Record(_) => {
                                let Some(name) = FieldParent::Variant(variant_def)
                                    .fields(self.db)
                                    .nth(field_idx)
                                    .and_then(|field| field.name(self.db))
                                else {
                                    self.push_error(ContractLayoutError::InvalidFieldType);
                                    return WalkOutput::empty();
                                };
                                ContractLayoutPathSegment::Member { name, index }
                            }
                            VariantKind::Tuple(_) | VariantKind::Unit => {
                                ContractLayoutPathSegment::TupleElement(index)
                            }
                        };
                        items.push((field(variant_idx, field_idx), segment));
                    }
                    // Enum payloads are not packed: runtime lowering offsets
                    // payload fields by whole words.
                    let mut output = self.walk_sequence(items, &variant_path, dimensions, false);
                    max_payload = max_payload.max(output.span);
                    for leaf in &mut output.leaves {
                        let Some(offset) = leaf.offset.checked_add(1) else {
                            self.push_error(ContractLayoutError::LayoutExtentOverflow);
                            continue;
                        };
                        leaf.offset = offset;
                    }
                    leaves.extend(output.leaves);
                }
                let span = max_payload.checked_add(1).unwrap_or_else(|| {
                    self.push_error(ContractLayoutError::LayoutExtentOverflow);
                    0
                });
                WalkOutput {
                    span,
                    leaves,
                    packable_bytes: None,
                }
            }
        }
    }
}

fn ty_has_incomplete_adt_application<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    fn inner<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        visiting: &mut FxHashSet<TyId<'db>>,
    ) -> bool {
        if !visiting.insert(ty) {
            return false;
        }
        let (base, args) = ty.decompose_ty_app(db);
        let incomplete = if let TyData::TyBase(TyBase::Adt(adt)) = base.data(db) {
            args.len() != adt.params(db).len()
        } else {
            false
        } || args.iter().any(|arg| inner(db, *arg, visiting));
        visiting.remove(&ty);
        incomplete
    }
    inner(db, ty, &mut FxHashSet::default())
}

fn affine_extent(dimensions: &[usize], strides: &[usize]) -> Option<usize> {
    dimensions
        .iter()
        .zip(strides)
        .try_fold(0usize, |offset, (dimension, stride)| {
            offset.checked_add(dimension.checked_sub(1)?.checked_mul(*stride)?)
        })?
        .checked_add(1)
}

fn collect_field_plan<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    field_index: u32,
) -> Option<(
    IdentId<'db>,
    Result<ValidatedFieldLayoutPlan<'db>, Vec<ContractLayoutError<'db>>>,
)> {
    let scope = contract.scope();
    let assumptions = PredicateListId::empty_list(db);
    let field = contract
        .hir_fields(db)
        .data(db)
        .iter()
        .filter(|field| field.name.is_present())
        .nth(field_index as usize)?;
    let name = field.name.unwrap();
    let declared = lower_opt_hir_ty(db, field.type_ref(), scope, assumptions);
    if declared.has_invalid(db) || ty_has_incomplete_adt_application(db, declared) {
        return Some((name, Err(vec![ContractLayoutError::InvalidFieldType])));
    }
    let default_space = if field.is_mut {
        ProviderAddressSpace::Storage
    } else {
        ProviderAddressSpace::Code
    };
    let (is_provider, address_space, target) =
        match resolve_effect_handle_layout(db, scope, assumptions, declared) {
            ProviderLayoutResolution::NotHandle => (false, default_space, declared),
            ProviderLayoutResolution::Resolved { target, space, .. } => {
                if target.has_invalid(db)
                    || target.has_var(db)
                    || ty_has_incomplete_adt_application(db, target)
                {
                    return Some((
                        name,
                        Err(vec![ContractLayoutError::UnresolvedProviderTarget]),
                    ));
                }
                (true, space, target)
            }
            ProviderLayoutResolution::Invalid(failure) => {
                return Some((
                    name,
                    Err(vec![contract_layout_error_for_provider_failure(failure)]),
                ));
            }
        };
    let explicit_slot = match field.slot {
        Some(_) if address_space == ProviderAddressSpace::Code => {
            return Some((name, Err(vec![ContractLayoutError::ExplicitSlotInCode])));
        }
        Some(body) => match contract_field_slot(db, body) {
            Ok(slot) => Some(slot),
            Err(_) => return Some((name, Err(vec![ContractLayoutError::InvalidExplicitSlot]))),
        },
        None => None,
    };
    let mut walker = FieldWalker {
        db,
        scope,
        errors: Vec::new(),
        content_spaces: Vec::new(),
        expanding: Vec::new(),
    };
    let output = walker.walk_ty(ConcreteTypeView::identity(target), &[], &[]);
    if !walker.errors.is_empty() {
        return Some((name, Err(walker.errors)));
    }
    let mut spaces = vec![address_space];
    spaces.extend(
        walker
            .content_spaces
            .into_iter()
            .filter(|space| *space != address_space),
    );
    Some((
        name,
        Ok(ValidatedFieldLayoutPlan {
            field: ContractFieldId {
                contract,
                index: field_index,
            },
            name,
            is_mut: field.is_mut,
            is_provider,
            address_space,
            declared,
            target,
            explicit_slot,
            slot_count: output.span,
            spaces,
            inline_leaves: output.leaves,
        }),
    ))
}

fn contract_layout_error_for_provider_failure<'db>(
    failure: ProviderLayoutFailure,
) -> ContractLayoutError<'db> {
    match failure {
        ProviderLayoutFailure::Ambiguous => ContractLayoutError::AmbiguousProviderLayout,
        ProviderLayoutFailure::UnresolvedTarget => ContractLayoutError::UnresolvedProviderTarget,
        ProviderLayoutFailure::UnresolvedSpace => ContractLayoutError::UnresolvedProviderSpace,
        ProviderLayoutFailure::UnresolvedRaw
        | ProviderLayoutFailure::UnsupportedRaw
        | ProviderLayoutFailure::UntrustedRaw => {
            ContractLayoutError::InvalidProviderRaw { failure }
        }
    }
}

/// The counter a space's slots are numbered by. Storage and transient slots
/// share one, so no transient slot number is also a storage one: a storage
/// collection keeps its contents at its own slot number in its contents'
/// space (a `TSlot`'s value in transient storage, a map's entries in
/// storage) wherever the collection itself lies.
fn slot_counter(space: ProviderAddressSpace) -> ProviderAddressSpace {
    match space {
        ProviderAddressSpace::Transient => ProviderAddressSpace::Storage,
        space => space,
    }
}

/// Places each field: a field with `#[slot(e)]` at `e`, any other after the
/// fields its space's counter numbered before it, skipping the slots explicit
/// fields take in the spaces it takes slots in.
fn allocate_contract<'db>(
    field_results: &[ContractFieldLayoutResult<'db>],
) -> Result<AllocatedContractStorageLayout<'db>, (ContractFieldId<'db>, ContractLayoutError<'db>)> {
    let plans = field_results.iter().map(|field| {
        field
            .result
            .as_ref()
            .expect("allocation requires every field to validate")
    });
    // The slots explicit fields take, by space.
    let mut explicit: FxHashMap<ProviderAddressSpace, Vec<(U256, U256, IdentId<'db>)>> =
        FxHashMap::default();
    for plan in plans.clone() {
        let Some(slot) = plan.explicit_slot else {
            continue;
        };
        let end = slot
            .checked_add(U256::from(plan.slot_count))
            .ok_or((plan.field, ContractLayoutError::LayoutExtentOverflow))?;
        for space in &plan.spaces {
            let ranges = explicit.entry(*space).or_default();
            if let Some(&(_, _, other)) = ranges
                .iter()
                .find(|(start, stop, _)| slot < *stop && *start < end)
            {
                return Err((
                    plan.field,
                    ContractLayoutError::ExplicitSlotOverlap { other },
                ));
            }
            if slot < end {
                ranges.push((slot, end, plan.name));
            }
        }
    }
    let mut counters: FxHashMap<ProviderAddressSpace, U256> = FxHashMap::default();
    let mut code_slot_count = 0;
    let mut fields = IndexMap::new();
    for plan in plans {
        let count = U256::from(plan.slot_count);
        let slot_offset = match plan.explicit_slot {
            Some(slot) => slot,
            None => {
                let counter = counters
                    .entry(slot_counter(plan.address_space))
                    .or_insert(U256::ZERO);
                let mut start = *counter;
                let ranges = plan
                    .spaces
                    .iter()
                    .filter_map(|space| explicit.get(space))
                    .flatten();
                while let Some(&(_, stop, _)) = ranges.clone().find(|(first, stop, _)| {
                    !count.is_zero() && start < *stop && *first < start.saturating_add(count)
                }) {
                    start = stop;
                }
                let end = start
                    .checked_add(count)
                    .ok_or((plan.field, ContractLayoutError::LayoutExtentOverflow))?;
                if !count.is_zero() {
                    *counter = end;
                }
                if plan.address_space == ProviderAddressSpace::Code {
                    code_slot_count = usize::try_from(end)
                        .ok()
                        .filter(|end| end.checked_mul(32).is_some())
                        .ok_or((plan.field, ContractLayoutError::LayoutExtentOverflow))?;
                }
                start
            }
        };
        fields.insert(
            plan.name,
            FieldStorageLayout {
                field: plan.field,
                name: plan.name,
                is_mut: plan.is_mut,
                is_provider: plan.is_provider,
                address_space: plan.address_space,
                declared: plan.declared,
                target: plan.target,
                slot_offset,
                slot_count: plan.slot_count,
                inline_leaves: plan.inline_leaves.clone(),
            },
        );
    }
    Ok(AllocatedContractStorageLayout {
        fields,
        code_slot_count,
    })
}

fn layout_integer<'db>(db: &'db dyn HirAnalysisDb, value: U256) -> IntegerId<'db> {
    IntegerId::new(db, BigUint::from_bytes_be(&value.to_be_bytes::<32>()))
}

fn allocated_contract_layout_report<'db>(
    db: &'db dyn HirAnalysisDb,
    fields: &IndexMap<IdentId<'db>, FieldStorageLayout<'db>>,
) -> ContractLayoutReport<'db> {
    let mut entries = Vec::new();
    for field in fields.values() {
        for leaf in &field.inline_leaves {
            // Slots wrap modulo `2**256`, like Solidity's.
            let base = field.slot_offset.wrapping_add(U256::from(leaf.offset));
            let value = if leaf.dimensions.is_empty() {
                ContractLayoutValue::Scalar(layout_integer(db, base))
            } else {
                ContractLayoutValue::Indexed {
                    base: layout_integer(db, base),
                    dimensions: leaf.dimensions.clone(),
                    strides: leaf.strides.clone(),
                    extent: affine_extent(&leaf.dimensions, &leaf.strides)
                        .expect("an allocated field's leaves have a finite extent"),
                }
            };
            entries.push(ContractLayoutEntry {
                field: field.field,
                path: ContractLayoutPath {
                    field: field.name,
                    segments: leaf.path.clone(),
                },
                ty: leaf.ty,
                address_space: field.address_space,
                value,
                lane: leaf.lane,
                kind: match leaf.kind {
                    InlineLayoutLeafKind::Field => ContractLayoutEntryKind::InlineField,
                    InlineLayoutLeafKind::Collection => ContractLayoutEntryKind::Collection,
                    InlineLayoutLeafKind::EnumTag => ContractLayoutEntryKind::EnumTag,
                },
            });
        }
    }
    entries.sort_by(|first, second| {
        let space_order = |space| match space {
            ProviderAddressSpace::Storage => 0,
            ProviderAddressSpace::Transient => 1,
            ProviderAddressSpace::Code => 2,
            ProviderAddressSpace::Memory => 3,
            ProviderAddressSpace::Calldata => 4,
        };
        space_order(first.address_space)
            .cmp(&space_order(second.address_space))
            .then_with(|| {
                first
                    .value
                    .base()
                    .data(db)
                    .cmp(second.value.base().data(db))
            })
    });
    ContractLayoutReport { entries }
}

#[salsa::tracked(return_ref)]
pub(crate) fn build_contract_layout_report<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Option<ContractLayoutReport<'db>> {
    let fields = &contract.storage_layout(db).allocated.as_ref()?.fields;
    Some(allocated_contract_layout_report(db, fields))
}

#[salsa::tracked(return_ref)]
pub(crate) fn build_contract_storage_layout<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> ContractStorageLayoutResult<'db> {
    let field_count = contract
        .hir_fields(db)
        .data(db)
        .iter()
        .filter(|field| field.name.is_present())
        .count();
    let mut field_results = Vec::with_capacity(field_count);
    let mut first_by_name = FxHashMap::default();
    for field_index in 0..field_count {
        if let Some((name, result)) = collect_field_plan(db, contract, field_index as u32) {
            let field = ContractFieldId {
                contract,
                index: field_index as u32,
            };
            let result_idx = field_results.len();
            field_results.push(ContractFieldLayoutResult {
                field,
                name,
                result,
            });
            if let Some(first_idx) = first_by_name.insert(name, result_idx) {
                field_results[first_idx].result = Err(vec![ContractLayoutError::InvalidFieldType]);
                field_results[result_idx].result = Err(vec![ContractLayoutError::InvalidFieldType]);
            }
        }
    }
    let allocated = if field_results.iter().all(|field| field.result.is_ok()) {
        match allocate_contract(&field_results) {
            Ok(layout) => Some(layout),
            Err((field, error)) => {
                if let Some(result) = field_results
                    .iter_mut()
                    .find(|result| result.field == field)
                {
                    result.result = Err(vec![error]);
                }
                None
            }
        }
    } else {
        None
    };
    ContractStorageLayoutResult {
        field_results,
        allocated,
    }
}
