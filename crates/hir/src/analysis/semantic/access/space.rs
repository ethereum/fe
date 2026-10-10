//! The space of a sub-place: where a path from a place in a known space
//! takes it. Spec 19's `space(P.f) = pin(F)`: a step into a pinned type
//! moves the place into the pin's space and an entry into its collection's
//! `SPACE`; a lane, a packed field or entry of a state word, has memory
//! grants, the memory copies it is accessed through.
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::SemanticInstance,
        ty::{provider::ProviderAddressSpace, result_space::PlaceStep, ty_def::TyId},
    },
    core::semantic::storage_field_lanes,
    hir_def::scope_graph::ScopeId,
};

/// Where a path takes a place, step by step.
#[derive(Clone, Copy, Debug)]
pub(crate) struct PathSpace {
    /// The space the innermost pinned type or entry along the path moved
    /// the place into.
    pub moved: Option<ProviderAddressSpace>,
    /// Whether the path crosses a lane.
    pub lane: bool,
}

impl PathSpace {
    /// At a root of type `root`.
    pub fn new<'db>(db: &'db dyn HirAnalysisDb, root: TyId<'db>) -> Self {
        Self {
            moved: root.pin(db),
            lane: false,
        }
    }

    /// One step, `(container, step, result)` from a place of type
    /// `container` to one of type `result`, on a path whose root lies in
    /// `start`.
    pub fn step<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        scope: ScopeId<'db>,
        start: Option<ProviderAddressSpace>,
        (container, step, result): (TyId<'db>, PlaceStep, TyId<'db>),
    ) {
        match step {
            PlaceStep::Entry => {
                if let Some(space) = instance.state_index_space(db, container) {
                    self.moved = Some(space);
                }
                self.lane |= instance.state_index_lanes(db, container).is_some();
            }
            PlaceStep::Field(field)
                if matches!(
                    self.moved.or(start),
                    Some(ProviderAddressSpace::Storage | ProviderAddressSpace::Transient)
                ) =>
            {
                self.lane |= storage_field_lanes(db, scope, container)
                    .get(field)
                    .copied()
                    .unwrap_or(false);
            }
            PlaceStep::Field(_) | PlaceStep::Index | PlaceStep::Variant => {}
        }
        if let Some(space) = result.pin(db) {
            self.moved = Some(space);
        }
    }

    /// The space a retained grant to the place lies in, for a root in
    /// `start`: memory for a lane, whose grants are memory copies.
    pub fn grant_space(self, start: Option<ProviderAddressSpace>) -> Option<ProviderAddressSpace> {
        if self.lane {
            Some(ProviderAddressSpace::Memory)
        } else {
            self.moved.or(start)
        }
    }
}
