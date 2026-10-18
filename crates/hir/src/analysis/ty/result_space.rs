//! Result-space contracts.
//!
//! Each access component of a projection's return lies in one address space,
//! which the access check infers per instance from its yields. A signature
//! may declare it after the component, or after the whole return for the
//! components that declare none: a space by name (`@memory`, `@storage`,
//! `@transient`, `@calldata`, `@code`), `@p` or `@self` for the space of a
//! data parameter's place, `@target(p)` for the space of the resource a
//! handle parameter names, or `@d` for an effect domain's. A parameter's or
//! domain's contract may name a sub-place, `@self.value` or `@self[_]`, by
//! fields, tuple components and `[_]` elements: its space is the one the
//! path reaches, through pinned types and collection entries, and memory for
//! a lane, whose grants are memory copies. The access check holds each
//! instance's yields to the contracts its signature declares, and an
//! implementation's to its trait method's.
use num_traits::ToPrimitive;
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            normalize::normalize_ty,
            provider::AddressSpace,
            state_index_tys,
            trait_resolution::PredicateListId,
            ty_check::{EffectParamSite, RecordLike},
            ty_def::TyId,
        },
    },
    core::semantic::{EffectRequirementKey, constraints_for, effect_requirements_for_site},
    hir_def::{
        FieldIndex, Func, IdentId, SpaceAnnotation, SpacePathId, SpaceStep, scope_graph::ScopeId,
    },
};

/// A declared result-space contract.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum SpaceContract<'db> {
    Space(AddressSpace),
    /// The space of a data parameter's place, or of a sub-place of it:
    /// `@p`, `@self`, `@self.value`.
    Param(u32, SpacePathId<'db>),
    /// The space of the resource a handle parameter names: `@target(p)`.
    Target(u32),
    /// The space of an effect's domain, or of a sub-place of its target:
    /// `@d`, `@d.f`.
    Domain(u32, SpacePathId<'db>),
}

/// What one step of a sub-place's path is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceStep {
    /// A field or tuple component, by index.
    Field(usize),
    /// An array element.
    Index,
    /// An entry of a `StateIndex` collection.
    Entry,
    /// A field of an enum variant's payload.
    Variant,
}

/// The steps `path` takes from a place of type `root`, each with the type of
/// the place it starts from and of the one it reaches. `None` when a step
/// names no part of its type.
pub fn space_path_steps<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    root: TyId<'db>,
    path: SpacePathId<'db>,
) -> Option<Vec<(TyId<'db>, PlaceStep, TyId<'db>)>> {
    let normalize = |ty: TyId<'db>| {
        let ty = ty.as_capability(db).map_or(ty, |(_, inner)| inner);
        normalize_ty(db, ty, scope, assumptions)
    };
    let mut ty = normalize(root);
    path.steps(db)
        .iter()
        .map(|step| {
            let (kind, next) = match *step {
                SpaceStep::Field(FieldIndex::Ident(name)) => (
                    PlaceStep::Field(RecordLike::Type(ty).record_field_idx(db, name)?),
                    RecordLike::Type(ty).record_field_ty(db, name)?,
                ),
                SpaceStep::Field(FieldIndex::Index(index)) => {
                    let index = ToPrimitive::to_usize(index.data(db))?;
                    let component = ty
                        .is_tuple(db)
                        .then(|| ty.field_types(db).get(index).copied())??;
                    (PlaceStep::Field(index), component)
                }
                SpaceStep::Element if ty.is_array(db) => {
                    (PlaceStep::Index, *ty.decompose_ty_app(db).1.first()?)
                }
                SpaceStep::Element => (
                    PlaceStep::Entry,
                    state_index_tys(db, scope, ty, assumptions)?.1,
                ),
            };
            let next = normalize(next);
            if next.has_invalid(db) {
                return None;
            }
            let step = (ty, kind, next);
            ty = next;
            Some(step)
        })
        .collect()
}

/// The contracts `func`'s return declares, per access component. `None`
/// where a component declares none, or one that names no space.
#[salsa::tracked(return_ref)]
pub fn declared_result_spaces<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<Option<SpaceContract<'db>>> {
    let param = |name: IdentId<'db>| {
        func.params(db)
            .position(|param| param.name(db) == Some(name))
            .map(|idx| idx as u32)
    };
    let scope = func.scope();
    let assumptions = constraints_for(db, func.into());
    // A sub-place names a part of its root's type.
    let valid = |root: Option<TyId<'db>>, steps: SpacePathId<'db>| {
        steps.steps(db).is_empty()
            || root
                .is_some_and(|root| space_path_steps(db, scope, assumptions, root, steps).is_some())
    };
    func.ret_spaces(db)
        .iter()
        .map(|annotation| match (*annotation)? {
            SpaceAnnotation::Target(path) => {
                param(path.to_opt()?.as_ident(db)?).map(SpaceContract::Target)
            }
            SpaceAnnotation::Path(path, steps) => {
                let path = path.to_opt()?;
                if let Some(name) = path.as_ident(db) {
                    if let Some(idx) = param(name) {
                        let root = func.params(db).nth(idx as usize).map(|param| param.ty(db));
                        return valid(root, steps).then_some(SpaceContract::Param(idx, steps));
                    }
                    if let Some(idx) = func.effects(db).data(db).iter().position(|effect| {
                        effect
                            .name
                            .or_else(|| effect.key_ty.to_opt()?.as_path(db)?.as_ident(db))
                            == Some(name)
                    }) {
                        let root = effect_requirements_for_site(db, EffectParamSite::Func(func))
                            .into_iter()
                            .find(|requirement| requirement.binding_idx as usize == idx)
                            .and_then(|requirement| match requirement.key {
                                EffectRequirementKey::Type(ty) => Some(ty),
                                _ => None,
                            });
                        return valid(root, steps)
                            .then_some(SpaceContract::Domain(idx as u32, steps));
                    }
                }
                if !steps.steps(db).is_empty() {
                    return None;
                }
                path.as_ident(db)
                    .and_then(|name| named_space(db, name))
                    .map(SpaceContract::Space)
            }
        })
        .collect()
}

/// The space `name` names.
fn named_space(db: &dyn HirAnalysisDb, name: IdentId<'_>) -> Option<AddressSpace> {
    Some(match name.data(db).as_str() {
        "memory" => AddressSpace::Memory,
        "storage" => AddressSpace::Storage,
        "transient" => AddressSpace::Transient,
        "calldata" => AddressSpace::Calldata,
        "code" => AddressSpace::Code,
        _ => return None,
    })
}
