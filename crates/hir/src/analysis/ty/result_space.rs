//! Result-space contracts.
//!
//! Each access component of a projection's return lies in one address space,
//! its contract. A signature may declare it after the component, or after the
//! whole return for the components that declare none: a space by name
//! (`@memory`, `@storage`, `@transient`, `@calldata`, `@code`), `@p` or
//! `@self` for the space of a data parameter's place, `@target(p)` for the
//! space of the resource a handle parameter names, `@d` for an effect
//! domain's, or an associated space, `@S` or `@B::S`. A parameter's or
//! domain's contract may name a sub-place, `@self.value` or `@self[_]`, by
//! fields, tuple components and `[_]` elements: its space is the one the
//! path reaches, through pinned types and collection entries, and memory for
//! a lane, whose grants are memory copies. A trait declares an associated
//! space, `space S`, and each implementation gives it: a space by name,
//! `self` or a sub-place of it (`self[_]`) for where the implementing value
//! or its elements lie, or another associated space (`space S = B::S`). The
//! access check
//! holds each instance's yields to the contracts its signature declares, and
//! an implementation's to its trait method's.
use num_traits::ToPrimitive;
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            binder::Binder,
            fold::{TyFoldable, TyFolder},
            normalize::normalize_ty,
            provider::ProviderAddressSpace,
            state_index_tys,
            trait_def::{ImplementorOrigin, TraitInstId, resolve_trait_impl_instance},
            trait_resolution::{
                PredicateListId, Selection, TraitSolveCx, constraint::resolve_assoc_item_path,
            },
            ty_check::{EffectParamSite, RecordLike},
            ty_def::TyId,
            visitor::{TyVisitable, TyVisitor},
        },
    },
    core::semantic::{EffectRequirementKey, constraints_for, effect_requirements_for_site},
    hir_def::{
        FieldIndex, Func, IdentId, ImplTrait, PathId, SpaceAnnotation, SpacePathId, SpaceStep,
        Trait, scope_graph::ScopeId,
    },
};

/// Associated space `space` of a trait instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct SpaceKey<'db> {
    pub inst: TraitInstId<'db>,
    pub space: u16,
}

impl<'db> SpaceKey<'db> {
    pub fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        self.inst.def(db).spaces(db)[self.space as usize]
            .name
            .to_opt()
    }
}

impl<'db> TyVisitable<'db> for SpaceKey<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.inst.visit_with(visitor)
    }
}

impl<'db> TyFoldable<'db> for SpaceKey<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        SpaceKey {
            inst: self.inst.fold_with(db, folder),
            space: self.space,
        }
    }
}

/// A declared result-space contract.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum SpaceContract<'db> {
    Space(ProviderAddressSpace),
    /// The space of a data parameter's place, or of a sub-place of it:
    /// `@p`, `@self`, `@self.value`.
    Param(u32, SpacePathId<'db>),
    /// The space of the resource a handle parameter names: `@target(p)`.
    Target(u32),
    /// The space of an effect's domain, or of a sub-place of its target:
    /// `@d`, `@d.f`.
    Domain(u32, SpacePathId<'db>),
    /// An associated space: `@S`, `@B::S`.
    Assoc(SpaceKey<'db>),
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
                    .or_else(|| space_key(db, path, scope, assumptions).map(SpaceContract::Assoc))
            }
        })
        .collect()
}

/// The space `name` names.
fn named_space(db: &dyn HirAnalysisDb, name: IdentId<'_>) -> Option<ProviderAddressSpace> {
    Some(match name.data(db).as_str() {
        "memory" => ProviderAddressSpace::Memory,
        "storage" => ProviderAddressSpace::Storage,
        "transient" => ProviderAddressSpace::Transient,
        "calldata" => ProviderAddressSpace::Calldata,
        "code" => ProviderAddressSpace::Code,
        _ => return None,
    })
}

fn space_index<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_def: Trait<'db>,
    name: IdentId<'db>,
) -> Option<usize> {
    trait_def
        .spaces(db)
        .iter()
        .position(|space| space.name.to_opt() == Some(name))
}

/// The associated space a path names: `S` or `Self::S` in a trait or an
/// implementation of one, and `B::S` for a type `B` bounded by a trait with
/// space `S`.
fn space_key<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<SpaceKey<'db>> {
    let (inst, space) =
        resolve_assoc_item_path(db, path, scope, assumptions, |trait_def, name| {
            space_index(db, trait_def, name)
        })?;
    Some(SpaceKey {
        inst,
        space: space as u16,
    })
}

/// What an associated space is, once the implementations involved are known.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolvedSpace<'db> {
    Space(ProviderAddressSpace),
    /// Where the value of this type, or a sub-place of it, lies: the
    /// implementation said `self` or `self[_]`.
    Owner(TyId<'db>, SpacePathId<'db>),
}

/// What `key` is where the scope selects the implementations involved.
pub fn resolve_space_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SpaceKey<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<ResolvedSpace<'db>> {
    let Selection::Unique(resolved) = resolve_trait_impl_instance(
        db,
        TraitSolveCx::new(db, scope).with_assumptions(assumptions),
        key.inst,
    ) else {
        return None;
    };
    let ImplementorOrigin::Hir(impl_trait) = resolved.selected().origin(db) else {
        return None;
    };
    let name = key.name(db)?;
    let (value, steps) = impl_trait
        .spaces(db)
        .iter()
        .find(|space| space.name.to_opt() == Some(name))?
        .value?;
    match impl_space_value(db, impl_trait, value.to_opt()?, steps)? {
        SpaceValue::Space(space) => Some(ResolvedSpace::Space(space)),
        SpaceValue::Owner(steps) => Some(ResolvedSpace::Owner(key.inst.self_ty(db), steps)),
        SpaceValue::Assoc(inner) => {
            let inner =
                Binder::bind(impl_trait.into(), inner).instantiate(db, resolved.impl_args(db));
            resolve_space_key(db, inner, scope, assumptions)
        }
    }
}

/// The space an implementation gives in `space S = value`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpaceValue<'db> {
    Space(ProviderAddressSpace),
    /// `self` or a sub-place of it: where the implementing value lies, or
    /// its elements (`self[_]`), as for an owner whose elements are its own
    /// parts.
    Owner(SpacePathId<'db>),
    /// Another associated space: `B::S`.
    Assoc(SpaceKey<'db>),
}

/// What `space S = value steps` gives; `None` when it names no space, or a
/// sub-place of something other than `self` or of no part of the
/// implementing type.
pub fn impl_space_value<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
    value: PathId<'db>,
    steps: SpacePathId<'db>,
) -> Option<SpaceValue<'db>> {
    if value.as_ident(db).is_some_and(|name| name.is_self(db)) {
        let owner = impl_trait.trait_inst_result(db).ok()?.self_ty(db);
        let scope = impl_trait.scope();
        let assumptions = constraints_for(db, impl_trait.into());
        return (steps.steps(db).is_empty()
            || space_path_steps(db, scope, assumptions, owner, steps).is_some())
        .then_some(SpaceValue::Owner(steps));
    }
    if !steps.steps(db).is_empty() {
        return None;
    }
    value
        .as_ident(db)
        .and_then(|name| named_space(db, name))
        .map(SpaceValue::Space)
        .or_else(|| {
            space_key(
                db,
                value,
                impl_trait.scope(),
                constraints_for(db, impl_trait.into()),
            )
            .map(SpaceValue::Assoc)
        })
}
