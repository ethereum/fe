//! Result-space contracts.
//!
//! Each access component of a projection's return lies in one address space,
//! its contract. A signature may declare it after the component, or after the
//! whole return for the components that declare none: a space by name
//! (`@memory`, `@storage`, `@transient`, `@calldata`, `@code`), `@p` or
//! `@self` for the space of a data parameter's place, `@target(p)` for the
//! space of the resource a handle parameter names, `@d` for an effect
//! domain's, or an associated space, `@S` or `@B::S`. A trait declares an
//! associated space, `space S`, and each implementation gives it: a space by
//! name, `self` for where the implementing value lies, or another associated
//! space (`space S = B::S`). The access check
//! holds each instance's yields to the contracts its signature declares, and
//! an implementation's to its trait method's.
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            binder::Binder,
            fold::{TyFoldable, TyFolder},
            provider::ProviderAddressSpace,
            trait_def::{ImplementorOrigin, TraitInstId, resolve_trait_impl_instance},
            trait_resolution::{
                PredicateListId, Selection, TraitSolveCx, constraint::resolve_assoc_item_path,
            },
            ty_def::TyId,
            visitor::{TyVisitable, TyVisitor},
        },
    },
    core::semantic::constraints_for,
    hir_def::{Func, IdentId, ImplTrait, PathId, SpaceAnnotation, Trait, scope_graph::ScopeId},
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
    /// The space of a data parameter's place: `@p`, `@self`.
    Param(u32),
    /// The space of the resource a handle parameter names: `@target(p)`.
    Target(u32),
    /// The space of an effect's domain: `@d`.
    Domain(u32),
    /// An associated space: `@S`, `@B::S`.
    Assoc(SpaceKey<'db>),
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
    func.ret_spaces(db)
        .iter()
        .map(|annotation| match (*annotation)? {
            SpaceAnnotation::Target(path) => {
                param(path.to_opt()?.as_ident(db)?).map(SpaceContract::Target)
            }
            SpaceAnnotation::Path(path) => {
                let path = path.to_opt()?;
                if let Some(name) = path.as_ident(db) {
                    let effect = func.effects(db).data(db).iter().position(|effect| {
                        effect
                            .name
                            .or_else(|| effect.key_ty.to_opt()?.as_path(db)?.as_ident(db))
                            == Some(name)
                    });
                    if let Some(contract) = param(name)
                        .map(SpaceContract::Param)
                        .or_else(|| effect.map(|idx| SpaceContract::Domain(idx as u32)))
                        .or_else(|| named_space(db, name).map(SpaceContract::Space))
                    {
                        return Some(contract);
                    }
                }
                space_key(db, path, func.scope(), constraints_for(db, func.into()))
                    .map(SpaceContract::Assoc)
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
    /// Where the value of this type lies: the implementation said `self`.
    Owner(TyId<'db>),
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
    let value = impl_trait
        .spaces(db)
        .iter()
        .find(|space| space.name.to_opt() == Some(name))?
        .value?
        .to_opt()?;
    match impl_space_value(db, impl_trait, value)? {
        SpaceValue::Space(space) => Some(ResolvedSpace::Space(space)),
        SpaceValue::Owner => Some(ResolvedSpace::Owner(key.inst.self_ty(db))),
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
    /// `self`: where the implementing value itself lies, as for an owner
    /// whose elements are its own parts.
    Owner,
    /// Another associated space: `B::S`.
    Assoc(SpaceKey<'db>),
}

pub fn impl_space_value<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
    value: PathId<'db>,
) -> Option<SpaceValue<'db>> {
    if value.as_ident(db).is_some_and(|name| name.is_self(db)) {
        return Some(SpaceValue::Owner);
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
