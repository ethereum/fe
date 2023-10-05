/// This module contains the logic for solving trait bounds.
use hir::hir_def::{ImplTrait, IngotId, Trait};
use rustc_hash::FxHashMap;

use crate::{ty::trait_lower::collect_trait_impls, HirAnalysisDb};

use super::{
    constraint::{collect_super_traits, ConstraintListId},
    ty_def::{Kind, Subst, TyId},
    unify::UnificationTable,
};

#[salsa::tracked(return_ref)]
pub(crate) fn ingot_trait_env(db: &dyn HirAnalysisDb, ingot: IngotId) -> TraitEnv {
    TraitEnv::collect(db, ingot)
}

#[salsa::tracked(return_ref)]
pub(crate) fn trait_implementors(db: &dyn HirAnalysisDb, trait_: TraitInstId) -> Vec<Implementor> {
    let env = ingot_trait_env(db, trait_.ingot(db));
    let Some(impls) = env.impls.get(&trait_.def(db)) else {
        return vec![];
    };

    impls
        .iter()
        .filter(|impl_| {
            let mut table = UnificationTable::new(db);
            let gen_impl = impl_.generalize(db, &mut table);
            table.unify(gen_impl.trait_(db), trait_)
        })
        .cloned()
        .collect()
}

/// Represents the trait environment of an ingot, which maintain all trait
/// implementors which can be used in the ingot.
#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct TraitEnv {
    impls: FxHashMap<TraitDef, Vec<Implementor>>,
    ingot: IngotId,
}

impl TraitEnv {
    pub(super) fn new(db: &dyn HirAnalysisDb, ingot: IngotId) -> &Self {
        ingot_trait_env(db, ingot)
    }

    fn collect(db: &dyn HirAnalysisDb, ingot: IngotId) -> Self {
        let mut impls: FxHashMap<_, Vec<Implementor>> = FxHashMap::default();

        for impl_map in ingot
            .external_ingots(db.as_hir_db())
            .iter()
            .map(|(_, external)| collect_trait_impls(db, *external))
            .chain(std::iter::once(collect_trait_impls(db, ingot)))
        {
            // `collect_trait_impls` ensure that there are no conflicting impls, so we can
            // just extend the map.
            for (trait_def, implementors) in impl_map.iter() {
                impls
                    .entry(*trait_def)
                    .or_default()
                    .extend(implementors.iter().copied());
            }
        }

        Self { impls, ingot }
    }

    /// Returns all implementors of the given instantiated trait.
    pub(crate) fn implementors_for<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        trait_: TraitInstId,
    ) -> &'db [Implementor] {
        trait_implementors(db, trait_)
    }
}

/// Represents an implementor of a trait.
#[salsa::interned]
pub(crate) struct Implementor {
    pub(crate) trait_: TraitInstId,
    pub(crate) ty: TyId,
    #[return_ref]
    pub(crate) params: Vec<TyId>,

    pub(crate) hir_impl: ImplTrait,
}

impl Implementor {
    /// Returns the trait definition that this implementor implements.
    pub(crate) fn trait_def(self, db: &dyn HirAnalysisDb) -> TraitDef {
        self.trait_(db).def(db)
    }

    /// Generalizes the implementor by replacing all type parameters with fresh
    /// type variables.
    pub(super) fn generalize(self, db: &dyn HirAnalysisDb, table: &mut UnificationTable) -> Self {
        let mut subst = FxHashMap::default();
        for param in self.params(db) {
            let var = table.new_var(param.kind(db));
            subst.insert(*param, var);
        }

        let hir_impl = self.hir_impl(db);
        let trait_ = self.trait_(db).apply_subst(db, &mut subst);
        let ty = self.ty(db).apply_subst(db, &mut subst);
        let params = self
            .params(db)
            .iter()
            .map(|param| subst[param])
            .collect::<Vec<_>>();

        Implementor::new(db, trait_, ty, params, hir_impl)
    }

    pub(super) fn apply_subst<S: Subst>(
        self,
        db: &dyn HirAnalysisDb,
        subst: &mut S,
    ) -> Implementor {
        Implementor::new(
            db,
            self.trait_(db).apply_subst(db, subst),
            self.ty(db).apply_subst(db, subst),
            self.params(db)
                .iter()
                .map(|param| param.apply_subst(db, subst))
                .collect(),
            self.hir_impl(db),
        )
    }

    pub(super) fn constraints(self, db: &dyn HirAnalysisDb) -> ConstraintListId {
        todo!()
    }
}

/// Represents an instantiated trait, which can be thought of as a trait
/// reference from a HIR perspective.
#[salsa::interned]
pub struct TraitInstId {
    pub def: TraitDef,
    #[return_ref]
    pub substs: Vec<TyId>,

    pub(super) ingot: IngotId,
}

impl TraitInstId {
    pub fn pretty_print(self, db: &dyn HirAnalysisDb) -> String {
        let mut s = self.def(db).name(db).unwrap_or("<unknown>").to_string();

        let mut args = self.substs(db).iter().map(|ty| ty.pretty_print(db));
        if let Some(first) = args.next() {
            s.push('<');
            s.push_str(&first);
            for arg in args {
                s.push_str(", ");
                s.push_str(&arg);
            }
            s.push('>');
        }

        s
    }

    /// Apply substitutions to this trait.
    /// Which is to say, replace all type parameters with their corresponding
    /// type in the `subst`.
    pub(super) fn apply_subst<S: Subst>(
        self,
        db: &dyn HirAnalysisDb,
        subst: &mut S,
    ) -> TraitInstId {
        TraitInstId::new(
            db,
            self.def(db),
            self.substs(db)
                .iter()
                .map(|ty| ty.apply_subst(db, subst))
                .collect(),
            self.ingot(db),
        )
    }

    pub(super) fn subst_table(self, db: &dyn HirAnalysisDb) -> impl Subst {
        let mut table = FxHashMap::default();
        for (param, subst) in self.def(db).params(db).iter().zip(self.substs(db)) {
            table.insert(*param, *subst);
        }

        table
    }
}

/// Represents a trait definition.
#[salsa::tracked]
pub struct TraitDef {
    pub trait_: Trait,
    #[return_ref]
    pub params: Vec<TyId>,
    /// We collects self type here to know the expected kind of implementor
    /// type in `Implementor` lowering phase.
    pub self_param: TyId,
    // TODO: we need to collect associated method types here.
    // methods: Vec<FuncInst>
}

impl TraitDef {
    /// Returns the type kind that implementor type must have.
    pub(crate) fn expected_implementor_kind(self, db: &dyn HirAnalysisDb) -> &Kind {
        self.self_param(db).kind(db)
    }

    /// Returns `ingot` in which this trait is defined.
    pub(crate) fn ingot(self, db: &dyn HirAnalysisDb) -> IngotId {
        let hir_db = db.as_hir_db();
        self.trait_(db).top_mod(hir_db).ingot(hir_db)
    }

    pub(super) fn super_traits(self, db: &dyn HirAnalysisDb) -> &[TraitInstId] {
        &collect_super_traits(db, self)
    }

    fn name(self, db: &dyn HirAnalysisDb) -> Option<&str> {
        self.trait_(db)
            .name(db.as_hir_db())
            .to_opt()
            .map(|name| name.data(db.as_hir_db()).as_str())
    }
}
