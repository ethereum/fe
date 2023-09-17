use std::collections::BTreeMap;

use hir::hir_def::{ImplTrait, TopLevelMod, Trait};
use rustc_hash::FxHashMap;

use crate::HirAnalysisDb;

use super::{
    diagnostics::{TraitLowerDiag, TyLowerDiag},
    trait_::{Implementor, TraitDef, TraitImplTable},
    ty::TyId,
    ty_lower::{collect_generic_params, GenericParamOwnerId},
    unify::UnificationTable,
};

#[salsa::tracked]
pub(crate) fn lower_trait(db: &dyn HirAnalysisDb, trait_: Trait) -> TraitDef {
    TraitBuilder::new(db, trait_).build()
}

struct TraitBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    trait_: Trait,
    params: Vec<TyId>,
    self_args: TyId,
    // TODO: We need to lower associated methods here.
    // methods: Vec
}

impl<'db> TraitBuilder<'db> {
    fn new(db: &'db dyn HirAnalysisDb, trait_: Trait) -> Self {
        let params_owner_id = GenericParamOwnerId::new(db, trait_.into());
        let params_set = collect_generic_params(db, params_owner_id);
        Self {
            db,
            trait_,
            params: params_set.params.clone(),
            self_args: params_set.trait_self.unwrap(),
        }
    }

    fn build(self) -> TraitDef {
        TraitDef::new(self.db, self.trait_, self.params, self.self_args)
    }
}

/// Collect all implementors in an ingot.
struct ImplementorCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    impl_table: TraitImplTable,
    diags: BTreeMap<TopLevelMod, TraitImplDiag>,
}

impl<'db> ImplementorCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            db,
            impl_table: TraitImplTable::default(),
            diags: BTreeMap::new(),
        }
    }

    fn collect_impls(&mut self, impls: &[ImplTrait]) {
        todo!()
    }

    fn push_diag(&mut self, impl_: ImplTrait, diag: impl Into<TraitImplDiag>) {
        let top_mod = impl_.top_mod(self.db.as_hir_db());
        self.diags
            .entry(top_mod)
            .or_insert_with(|| TraitImplDiag::from(diag.into()));
    }
}

impl Implementor {
    fn generalize<'db>(self, db: &'db dyn HirAnalysisDb) -> (Self, UnificationTable<'db>) {
        let mut subst = FxHashMap::default();
        let mut table = UnificationTable::new(db);
        for param in self.params(db) {
            let var = table.new_var(param.kind(db));
            subst.insert(*param, var);
        }

        let impl_def = self.impl_def(db);
        let trait_ = self.trait_(db).apply_subst(db, &mut subst);
        let ty = self.ty(db).apply_subst(db, &mut subst);
        let params = self
            .params(db)
            .iter()
            .map(|param| subst[param])
            .collect::<Vec<_>>();

        let implementor = Implementor::new(db, impl_def, trait_, ty, params);

        (implementor, table)
    }

    fn does_conflict(self, db: &dyn HirAnalysisDb, other: &Self) -> bool {
        if self.trait_def(db) != other.trait_def(db) {
            return false;
        }

        let (self_, mut table) = self.generalize(db);
        for (&self_param, &other_param) in self_.params(db).iter().zip(other.params(db)) {
            if !table.unify(self_param, other_param) {
                return false;
            }
        }

        table.unify(self_.ty(db), other.ty(db))
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, derive_more::From)]
enum TraitImplDiag {
    Ty(TyLowerDiag),
    Trait(TraitLowerDiag),
}
