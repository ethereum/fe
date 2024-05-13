use rustc_hash::FxHashMap;

use super::{
    const_ty::{ConstTyData, ConstTyId},
    fold::{TyFoldable, TyFolder},
    ty_def::{TyData, TyId, TyVar},
    unify::{InferenceKey, UnificationTable},
};
use crate::{ty::visitor::TyVisitor, HirAnalysisDb};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Canonical<T> {
    pub value: T,
}

impl<T> Canonical<T>
where
    T: for<'db> TyFoldable<'db>,
{
    pub fn new(db: &dyn HirAnalysisDb, value: T) -> Self {
        let mut c = Canonicalizer::new(db);
        let value = value.fold_with(&mut c);
        Canonical { value }
    }

    /// Extract canonical query to the current environment.
    /// # Panics
    /// Panics if the table is not empty.
    pub(super) fn extract_identity(self, table: &mut UnificationTable) -> T {
        assert!(table.is_empty());
        struct TyVarCollector<'db> {
            db: &'db dyn HirAnalysisDb,
            vars: Vec<TyVar>,
        }

        impl<'db> TyVisitor<'db> for TyVarCollector<'db> {
            fn db(&self) -> &'db dyn HirAnalysisDb {
                self.db
            }

            fn visit_var(&mut self, var: &TyVar) {
                self.vars.push(var.clone());
            }
        }
        let mut collector = TyVarCollector {
            db: table.db(),
            vars: vec![],
        };
        self.value.visit_with(&mut collector);
        collector.vars.sort_unstable_by_key(|var| var.key);

        for var in collector.vars {
            table.new_var(var.sort, &var.kind);
        }

        self.value
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Canonicalized<T> {
    pub value: Canonical<T>,
    // A substitution from canonical type variables to original type variables.
    subst: FxHashMap<TyId, TyId>,
}

impl<T> Canonicalized<T>
where
    T: for<'db> TyFoldable<'db>,
{
    pub fn new(db: &dyn HirAnalysisDb, value: T) -> Self {
        let mut canonicalizer = Canonicalizer::new(db);
        let value = value.fold_with(&mut canonicalizer);
        let map = canonicalizer
            .subst
            .into_iter()
            .map(|(orig_var, canonical_var)| (canonical_var, orig_var))
            .collect();
        Canonicalized {
            value: Canonical { value },
            subst: map,
        }
    }

    /// Extract solution to the current environment.
    pub fn extract_solution<U>(&self, table: &mut UnificationTable, solution: Solution<U>) -> U
    where
        U: for<'db> TyFoldable<'db>,
    {
        let map = self.subst.clone();
        let mut extractor = SolutionExtractor::new(table, map);
        solution.value.fold_with(&mut extractor)
    }
}

/// A solution for the [`Canonical`] query.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Solution<T> {
    pub(super) value: T,
}

impl<T> Solution<T> {
    pub(super) fn new(value: T) -> Self {
        Self { value }
    }
}

struct Canonicalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    // A substitution from original type variables to canonical variables.
    subst: FxHashMap<TyId, TyId>,
}

impl<'db> Canonicalizer<'db> {
    fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Canonicalizer {
            db,
            subst: FxHashMap::default(),
        }
    }

    fn canonical_var(&mut self, var: &TyVar) -> TyVar {
        let key = self.subst.len() as u32;
        TyVar {
            sort: var.sort,
            kind: var.kind.clone(),
            key: InferenceKey(key),
        }
    }
}

impl<'db> TyFolder<'db> for Canonicalizer<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    fn fold_ty(&mut self, ty: TyId) -> TyId {
        if let Some(&canonical) = self.subst.get(&ty) {
            return canonical;
        }

        match ty.data(self.db) {
            TyData::TyVar(var) => {
                let canonical_var = self.canonical_var(var);
                let canonical_ty = TyId::new(self.db, TyData::TyVar(canonical_var));

                self.subst.insert(ty, canonical_ty);
                canonical_ty
            }

            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyVar(var, const_ty_ty) = const_ty.data(self.db) {
                    let canonical_var = self.canonical_var(var);
                    let const_ty =
                        ConstTyId::new(self.db, ConstTyData::TyVar(canonical_var, *const_ty_ty));
                    let canonical_ty = TyId::const_ty(self.db, const_ty);

                    self.subst.insert(ty, canonical_ty);
                    canonical_ty
                } else {
                    ty.super_fold_with(self)
                }
            }

            _ => ty.super_fold_with(self),
        }
    }
}

struct SolutionExtractor<'a, 'db> {
    table: &'a mut UnificationTable<'db>,
    /// A subst from canonical type variables to the variables in the current
    /// env.
    subst: FxHashMap<TyId, TyId>,
}

impl<'a, 'db> SolutionExtractor<'a, 'db> {
    fn new(table: &'a mut UnificationTable<'db>, subst: FxHashMap<TyId, TyId>) -> Self {
        SolutionExtractor { table, subst }
    }
}

impl<'a, 'db> TyFolder<'db> for SolutionExtractor<'a, 'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.table.db()
    }

    fn fold_ty(&mut self, ty: TyId) -> TyId {
        if let Some(&ty) = self.subst.get(&ty) {
            return ty;
        }

        match ty.data(self.db()) {
            TyData::TyVar(var) => {
                let new_ty = self.table.new_var(var.sort, &var.kind);
                self.subst.insert(ty, new_ty);
                new_ty
            }

            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyVar(var, const_ty_ty) = const_ty.data(self.db()) {
                    let new_key = self.table.new_key(&var.kind, var.sort);
                    let new_ty = TyId::const_ty_var(self.db(), *const_ty_ty, new_key);
                    self.subst.insert(ty, new_ty);
                    new_ty
                } else {
                    ty.super_fold_with(self)
                }
            }

            _ => ty.super_fold_with(self),
        }
    }
}
