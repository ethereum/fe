use rustc_hash::FxHashMap;

use super::{
    const_ty::{ConstTyData, ConstTyId},
    fold::{TyFoldable, TyFolder},
    ty_def::{TyData, TyId, TyVar},
    unify::{InferenceKey, UnificationTable},
};
use crate::HirAnalysisDb;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Canonical<T> {
    pub value: T,
}

impl<T> Canonical<T>
where
    T: for<'db> TyFoldable<'db>,
{
    pub fn canonicalize(db: &dyn HirAnalysisDb, value: T) -> Self {
        let mut c = Canonicalizer::new(db);
        let value = value.fold_with(&mut c);
        Canonical { value }
    }

    pub(super) fn decanonicalize(self, table: &mut UnificationTable) -> T {
        let mut d = DeCanonicalizer::new(table);
        self.value.fold_with(&mut d)
    }
}

struct Canonicalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    var_map: FxHashMap<TyId, TyId>,
}

impl<'db> Canonicalizer<'db> {
    fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Canonicalizer {
            db,
            var_map: FxHashMap::default(),
        }
    }

    fn canonical_var(&mut self, var: &TyVar) -> TyVar {
        let key = self.var_map.len() as u32;
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
        if let Some(&canonical) = self.var_map.get(&ty) {
            return canonical;
        }

        match ty.data(self.db) {
            TyData::TyVar(var) => {
                let canonical_var = self.canonical_var(var);
                let canonical_ty = TyId::new(self.db, TyData::TyVar(canonical_var));

                self.var_map.insert(ty, canonical_ty);
                canonical_ty
            }

            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyVar(var, const_ty_ty) = const_ty.data(self.db) {
                    let canonical_var = self.canonical_var(var);
                    let const_ty =
                        ConstTyId::new(self.db, ConstTyData::TyVar(canonical_var, *const_ty_ty));
                    let canonical_ty = TyId::const_ty(self.db, const_ty);

                    self.var_map.insert(ty, canonical_ty);
                    canonical_ty
                } else {
                    ty.super_fold_with(self)
                }
            }

            _ => ty.super_fold_with(self),
        }
    }
}

struct DeCanonicalizer<'a, 'db> {
    table: &'a mut UnificationTable<'db>,
    var_map: FxHashMap<TyId, TyId>,
}

impl<'a, 'db> DeCanonicalizer<'a, 'db> {
    fn new(table: &'a mut UnificationTable<'db>) -> Self {
        DeCanonicalizer {
            table,
            var_map: FxHashMap::default(),
        }
    }
}

impl<'a, 'db> TyFolder<'db> for DeCanonicalizer<'a, 'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.table.db()
    }

    fn fold_ty(&mut self, ty: TyId) -> TyId {
        if let Some(&ty) = self.var_map.get(&ty) {
            return ty;
        }

        match ty.data(self.db()) {
            TyData::TyVar(var) => {
                let new_ty = self.table.new_var(var.sort, &var.kind);
                self.var_map.insert(ty, new_ty);
                new_ty
            }

            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyVar(var, const_ty_ty) = const_ty.data(self.db()) {
                    let new_key = self.table.new_key(&var.kind, var.sort);
                    let new_ty = TyId::const_ty_var(self.db(), *const_ty_ty, new_key);
                    self.var_map.insert(ty, new_ty);
                    new_ty
                } else {
                    ty.super_fold_with(self)
                }
            }

            _ => ty.super_fold_with(self),
        }
    }
}
