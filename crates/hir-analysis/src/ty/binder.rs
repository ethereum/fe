use std::collections::hash_map::Entry;

use rustc_hash::FxHashMap;

use super::{
    const_ty::{self, ConstTyData},
    fold::{TypeFoldable, TypeFolder},
    ty_def::{collect_type_params, TyData, TyId},
    ty_lower::collect_generic_params,
};
use crate::HirAnalysisDb;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binder<T> {
    value: T,
}

impl<T> Binder<T> {
    pub const fn bind(value: T) -> Self {
        Binder { value }
    }
}

impl<'db, T> Binder<T>
where
    T: TypeFoldable<'db>,
{
    pub fn instantiate_identity(self) -> T {
        self.value
    }

    pub fn skip_binder(self) -> T {
        self.value
    }

    pub fn instantiate(self, db: &'db dyn HirAnalysisDb, args: &[TyId]) -> T {
        let mut folder = InstantiateFolder { db, args };
        self.value.fold_with(&mut folder)
    }

    pub fn instantiate_with<F>(self, db: &'db dyn HirAnalysisDb, f: F) -> T
    where
        F: FnMut(TyId) -> TyId,
    {
        let mut folder = InstantiateWithFolder {
            db,
            f,
            params: FxHashMap::default(),
        };
        self.value.fold_with(&mut folder)
    }
}

struct InstantiateFolder<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    args: &'a [TyId],
}

impl<'db, 'a> TypeFolder<'db> for InstantiateFolder<'db, 'a> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    fn fold_ty(&mut self, ty: TyId) -> TyId {
        match ty.data(self.db) {
            TyData::TyParam(param) => return self.args[param.idx],
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(param, _) = const_ty.data(self.db) {
                    return self.args[param.idx];
                }
            }

            _ => {}
        }

        ty.super_fold_with(self)
    }
}

struct InstantiateWithFolder<'db, F>
where
    F: FnMut(TyId) -> TyId,
{
    db: &'db dyn HirAnalysisDb,
    f: F,
    params: FxHashMap<usize, TyId>,
}

impl<'db, F> TypeFolder<'db> for InstantiateWithFolder<'db, F>
where
    F: FnMut(TyId) -> TyId,
{
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    fn fold_ty(&mut self, ty: TyId) -> TyId {
        match ty.data(self.db) {
            TyData::TyParam(param) => {
                match self.params.entry(param.idx) {
                    Entry::Occupied(entry) => return *entry.get(),
                    Entry::Vacant(entry) => {
                        let ty = (self.f)(ty);
                        entry.insert(ty);
                        return ty;
                    }
                };
            }
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(param, _) = const_ty.data(self.db) {
                    match self.params.entry(param.idx) {
                        Entry::Occupied(entry) => return *entry.get(),
                        Entry::Vacant(entry) => {
                            let ty = (self.f)(ty);
                            entry.insert(ty);
                            return ty;
                        }
                    };
                }
            }

            _ => {}
        }

        ty.super_fold_with(self)
    }
}
