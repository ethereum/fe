use std::collections::hash_map::Entry;

use rustc_hash::FxHashMap;

use super::{
    const_ty::ConstTyData,
    fold::{TyFoldable, TyFolder},
    ty_def::{TyData, TyId},
};
use crate::HirAnalysisDb;

/// A `Binder` is a type constructor that binds a type variable within its
/// scope.
///
/// # Type Parameters
/// - `T`: The type being bound within the `Binder`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binder<T> {
    value: T,
}
unsafe impl<T> salsa::update::Update for Binder<T>
where
    T: salsa::update::Update,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        T::maybe_update(&mut old_value.value, new_value.value)
    }
}

impl<T> Binder<T> {
    pub const fn bind(value: T) -> Self {
        Binder { value }
    }
}

impl<'db, T> Binder<T>
where
    T: TyFoldable<'db>,
{
    /// Instantiates the binder with an identity function.
    ///
    /// This method essentially returns the value within the binder without any
    /// modifications.
    ///
    /// # Returns
    /// The value contained within the `Binder`.
    ///
    /// # Note
    /// This function is useful when you want to retrieve the value inside the
    /// binder without applying any transformations.
    pub fn instantiate_identity(self) -> T {
        self.value
    }

    /// Retrieves a reference to the value within the binder.
    ///
    /// This function is useful when you want to access some data that you know
    /// doesn't depend on bounded variables in the binder.
    pub fn skip_binder(&self) -> &T {
        &self.value
    }

    /// Instantiates the binder with the provided arguments.
    ///
    /// This method takes a reference to a `HirAnalysisDb` and a slice of `TyId`
    /// arguments, and returns a new instance of the type contained within
    /// the binder with the arguments applied.
    ///
    /// # Parameters
    /// - `db`: A reference to the `HirAnalysisDb`.
    /// - `args`: A slice of `TyId` that will be used to instantiate the type.
    ///
    /// # Returns
    /// A new instance of the type contained within the binder with the
    /// arguments applied.
    pub fn instantiate(self, db: &'db dyn HirAnalysisDb, args: &[TyId<'db>]) -> T {
        let mut folder = InstantiateFolder { db, args };
        self.value.fold_with(&mut folder)
    }

    /// Instantiates the binder with a custom function.
    ///
    /// This method takes a reference to a `HirAnalysisDb` and a closure that
    /// maps a bound variable to `TyId`, and returns a new instance of the
    /// type contained within the binder with the custom function applied.
    ///
    /// # Parameters
    /// - `db`: A reference to the `HirAnalysisDb`.
    /// - `f`: A function that map a bouded variable to a type.
    ///
    /// # Returns
    /// A new instance of the type contained within the binder with the custom
    /// function applied.
    pub fn instantiate_with<F>(self, db: &'db dyn HirAnalysisDb, f: F) -> T
    where
        F: FnMut(TyId<'db>) -> TyId<'db>,
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
    args: &'a [TyId<'db>],
}

impl<'db, 'a> TyFolder<'db> for InstantiateFolder<'db, 'a> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    fn fold_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
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
    F: FnMut(TyId<'db>) -> TyId<'db>,
{
    db: &'db dyn HirAnalysisDb,
    f: F,
    params: FxHashMap<usize, TyId<'db>>,
}

impl<'db, F> TyFolder<'db> for InstantiateWithFolder<'db, F>
where
    F: FnMut(TyId<'db>) -> TyId<'db>,
{
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    fn fold_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
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
