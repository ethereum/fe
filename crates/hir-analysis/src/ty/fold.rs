use std::collections::BTreeSet;

use super::{
    constraint::{PredicateId, PredicateListId},
    trait_def::{Implementor, TraitInstId, TraitMethod},
    ty_def::{FuncDef, TyData, TyId},
    visitor::TypeVisitable,
};
use crate::HirAnalysisDb;

pub trait TypeFoldable<'db>
where
    Self: Sized + TypeVisitable<'db>,
{
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>;

    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        self.super_fold_with(folder)
    }
}

pub trait TypeFolder<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb;
    fn fold_ty(&mut self, ty: TyId) -> TyId;
}

impl<'db> TypeFoldable<'db> for TyId {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        use TyData::*;

        let db = folder.db();
        match self.data(db) {
            TyApp(abs, arg) => {
                let abs = folder.fold_ty(*abs);
                let arg = folder.fold_ty(*arg);

                TyId::app(db, abs, arg)
            }

            TyVar(_) | TyParam(_) | TyBase(_) | ConstTy(_) | Bot | Invalid(_) => self,
        }
    }

    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        folder.fold_ty(self)
    }
}

impl<'db, T> TypeFoldable<'db> for Vec<T>
where
    T: TypeFoldable<'db>,
{
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        self.into_iter()
            .map(|inner| inner.fold_with(folder))
            .collect()
    }
}

impl<'db, T> TypeFoldable<'db> for BTreeSet<T>
where
    T: TypeFoldable<'db> + Ord,
{
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        self.into_iter().map(|ty| ty.fold_with(folder)).collect()
    }
}

impl<'db> TypeFoldable<'db> for TraitInstId {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        let db = folder.db();
        let def = self.def(db);
        let args = self
            .args(db)
            .iter()
            .map(|ty| ty.fold_with(folder))
            .collect();

        TraitInstId::new(db, def, args)
    }
}

impl<'db> TypeFoldable<'db> for Implementor {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        let db = folder.db();
        let trait_inst = self.trait_(db).fold_with(folder);
        let ty = self.ty(db).fold_with(folder);
        let params = self
            .params(db)
            .iter()
            .map(|ty| ty.fold_with(folder))
            .collect();
        let hir_impl_trait = self.hir_impl_trait(db);

        Implementor::new(db, trait_inst, ty, params, hir_impl_trait)
    }
}

impl<'db> TypeFoldable<'db> for FuncDef {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        let db = folder.db();
        let arg_tys = self
            .arg_tys(db)
            .iter()
            .map(|ty| ty.fold_with(folder))
            .collect();
        let ret_ty = self.ret_ty(db).fold_with(folder);

        FuncDef::new(
            db,
            self.hir_func(db),
            self.name(db),
            self.params_set(db),
            arg_tys,
            self.ret_ty(db),
        )
    }
}

impl<'db> TypeFoldable<'db> for TraitMethod {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        TraitMethod(self.0.fold_with(folder))
    }
}

impl<'db> TypeFoldable<'db> for PredicateId {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        let ty = self.ty(folder.db()).fold_with(folder);
        let trait_inst = self.trait_inst(folder.db()).fold_with(folder);
        Self::new(folder.db(), ty, trait_inst)
    }
}

impl<'db> TypeFoldable<'db> for PredicateListId {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TypeFolder<'db>,
    {
        let predicates = self
            .predicates(folder.db())
            .iter()
            .map(|pred| pred.fold_with(folder))
            .collect();

        Self::new(folder.db(), predicates, self.ingot(folder.db()))
    }
}
