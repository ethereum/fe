use std::hash::Hash;

use common::indexmap::{IndexMap, IndexSet};

use super::{
    trait_def::{Implementor, TraitInstId},
    trait_resolution::PredicateListId,
    ty_check::ExprProp,
    ty_def::{TyData, TyId},
    visitor::TyVisitable,
};
use crate::{
    ty::const_ty::{ConstTyData, ConstTyId},
    HirAnalysisDb,
};

pub trait TyFoldable<'db>
where
    Self: Sized + TyVisitable<'db>,
{
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>;

    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self.super_fold_with(folder)
    }
}

pub trait TyFolder<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb;
    fn fold_ty(&mut self, ty: TyId<'db>) -> TyId<'db>;
}

impl<'db> TyFoldable<'db> for TyId<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        use TyData::*;

        let db = folder.db();
        match self.data(db) {
            TyApp(abs, arg) => {
                let abs = folder.fold_ty(*abs);
                let arg = folder.fold_ty(*arg);

                TyId::app(db, abs, arg)
            }

            ConstTy(cty) => {
                use ConstTyData::*;
                let cty_data = match cty.data(db) {
                    TyVar(var, ty) => {
                        let ty = folder.fold_ty(*ty);
                        TyVar(var.clone(), ty)
                    }
                    TyParam(param, ty) => {
                        let ty = folder.fold_ty(*ty);
                        TyParam(param.clone(), ty)
                    }
                    Evaluated(val, ty) => {
                        let ty = folder.fold_ty(*ty);
                        Evaluated(val.clone(), ty)
                    }
                    UnEvaluated(body) => UnEvaluated(*body),
                };

                let const_ty = ConstTyId::new(db, cty_data);
                TyId::const_ty(db, const_ty)
            }

            TyVar(_) | TyParam(_) | TyBase(_) | Never | Invalid(_) => self,
        }
    }

    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        folder.fold_ty(self)
    }
}

impl<'db, T> TyFoldable<'db> for Vec<T>
where
    T: TyFoldable<'db>,
{
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self.into_iter()
            .map(|inner| inner.fold_with(folder))
            .collect()
    }
}

impl<'db, T> TyFoldable<'db> for IndexSet<T>
where
    T: TyFoldable<'db> + Hash + Eq,
{
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self.into_iter().map(|ty| ty.fold_with(folder)).collect()
    }
}

impl<'db> TyFoldable<'db> for TraitInstId<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let db = folder.db();
        let def = self.def(db);
        let args = self
            .args(db)
            .iter()
            .map(|ty| ty.fold_with(folder))
            .collect::<Vec<_>>();

        TraitInstId::new(db, def, args)
    }
}

impl<'db> TyFoldable<'db> for Implementor<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let db = folder.db();
        let trait_inst = self.trait_(db).fold_with(folder);
        let params = self
            .params(db)
            .iter()
            .map(|ty| ty.fold_with(folder))
            .collect::<Vec<_>>();
        let hir_impl_trait = self.hir_impl_trait(db);

        let types = self
            .types(db)
            .iter()
            .map(|(ident, ty)| (*ident, ty.fold_with(folder)))
            .collect::<IndexMap<_, _>>();

        Implementor::new(db, trait_inst, params, types, hir_impl_trait)
    }
}

impl<'db> TyFoldable<'db> for PredicateListId<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let predicates = self
            .list(folder.db())
            .iter()
            .map(|pred| pred.fold_with(folder))
            .collect::<Vec<_>>();

        Self::new(folder.db(), predicates)
    }
}

impl<'db> TyFoldable<'db> for ExprProp<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let ty = self.ty.fold_with(folder);
        Self { ty, ..self }
    }
}
