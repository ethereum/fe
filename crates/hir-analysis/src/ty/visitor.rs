use std::collections::BTreeSet;

use super::{
    const_ty::{ConstTyData, ConstTyId},
    constraint::{PredicateId, PredicateListId},
    trait_def::{Implementor, TraitInstId, TraitMethod},
    ty_def::{AdtDef, FuncDef, InvalidCause, PrimTy, TyBase, TyData, TyId, TyParam, TyVar},
};
use crate::HirAnalysisDb;

pub trait TypeVisitable<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TypeVisitor<'db>;
}

pub trait TypeVisitor<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb;

    fn visit_ty(&mut self, ty: TyId) {
        walk_ty(self, ty)
    }

    #[allow(unused_variables)]
    fn visit_var(&mut self, var: &TyVar) {}

    #[allow(unused_variables)]
    fn visit_param(&mut self, ty_param: &TyParam) {}

    #[allow(unused_variables)]
    fn visit_const_param(&mut self, ty_param: &TyParam, const_ty_ty: TyId) {}

    fn visit_app(&mut self, abs: TyId, arg: TyId) {
        self.visit_ty(abs);
        self.visit_ty(arg);
    }

    #[allow(unused_variables)]
    fn visit_ty_base(&mut self, ty_base: &TyBase) {
        walk_ty_base(self, ty_base);
    }

    #[allow(unused_variables)]
    fn visit_invalid(&mut self, cause: &InvalidCause) {}

    #[allow(unused_variables)]
    fn visit_prim(&mut self, prim: &PrimTy) {}

    #[allow(unused_variables)]
    fn visit_adt(&mut self, adt: AdtDef) {}

    #[allow(unused_variables)]
    fn visit_func(&mut self, func: FuncDef) {}

    #[allow(unused_variables)]
    fn visit_const_ty(&mut self, const_ty: &ConstTyId) {
        walk_const_ty(self, const_ty)
    }
}

pub fn walk_ty<'db, V>(visitor: &mut V, ty: TyId)
where
    V: TypeVisitor<'db> + ?Sized,
{
    match ty.data(visitor.db()) {
        TyData::TyVar(var) => visitor.visit_var(var),

        TyData::TyParam(param) => visitor.visit_param(param),

        TyData::TyApp(abs, arg) => visitor.visit_app(*abs, *arg),

        TyData::TyBase(ty_con) => visitor.visit_ty_base(ty_con),

        TyData::ConstTy(const_ty) => visitor.visit_const_ty(const_ty),

        TyData::Bot => {}

        TyData::Invalid(cause) => visitor.visit_invalid(cause),
    }
}

pub fn walk_ty_base<'db, V>(visitor: &mut V, ty_con: &TyBase)
where
    V: TypeVisitor<'db> + ?Sized,
{
    match ty_con {
        TyBase::Prim(prim) => visitor.visit_prim(prim),
        TyBase::Adt(adt) => visitor.visit_adt(*adt),
        TyBase::Func(func) => visitor.visit_func(*func),
    }
}

pub fn walk_const_ty<'db, V>(visitor: &mut V, const_ty: &ConstTyId)
where
    V: TypeVisitor<'db> + ?Sized,
{
    let db = visitor.db();
    visitor.visit_ty(const_ty.ty(db));
    match &const_ty.data(db) {
        ConstTyData::TyVar(var, _) => visitor.visit_var(var),
        ConstTyData::TyParam(param, ty) => visitor.visit_const_param(param, *ty),
        ConstTyData::Evaluated(..) | ConstTyData::UnEvaluated(..) => {}
    }
}

impl<'db> TypeVisitable<'db> for TyId {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TypeVisitor<'db>,
    {
        visitor.visit_ty(*self)
    }
}

impl<'db, T> TypeVisitable<'db> for Vec<T>
where
    T: TypeVisitable<'db>,
{
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TypeVisitor<'db>,
    {
        self.iter().for_each(|ty| ty.visit_with(visitor))
    }
}

impl<'db, T> TypeVisitable<'db> for &[T]
where
    T: TypeVisitable<'db>,
{
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TypeVisitor<'db>,
    {
        self.iter().for_each(|ty| ty.visit_with(visitor))
    }
}

impl<'db, T> TypeVisitable<'db> for BTreeSet<T>
where
    T: TypeVisitable<'db>,
{
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TypeVisitor<'db>,
    {
        self.iter().for_each(|ty| ty.visit_with(visitor))
    }
}

impl<'db> TypeVisitable<'db> for TraitInstId {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TypeVisitor<'db>,
    {
        let db = visitor.db();
        self.args(db).visit_with(visitor);
    }
}

impl<'db> TypeVisitable<'db> for Implementor {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TypeVisitor<'db>,
    {
        let db = visitor.db();
        self.ty(db).visit_with(visitor);
        self.params(db).visit_with(visitor);
    }
}

impl<'db> TypeVisitable<'db> for PredicateId {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TypeVisitor<'db>,
    {
        self.ty(visitor.db()).visit_with(visitor);
        self.trait_inst(visitor.db()).visit_with(visitor);
    }
}

impl<'db> TypeVisitable<'db> for PredicateListId {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TypeVisitor<'db>,
    {
        self.predicates(visitor.db()).visit_with(visitor)
    }
}
