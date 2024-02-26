use super::{
    const_ty::{ConstTyData, ConstTyId},
    ty_def::{AdtDef, FuncDef, InvalidCause, PrimTy, TyBase, TyData, TyId, TyParam, TyVar},
};
use crate::HirAnalysisDb;

pub trait TyVisitor {
    fn visit_ty(&mut self, db: &dyn HirAnalysisDb, ty: TyId) {
        walk_ty(self, db, ty)
    }

    #[allow(unused_variables)]
    fn visit_var(&mut self, db: &dyn HirAnalysisDb, var: &TyVar) {}

    #[allow(unused_variables)]
    fn visit_param(&mut self, db: &dyn HirAnalysisDb, ty_param: &TyParam) {}

    #[allow(unused_variables)]
    fn visit_const_param(&mut self, db: &dyn HirAnalysisDb, ty_param: &TyParam, const_ty_ty: TyId) {
    }

    fn visit_app(&mut self, db: &dyn HirAnalysisDb, abs: TyId, arg: TyId) {
        self.visit_ty(db, abs);
        self.visit_ty(db, arg);
    }

    #[allow(unused_variables)]
    fn visit_ty_base(&mut self, db: &dyn HirAnalysisDb, ty_base: &TyBase) {
        walk_ty_base(self, db, ty_base);
    }

    #[allow(unused_variables)]
    fn visit_invalid(&mut self, db: &dyn HirAnalysisDb, cause: &InvalidCause) {}

    #[allow(unused_variables)]
    fn visit_prim(&mut self, db: &dyn HirAnalysisDb, prim: &PrimTy) {}

    #[allow(unused_variables)]
    fn visit_adt(&mut self, db: &dyn HirAnalysisDb, adt: AdtDef) {}

    #[allow(unused_variables)]
    fn visit_func(&mut self, db: &dyn HirAnalysisDb, func: FuncDef) {}

    #[allow(unused_variables)]
    fn visit_const_ty(&mut self, db: &dyn HirAnalysisDb, const_ty: &ConstTyId) {
        walk_const_ty(self, db, const_ty)
    }
}

pub fn walk_ty<V>(visitor: &mut V, db: &dyn HirAnalysisDb, ty: TyId)
where
    V: TyVisitor + ?Sized,
{
    match ty.data(db) {
        TyData::TyVar(var) => visitor.visit_var(db, var),

        TyData::TyParam(param) => visitor.visit_param(db, param),

        TyData::TyApp(abs, arg) => visitor.visit_app(db, *abs, *arg),

        TyData::TyBase(ty_con) => visitor.visit_ty_base(db, ty_con),

        TyData::ConstTy(const_ty) => visitor.visit_const_ty(db, const_ty),
        
        TyData::Bot => {}

        TyData::Invalid(cause) => visitor.visit_invalid(db, cause),
    }
}

pub fn walk_ty_base<V>(visitor: &mut V, db: &dyn HirAnalysisDb, ty_con: &TyBase)
where
    V: TyVisitor + ?Sized,
{
    match ty_con {
        TyBase::Prim(prim) => visitor.visit_prim(db, prim),
        TyBase::Adt(adt) => visitor.visit_adt(db, *adt),
        TyBase::Func(func) => visitor.visit_func(db, *func),
    }
}

pub fn walk_const_ty<V>(visitor: &mut V, db: &dyn HirAnalysisDb, const_ty: &ConstTyId)
where
    V: TyVisitor + ?Sized,
{
    visitor.visit_ty(db, const_ty.ty(db));
    match &const_ty.data(db) {
        ConstTyData::TyVar(var, _) => visitor.visit_var(db, var),
        ConstTyData::TyParam(param, ty) => visitor.visit_const_param(db, param, *ty),
        ConstTyData::Evaluated(..) | ConstTyData::UnEvaluated(..) => {}
    }
}
