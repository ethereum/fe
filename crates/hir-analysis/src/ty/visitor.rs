use crate::HirAnalysisDb;

use super::ty::{AdtDef, InvalidCause, PrimTy, TyConcrete, TyData, TyId, TyParam, TyVar};

pub trait TyVisitor {
    fn visit_ty(&mut self, db: &dyn HirAnalysisDb, ty: TyId) {
        walk_ty(self, db, ty)
    }

    #[allow(unused_variables)]
    fn visit_var(&mut self, db: &dyn HirAnalysisDb, var: &TyVar) {}

    #[allow(unused_variables)]
    fn visit_param(&self, db: &dyn HirAnalysisDb, ty_param: &TyParam) {}

    fn visit_app(&mut self, db: &dyn HirAnalysisDb, abs: TyId, arg: TyId) {
        self.visit_ty(db, abs);
        self.visit_ty(db, arg);
    }

    #[allow(unused_variables)]
    fn visit_ty_con(&mut self, db: &dyn HirAnalysisDb, ty_con: &TyConcrete) {
        walk_ty_con(self, db, ty_con);
    }

    #[allow(unused_variables)]
    fn visit_invalid(&mut self, db: &dyn HirAnalysisDb, cause: &InvalidCause) {}

    #[allow(unused_variables)]
    fn visit_prim(&mut self, db: &dyn HirAnalysisDb, prim: &PrimTy) {}

    #[allow(unused_variables)]
    fn visit_adt(&mut self, db: &dyn HirAnalysisDb, adt: AdtDef) {}
}

pub fn walk_ty<V>(visitor: &mut V, db: &dyn HirAnalysisDb, ty: TyId)
where
    V: TyVisitor + ?Sized,
{
    match ty.data(db) {
        TyData::TyVar(var) => visitor.visit_var(db, &var),

        TyData::TyParam(param) => visitor.visit_param(db, &param),

        TyData::TyApp(abs, arg) => visitor.visit_app(db, abs, arg),

        TyData::TyCon(ty_con) => visitor.visit_ty_con(db, &ty_con),

        TyData::Invalid(cause) => visitor.visit_invalid(db, &cause),
    }
}

pub fn walk_ty_con<V>(visitor: &mut V, db: &dyn HirAnalysisDb, ty_con: &TyConcrete)
where
    V: TyVisitor + ?Sized,
{
    match ty_con {
        TyConcrete::Prim(prim) => visitor.visit_prim(db, prim),
        TyConcrete::Adt(adt) => visitor.visit_adt(db, *adt),
        TyConcrete::Abs => {}
    }
}
