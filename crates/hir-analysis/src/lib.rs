use common::db::Upcast;
use hir::HirDb;

#[salsa::jar(db = HirAnalysisDb)]
pub struct Jar();

pub trait HirAnalysisDb: salsa::DbWithJar<Jar> + Upcast<dyn HirDb> {}
impl<DB> HirAnalysisDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + Upcast<dyn HirDb> {}
