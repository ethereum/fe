use fe_analyzer::namespace::items::ModuleId;
use fe_analyzer::AnalyzerDb;
use fe_common::db::{SourceDb, SourceDbStorage, Upcast, UpcastMut};
use fe_parser::ast;
use std::rc::Rc;

mod queries;

#[salsa::query_group(LoweringDbStorage)]
pub trait LoweringDb: AnalyzerDb + Upcast<dyn AnalyzerDb> {
    #[salsa::invoke(queries::lowered_module_ast)]
    fn lowered_module_ast(&self, module: ModuleId) -> Rc<ast::Module>;
}

#[salsa::database(SourceDbStorage, fe_analyzer::db::AnalyzerDbStorage, LoweringDbStorage)]
#[derive(Default)]
pub struct TestDb {
    storage: salsa::Storage<TestDb>,
}
impl salsa::Database for TestDb {}

impl Upcast<dyn AnalyzerDb> for TestDb {
    fn upcast(&self) -> &(dyn AnalyzerDb + 'static) {
        &*self
    }
}

impl UpcastMut<dyn SourceDb> for TestDb {
    fn upcast_mut(&mut self) -> &mut (dyn SourceDb + 'static) {
        &mut *self
    }
}

impl Upcast<dyn SourceDb> for TestDb {
    fn upcast(&self) -> &(dyn SourceDb + 'static) {
        &*self
    }
}
