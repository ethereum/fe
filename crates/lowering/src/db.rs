use fe_analyzer::namespace::items::{IngotId, ModuleId};
use fe_analyzer::AnalyzerDb;
use fe_common::Upcast;

mod queries;

#[salsa::query_group(LoweringDbStorage)]
pub trait LoweringDb: AnalyzerDb + Upcast<dyn AnalyzerDb> {
    #[salsa::invoke(queries::lowered_ingot)]
    fn lowered_ingot(&self, ingot: IngotId) -> IngotId;

    #[salsa::invoke(queries::lowered_module)]
    fn lowered_module(&self, module: ModuleId) -> ModuleId;
}

#[salsa::database(fe_analyzer::db::AnalyzerDbStorage, LoweringDbStorage)]
#[derive(Default)]
pub struct TestDb {
    storage: salsa::Storage<TestDb>,
}
impl salsa::Database for TestDb {}

impl Upcast<dyn LoweringDb> for TestDb {
    fn upcast(&self) -> &(dyn LoweringDb + 'static) {
        &*self
    }
}

impl Upcast<dyn AnalyzerDb> for TestDb {
    fn upcast(&self) -> &(dyn AnalyzerDb + 'static) {
        &*self
    }
}
