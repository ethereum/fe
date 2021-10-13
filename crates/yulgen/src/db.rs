use fe_analyzer::namespace::items::ModuleId;
use fe_analyzer::AnalyzerDb;
use fe_common::Upcast;
use fe_lowering::LoweringDb;
use indexmap::map::IndexMap;

mod queries;

#[salsa::query_group(YulgenDbStorage)]
pub trait YulgenDb:
    AnalyzerDb + LoweringDb + Upcast<dyn AnalyzerDb> + Upcast<dyn LoweringDb>
{
    #[salsa::invoke(queries::compile_module)]
    fn compile_module(&self, module_id: ModuleId) -> IndexMap<String, String>;
}

#[salsa::database(
    fe_analyzer::db::AnalyzerDbStorage,
    fe_lowering::db::LoweringDbStorage,
    YulgenDbStorage
)]
#[derive(Default)]
pub struct Db {
    storage: salsa::Storage<Db>,
}
impl salsa::Database for Db {}

impl Upcast<dyn LoweringDb> for Db {
    fn upcast(&self) -> &(dyn LoweringDb + 'static) {
        &*self
    }
}

impl Upcast<dyn AnalyzerDb> for Db {
    fn upcast(&self) -> &(dyn AnalyzerDb + 'static) {
        &*self
    }
}

impl Upcast<dyn YulgenDb> for Db {
    fn upcast(&self) -> &(dyn YulgenDb + 'static) {
        &*self
    }
}
