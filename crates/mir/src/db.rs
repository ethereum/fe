use std::rc::Rc;

use fe_analyzer::{
    namespace::{items as analyzer_items, types as analyzer_types},
    AnalyzerDb,
};
use fe_common::db::Upcast;

use crate::ir::{self, ModuleId, TypeId};

mod queries;

#[salsa::query_group(MirDbStorage)]
pub trait MirDb: AnalyzerDb + Upcast<dyn AnalyzerDb> {
    #[salsa::interned]
    fn intern_module(&self, data: Rc<ir::Module>) -> ir::ModuleId;
    #[salsa::interned]
    fn intern_const(&self, data: Rc<ir::Constant>) -> ir::ConstantId;
    #[salsa::interned]
    fn intern_type(&self, data: Rc<ir::Type>) -> ir::TypeId;
    #[salsa::interned]
    fn intern_function(&self, data: Rc<ir::FunctionSignature>) -> ir::FunctionId;

    #[salsa::invoke(queries::module::lowered_module)]
    fn lowered_module(&self, analyzer_module: analyzer_items::ModuleId) -> ModuleId;

    #[salsa::invoke(queries::types::lowered_type)]
    fn lowered_type(&self, analyzer_type: analyzer_types::Type) -> TypeId;
}
