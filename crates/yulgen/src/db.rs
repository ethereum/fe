use crate::types::AbiType;
use fe_analyzer::namespace::items::{ContractId, EventId, FunctionId, ModuleId, StructId};
use fe_analyzer::AnalyzerDb;
use fe_common::Upcast;
use fe_lowering::LoweringDb;
use indexmap::{IndexMap, IndexSet};
use smol_str::SmolStr;
use std::rc::Rc;
use yultsur::yul;

mod queries;

#[salsa::query_group(YulgenDbStorage)]
pub trait YulgenDb:
    AnalyzerDb + LoweringDb + Upcast<dyn AnalyzerDb> + Upcast<dyn LoweringDb>
{
    #[salsa::invoke(queries::compile_module)]
    fn compile_module(&self, module_id: ModuleId) -> IndexMap<String, String>;

    #[salsa::invoke(queries::contracts::contract_object)]
    fn contract_object(&self, contract: ContractId) -> yul::Object;
    #[salsa::invoke(queries::contracts::contract_abi_dispatcher)]
    fn contract_abi_dispatcher(&self, contract: ContractId) -> Vec<yul::Statement>;

    #[salsa::invoke(queries::functions::function_yul_name)]
    fn function_yul_name(&self, function: FunctionId) -> SmolStr;
    #[salsa::invoke(queries::functions::function_external_call_name)]
    fn function_external_call_name(&self, function: FunctionId) -> SmolStr;
    #[salsa::invoke(queries::functions::function_external_call_fn)]
    fn function_external_call_fn(&self, function: FunctionId) -> Vec<yul::Statement>;
    #[salsa::invoke(queries::functions::function_def)]
    fn function_def(&self, function: FunctionId) -> yul::Statement;
    #[salsa::invoke(queries::functions::function_sig_abi_types)]
    fn function_sig_abi_types(&self, function: FunctionId) -> (Rc<[AbiType]>, Option<AbiType>);
    #[salsa::invoke(queries::functions::assert_string_types)]
    fn function_assert_string_types(&self, function: FunctionId) -> Rc<IndexSet<AbiType>>;
    #[salsa::invoke(queries::functions::revert_types)]
    fn function_revert_errors(&self, function: FunctionId) -> Rc<IndexSet<StructId>>;

    #[salsa::invoke(queries::events::event_idx_abi_types)]
    fn event_idx_abi_types(&self, event: EventId) -> Rc<[AbiType]>;

    #[salsa::invoke(queries::structs::struct_abi_type)]
    fn struct_abi_type(&self, id: StructId) -> AbiType;
    #[salsa::invoke(queries::structs::struct_field_abi_types)]
    fn struct_field_abi_types(&self, id: StructId) -> Rc<[AbiType]>;
    #[salsa::invoke(queries::structs::struct_qualified_name)]
    fn struct_qualified_name(&self, id: StructId) -> SmolStr;
    #[salsa::invoke(queries::structs::struct_getter_name)]
    fn struct_getter_name(&self, id: StructId, field: SmolStr, deref: bool) -> SmolStr;
    #[salsa::invoke(queries::structs::struct_getter_fn)]
    fn struct_getter_fn(&self, id: StructId, field: SmolStr, deref: bool) -> yul::Statement;
    #[salsa::invoke(queries::structs::struct_init_name)]
    fn struct_init_name(&self, id: StructId) -> SmolStr;
    #[salsa::invoke(queries::structs::struct_init_fn)]
    fn struct_init_fn(&self, id: StructId) -> yul::Statement;
    #[salsa::invoke(queries::structs::struct_api_fns)]
    fn struct_api_fns(&self, id: StructId) -> Vec<yul::Statement>;
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
