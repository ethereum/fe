use std::rc::Rc;

use fe_analyzer::{db::AnalyzerDbStorage, namespace::items::ContractId, AnalyzerDb};
use fe_common::db::{SourceDb, SourceDbStorage, Upcast, UpcastMut};
use fe_mir::{
    db::{MirDb, MirDbStorage},
    ir::{FunctionBody, FunctionId, FunctionSignature, TypeId},
};
use fe_new_abi::{contract::AbiContract, event::AbiEvent, function::AbiFunction, types::AbiType};

mod queries;

#[salsa::query_group(CodegenDbStorage)]
pub trait CodegenDb: MirDb + Upcast<dyn MirDb> + UpcastMut<dyn MirDb> {
    #[salsa::invoke(queries::function::legalized_signature)]
    fn codegen_legalized_signature(&self, function_id: FunctionId) -> Rc<FunctionSignature>;
    #[salsa::invoke(queries::function::legalized_body)]
    fn codegen_legalized_body(&self, function_id: FunctionId) -> Rc<FunctionBody>;
    #[salsa::invoke(queries::function::symbol_name)]
    fn codegen_function_symbol_name(&self, function_id: FunctionId) -> Rc<String>;

    #[salsa::invoke(queries::types::legalized_type)]
    fn codegen_legalized_type(&self, ty: TypeId) -> TypeId;

    #[salsa::invoke(queries::abi::abi_type)]
    fn codegen_abi_type(&self, ty: TypeId) -> AbiType;
    #[salsa::invoke(queries::abi::abi_function)]
    fn codegen_abi_function(&self, function_id: FunctionId) -> AbiFunction;
    #[salsa::invoke(queries::abi::abi_event)]
    fn codegen_abi_event(&self, ty: TypeId) -> AbiEvent;
    #[salsa::invoke(queries::abi::abi_contract)]
    fn codegen_abi_contract(&self, contract: ContractId) -> AbiContract;
    #[salsa::invoke(queries::abi::abi_type_maximum_size)]
    fn codegen_abi_type_maximum_size(&self, ty: TypeId) -> usize;
    #[salsa::invoke(queries::abi::abi_function_argument_maximum_size)]
    fn codegen_abi_function_argument_maximum_size(&self, contract: FunctionId) -> usize;
    #[salsa::invoke(queries::abi::abi_function_return_maximum_size)]
    fn codegen_abi_function_return_maximum_size(&self, function: FunctionId) -> usize;

    #[salsa::invoke(queries::contract::symbol_name)]
    fn codegen_contract_symbol_name(&self, contract: ContractId) -> Rc<String>;
    #[salsa::invoke(queries::contract::deployer_symbol_name)]
    fn codegen_contract_deployer_symbol_name(&self, contract: ContractId) -> Rc<String>;

    #[salsa::invoke(queries::constant::string_symbol_name)]
    fn codegen_constant_string_symbol_name(&self, data: String) -> Rc<String>;
}

// TODO: Move this to driver.
#[salsa::database(SourceDbStorage, AnalyzerDbStorage, MirDbStorage, CodegenDbStorage)]
#[derive(Default)]
pub struct NewDb {
    storage: salsa::Storage<NewDb>,
}
impl salsa::Database for NewDb {}

impl Upcast<dyn MirDb> for NewDb {
    fn upcast(&self) -> &(dyn MirDb + 'static) {
        &*self
    }
}

impl UpcastMut<dyn MirDb> for NewDb {
    fn upcast_mut(&mut self) -> &mut (dyn MirDb + 'static) {
        &mut *self
    }
}

impl Upcast<dyn SourceDb> for NewDb {
    fn upcast(&self) -> &(dyn SourceDb + 'static) {
        &*self
    }
}

impl UpcastMut<dyn SourceDb> for NewDb {
    fn upcast_mut(&mut self) -> &mut (dyn SourceDb + 'static) {
        &mut *self
    }
}

impl Upcast<dyn AnalyzerDb> for NewDb {
    fn upcast(&self) -> &(dyn AnalyzerDb + 'static) {
        &*self
    }
}

impl UpcastMut<dyn AnalyzerDb> for NewDb {
    fn upcast_mut(&mut self) -> &mut (dyn AnalyzerDb + 'static) {
        &mut *self
    }
}
