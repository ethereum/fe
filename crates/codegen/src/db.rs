use std::rc::Rc;

use fe_abi::{contract::AbiContract, event::AbiEvent, function::AbiFunction, types::AbiType};
use fe_analyzer::{
    db::AnalyzerDbStorage,
    namespace::items::{ContractId, IngotId, ModuleId},
    AnalyzerDb,
};
use fe_common::db::{SourceDb, SourceDbStorage, Upcast, UpcastMut};
use fe_mir::{
    db::{MirDb, MirDbStorage},
    ir::{FunctionSigId, FunctionSignature, TypeId},
};
use fxhash::FxHashMap;

use crate::cgu::{CodegenUnit, CodegenUnitId};

mod queries;

#[salsa::query_group(CodegenDbStorage)]
pub trait CodegenDb: MirDb + Upcast<dyn MirDb> + UpcastMut<dyn MirDb> {
    #[salsa::interned]
    fn codegen_intern_cgu(&self, data: Rc<CodegenUnit>) -> CodegenUnitId;
    #[salsa::invoke(queries::cgu::generate_cgu)]
    fn codegen_generate_cgu(&self, ingot: IngotId) -> Rc<FxHashMap<ModuleId, CodegenUnitId>>;

    #[salsa::invoke(queries::function::legalized_signature)]
    fn codegen_legalized_signature(&self, function_id: FunctionSigId) -> Rc<FunctionSignature>;
    #[salsa::invoke(queries::function::symbol_name)]
    fn codegen_function_symbol_name(&self, function_id: FunctionSigId) -> Rc<String>;

    #[salsa::invoke(queries::types::legalized_type)]
    fn codegen_legalized_type(&self, ty: TypeId) -> TypeId;

    #[salsa::invoke(queries::abi::abi_type)]
    fn codegen_abi_type(&self, ty: TypeId) -> AbiType;
    #[salsa::invoke(queries::abi::abi_function)]
    fn codegen_abi_function(&self, function_id: FunctionSigId) -> AbiFunction;
    #[salsa::invoke(queries::abi::abi_event)]
    fn codegen_abi_event(&self, ty: TypeId) -> AbiEvent;
    #[salsa::invoke(queries::abi::abi_contract)]
    fn codegen_abi_contract(&self, contract: ContractId) -> AbiContract;
    #[salsa::invoke(queries::abi::abi_type_maximum_size)]
    fn codegen_abi_type_maximum_size(&self, ty: TypeId) -> usize;
    #[salsa::invoke(queries::abi::abi_type_minimum_size)]
    fn codegen_abi_type_minimum_size(&self, ty: TypeId) -> usize;
    #[salsa::invoke(queries::abi::abi_function_argument_maximum_size)]
    fn codegen_abi_function_argument_maximum_size(&self, contract: FunctionSigId) -> usize;
    #[salsa::invoke(queries::abi::abi_function_return_maximum_size)]
    fn codegen_abi_function_return_maximum_size(&self, function: FunctionSigId) -> usize;

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
pub struct Db {
    storage: salsa::Storage<Db>,
}
impl salsa::Database for Db {}

impl Upcast<dyn MirDb> for Db {
    fn upcast(&self) -> &(dyn MirDb + 'static) {
        self
    }
}

impl UpcastMut<dyn MirDb> for Db {
    fn upcast_mut(&mut self) -> &mut (dyn MirDb + 'static) {
        &mut *self
    }
}

impl Upcast<dyn SourceDb> for Db {
    fn upcast(&self) -> &(dyn SourceDb + 'static) {
        self
    }
}

impl UpcastMut<dyn SourceDb> for Db {
    fn upcast_mut(&mut self) -> &mut (dyn SourceDb + 'static) {
        &mut *self
    }
}

impl Upcast<dyn AnalyzerDb> for Db {
    fn upcast(&self) -> &(dyn AnalyzerDb + 'static) {
        self
    }
}

impl UpcastMut<dyn AnalyzerDb> for Db {
    fn upcast_mut(&mut self) -> &mut (dyn AnalyzerDb + 'static) {
        &mut *self
    }
}
