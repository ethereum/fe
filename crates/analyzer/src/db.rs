use crate::context::{Analysis, FunctionBody};
use crate::errors::TypeError;
use crate::namespace::items::{
    self, ContractFieldId, ContractId, EventId, FunctionId, ModuleId, StructFieldId, StructId,
    TypeAliasId, TypeDefId,
};
use crate::namespace::types;
use indexmap::IndexMap;
use std::rc::Rc;

mod queries;

// from rust-analyzer {
#[macro_export]
macro_rules! impl_intern_key {
    ($name:ident) => {
        impl salsa::InternKey for $name {
            fn from_intern_id(v: salsa::InternId) -> Self {
                $name(v.as_u32())
            }
            fn as_intern_id(&self) -> salsa::InternId {
                salsa::InternId::from(self.0)
            }
        }
    };
}
// } from rust-analyzer

#[salsa::query_group(AnalyzerDbStorage)]
pub trait AnalyzerDb {
    #[salsa::interned]
    fn intern_module(&self, data: Rc<items::Module>) -> ModuleId;
    #[salsa::interned]
    fn intern_struct(&self, data: Rc<items::Struct>) -> StructId;
    #[salsa::interned]
    fn intern_struct_field(&self, data: Rc<items::StructField>) -> StructFieldId;
    #[salsa::interned]
    fn intern_type_alias(&self, data: Rc<items::TypeAlias>) -> TypeAliasId;
    #[salsa::interned]
    fn intern_contract(&self, data: Rc<items::Contract>) -> ContractId;
    #[salsa::interned]
    fn intern_contract_field(&self, data: Rc<items::ContractField>) -> ContractFieldId;
    #[salsa::interned]
    fn intern_function(&self, data: Rc<items::Function>) -> FunctionId;
    #[salsa::interned]
    fn intern_event(&self, data: Rc<items::Event>) -> EventId;

    // Module
    #[salsa::invoke(queries::module::module_all_type_defs)]
    fn module_all_type_defs(&self, module: ModuleId) -> Rc<Vec<TypeDefId>>;
    #[salsa::invoke(queries::module::module_type_def_map)]
    fn module_type_def_map(&self, module: ModuleId) -> Analysis<Rc<IndexMap<String, TypeDefId>>>;
    #[salsa::invoke(queries::module::module_resolve_type)]
    #[salsa::cycle(queries::module::module_resolve_type_cycle)]
    fn module_resolve_type(
        &self,
        module: ModuleId,
        name: String,
    ) -> Option<Result<types::Type, TypeError>>;
    #[salsa::invoke(queries::module::module_contracts)]
    fn module_contracts(&self, module: ModuleId) -> Rc<Vec<ContractId>>;
    #[salsa::invoke(queries::module::module_structs)]
    fn module_structs(&self, module: ModuleId) -> Rc<Vec<StructId>>;

    // Contract
    #[salsa::invoke(queries::contracts::contract_all_functions)]
    fn contract_all_functions(&self, id: ContractId) -> Rc<Vec<FunctionId>>;
    #[salsa::invoke(queries::contracts::contract_function_map)]
    fn contract_function_map(&self, id: ContractId) -> Analysis<Rc<IndexMap<String, FunctionId>>>;
    #[salsa::invoke(queries::contracts::contract_public_function_map)]
    fn contract_public_function_map(&self, id: ContractId) -> Rc<IndexMap<String, FunctionId>>;
    #[salsa::invoke(queries::contracts::contract_init_function)]
    fn contract_init_function(&self, id: ContractId) -> Analysis<Option<FunctionId>>;

    #[salsa::invoke(queries::contracts::contract_all_events)]
    fn contract_all_events(&self, id: ContractId) -> Rc<Vec<EventId>>;
    #[salsa::invoke(queries::contracts::contract_event_map)]
    fn contract_event_map(&self, id: ContractId) -> Analysis<Rc<IndexMap<String, EventId>>>;

    #[salsa::invoke(queries::contracts::contract_all_fields)]
    fn contract_all_fields(&self, id: ContractId) -> Rc<Vec<ContractFieldId>>;
    #[salsa::invoke(queries::contracts::contract_field_map)]
    fn contract_field_map(&self, id: ContractId)
        -> Analysis<Rc<IndexMap<String, ContractFieldId>>>;
    #[salsa::invoke(queries::contracts::contract_field_type)]
    fn contract_field_type(
        &self,
        field: ContractFieldId,
    ) -> Analysis<Result<types::Type, TypeError>>;

    // Function
    #[salsa::invoke(queries::functions::function_signature)]
    fn function_signature(&self, id: FunctionId) -> Analysis<Rc<types::FunctionSignature>>;
    #[salsa::invoke(queries::functions::function_body)]
    fn function_body(&self, id: FunctionId) -> Analysis<Rc<FunctionBody>>;

    // Struct
    #[salsa::invoke(queries::structs::struct_type)]
    fn struct_type(&self, id: StructId) -> Rc<types::Struct>;
    #[salsa::invoke(queries::structs::struct_all_fields)]
    fn struct_all_fields(&self, id: StructId) -> Rc<Vec<StructFieldId>>;
    #[salsa::invoke(queries::structs::struct_field_map)]
    fn struct_field_map(&self, id: StructId) -> Analysis<Rc<IndexMap<String, StructFieldId>>>;
    #[salsa::invoke(queries::structs::struct_field_type)]
    fn struct_field_type(
        &self,
        field: StructFieldId,
    ) -> Analysis<Result<types::FixedSize, TypeError>>;

    // Event
    #[salsa::invoke(queries::events::event_type)]
    fn event_type(&self, event: EventId) -> Analysis<Rc<types::Event>>;

    // Type alias
    #[salsa::invoke(queries::types::type_alias_type)]
    #[salsa::cycle(queries::types::type_alias_type_cycle)]
    fn type_alias_type(&self, id: TypeAliasId) -> Analysis<Result<types::Type, TypeError>>;
}

#[salsa::database(AnalyzerDbStorage)]
#[derive(Default)]
pub struct Db {
    storage: salsa::Storage<Db>,
}
impl salsa::Database for Db {}
