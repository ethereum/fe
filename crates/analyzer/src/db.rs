use crate::namespace::items::{
    self, AttributeId, ContractFieldId, ContractId, DepGraphWrapper, EnumVariantKind, FunctionId,
    FunctionSigId, ImplId, IngotId, Item, ModuleConstantId, ModuleId, StructFieldId, StructId,
    TraitId, TypeAliasId,
};
use crate::namespace::types::{self, Type, TypeId};
use crate::{
    context::{Analysis, Constant, FunctionBody},
    namespace::items::EnumId,
};
use crate::{
    errors::{ConstEvalError, TypeError},
    namespace::items::EnumVariantId,
};
use fe_common::db::{SourceDb, SourceDbStorage, Upcast, UpcastMut};
use fe_common::{SourceFileId, Span};
use fe_parser::ast;
use indexmap::map::IndexMap;
use smol_str::SmolStr;
use std::rc::Rc;
mod queries;

#[salsa::query_group(AnalyzerDbStorage)]
pub trait AnalyzerDb: SourceDb + Upcast<dyn SourceDb> + UpcastMut<dyn SourceDb> {
    #[salsa::interned]
    fn intern_ingot(&self, data: Rc<items::Ingot>) -> IngotId;
    #[salsa::interned]
    fn intern_module(&self, data: Rc<items::Module>) -> ModuleId;
    #[salsa::interned]
    fn intern_module_const(&self, data: Rc<items::ModuleConstant>) -> ModuleConstantId;
    #[salsa::interned]
    fn intern_struct(&self, data: Rc<items::Struct>) -> StructId;
    #[salsa::interned]
    fn intern_struct_field(&self, data: Rc<items::StructField>) -> StructFieldId;
    #[salsa::interned]
    fn intern_enum(&self, data: Rc<items::Enum>) -> EnumId;
    #[salsa::interned]
    fn intern_attribute(&self, data: Rc<items::Attribute>) -> AttributeId;
    #[salsa::interned]
    fn intern_enum_variant(&self, data: Rc<items::EnumVariant>) -> EnumVariantId;
    #[salsa::interned]
    fn intern_trait(&self, data: Rc<items::Trait>) -> TraitId;
    #[salsa::interned]
    fn intern_impl(&self, data: Rc<items::Impl>) -> ImplId;
    #[salsa::interned]
    fn intern_type_alias(&self, data: Rc<items::TypeAlias>) -> TypeAliasId;
    #[salsa::interned]
    fn intern_contract(&self, data: Rc<items::Contract>) -> ContractId;
    #[salsa::interned]
    fn intern_contract_field(&self, data: Rc<items::ContractField>) -> ContractFieldId;
    #[salsa::interned]
    fn intern_function_sig(&self, data: Rc<items::FunctionSig>) -> FunctionSigId;
    #[salsa::interned]
    fn intern_function(&self, data: Rc<items::Function>) -> FunctionId;
    #[salsa::interned]
    fn intern_type(&self, data: Type) -> TypeId;

    // Ingot

    // These are inputs so that the (future) language server can add
    // and remove files/dependencies. Set via eg `db.set_ingot_files`.
    // If an input is used before it's set, salsa will panic.
    #[salsa::input]
    fn ingot_files(&self, ingot: IngotId) -> Rc<[SourceFileId]>;
    #[salsa::input]
    fn ingot_external_ingots(&self, ingot: IngotId) -> Rc<IndexMap<SmolStr, IngotId>>;
    // Having the root ingot available as a "global" might offend functional
    // programming purists but it makes for much nicer ergonomics in queries
    // that just need the global entrypoint
    #[salsa::input]
    fn root_ingot(&self) -> IngotId;

    #[salsa::invoke(queries::ingots::ingot_modules)]
    fn ingot_modules(&self, ingot: IngotId) -> Rc<[ModuleId]>;
    #[salsa::invoke(queries::ingots::ingot_root_module)]
    fn ingot_root_module(&self, ingot: IngotId) -> Option<ModuleId>;

    // Module
    #[salsa::invoke(queries::module::module_file_path)]
    fn module_file_path(&self, module: ModuleId) -> SmolStr;
    #[salsa::invoke(queries::module::module_parse)]
    fn module_parse(&self, module: ModuleId) -> Analysis<Rc<ast::Module>>;
    #[salsa::invoke(queries::module::module_is_incomplete)]
    fn module_is_incomplete(&self, module: ModuleId) -> bool;
    #[salsa::invoke(queries::module::module_all_items)]
    fn module_all_items(&self, module: ModuleId) -> Rc<[Item]>;
    #[salsa::invoke(queries::module::module_all_impls)]
    fn module_all_impls(&self, module: ModuleId) -> Analysis<Rc<[ImplId]>>;
    #[salsa::invoke(queries::module::module_item_map)]
    fn module_item_map(&self, module: ModuleId) -> Analysis<Rc<IndexMap<SmolStr, Item>>>;
    #[salsa::invoke(queries::module::module_impl_map)]
    fn module_impl_map(
        &self,
        module: ModuleId,
    ) -> Analysis<Rc<IndexMap<(TraitId, TypeId), ImplId>>>;
    #[salsa::invoke(queries::module::module_contracts)]
    fn module_contracts(&self, module: ModuleId) -> Rc<[ContractId]>;
    #[salsa::invoke(queries::module::module_structs)]
    fn module_structs(&self, module: ModuleId) -> Rc<[StructId]>;
    #[salsa::invoke(queries::module::module_constants)]
    fn module_constants(&self, module: ModuleId) -> Rc<Vec<ModuleConstantId>>;
    #[salsa::invoke(queries::module::module_used_item_map)]
    fn module_used_item_map(
        &self,
        module: ModuleId,
    ) -> Analysis<Rc<IndexMap<SmolStr, (Span, Item)>>>;
    #[salsa::invoke(queries::module::module_parent_module)]
    fn module_parent_module(&self, module: ModuleId) -> Option<ModuleId>;
    #[salsa::invoke(queries::module::module_submodules)]
    fn module_submodules(&self, module: ModuleId) -> Rc<[ModuleId]>;
    #[salsa::invoke(queries::module::module_tests)]
    fn module_tests(&self, module: ModuleId) -> Vec<FunctionId>;

    // Module Constant
    #[salsa::cycle(queries::module::module_constant_type_cycle)]
    #[salsa::invoke(queries::module::module_constant_type)]
    fn module_constant_type(&self, id: ModuleConstantId) -> Analysis<Result<TypeId, TypeError>>;
    #[salsa::cycle(queries::module::module_constant_value_cycle)]
    #[salsa::invoke(queries::module::module_constant_value)]
    fn module_constant_value(
        &self,
        id: ModuleConstantId,
    ) -> Analysis<Result<Constant, ConstEvalError>>;

    // Contract
    #[salsa::invoke(queries::contracts::contract_all_functions)]
    fn contract_all_functions(&self, id: ContractId) -> Rc<[FunctionId]>;
    #[salsa::invoke(queries::contracts::contract_function_map)]
    fn contract_function_map(&self, id: ContractId) -> Analysis<Rc<IndexMap<SmolStr, FunctionId>>>;
    #[salsa::invoke(queries::contracts::contract_public_function_map)]
    fn contract_public_function_map(&self, id: ContractId) -> Rc<IndexMap<SmolStr, FunctionId>>;
    #[salsa::invoke(queries::contracts::contract_init_function)]
    fn contract_init_function(&self, id: ContractId) -> Analysis<Option<FunctionId>>;
    #[salsa::invoke(queries::contracts::contract_call_function)]
    fn contract_call_function(&self, id: ContractId) -> Analysis<Option<FunctionId>>;

    #[salsa::invoke(queries::contracts::contract_all_fields)]
    fn contract_all_fields(&self, id: ContractId) -> Rc<[ContractFieldId]>;
    #[salsa::invoke(queries::contracts::contract_field_map)]
    fn contract_field_map(
        &self,
        id: ContractId,
    ) -> Analysis<Rc<IndexMap<SmolStr, ContractFieldId>>>;
    #[salsa::invoke(queries::contracts::contract_field_type)]
    fn contract_field_type(&self, field: ContractFieldId) -> Analysis<Result<TypeId, TypeError>>;
    #[salsa::cycle(queries::contracts::contract_dependency_graph_cycle)]
    #[salsa::invoke(queries::contracts::contract_dependency_graph)]
    fn contract_dependency_graph(&self, id: ContractId) -> DepGraphWrapper;
    #[salsa::cycle(queries::contracts::contract_runtime_dependency_graph_cycle)]
    #[salsa::invoke(queries::contracts::contract_runtime_dependency_graph)]
    fn contract_runtime_dependency_graph(&self, id: ContractId) -> DepGraphWrapper;

    // Function
    #[salsa::invoke(queries::functions::function_signature)]
    fn function_signature(&self, id: FunctionSigId) -> Analysis<Rc<types::FunctionSignature>>;
    #[salsa::invoke(queries::functions::function_body)]
    fn function_body(&self, id: FunctionId) -> Analysis<Rc<FunctionBody>>;
    #[salsa::cycle(queries::functions::function_dependency_graph_cycle)]
    #[salsa::invoke(queries::functions::function_dependency_graph)]
    fn function_dependency_graph(&self, id: FunctionId) -> DepGraphWrapper;

    // Struct
    #[salsa::invoke(queries::structs::struct_all_fields)]
    fn struct_all_fields(&self, id: StructId) -> Rc<[StructFieldId]>;
    #[salsa::invoke(queries::structs::struct_field_map)]
    fn struct_field_map(&self, id: StructId) -> Analysis<Rc<IndexMap<SmolStr, StructFieldId>>>;
    #[salsa::invoke(queries::structs::struct_field_type)]
    fn struct_field_type(&self, field: StructFieldId) -> Analysis<Result<TypeId, TypeError>>;
    #[salsa::invoke(queries::structs::struct_all_functions)]
    fn struct_all_functions(&self, id: StructId) -> Rc<[FunctionId]>;
    #[salsa::invoke(queries::structs::struct_function_map)]
    fn struct_function_map(&self, id: StructId) -> Analysis<Rc<IndexMap<SmolStr, FunctionId>>>;
    #[salsa::cycle(queries::structs::struct_cycle)]
    #[salsa::invoke(queries::structs::struct_dependency_graph)]
    fn struct_dependency_graph(&self, id: StructId) -> Analysis<DepGraphWrapper>;

    // Enum
    #[salsa::invoke(queries::enums::enum_all_variants)]
    fn enum_all_variants(&self, id: EnumId) -> Rc<[EnumVariantId]>;
    #[salsa::invoke(queries::enums::enum_variant_map)]
    fn enum_variant_map(&self, id: EnumId) -> Analysis<Rc<IndexMap<SmolStr, EnumVariantId>>>;
    #[salsa::invoke(queries::enums::enum_all_functions)]
    fn enum_all_functions(&self, id: EnumId) -> Rc<[FunctionId]>;
    #[salsa::invoke(queries::enums::enum_function_map)]
    fn enum_function_map(&self, id: EnumId) -> Analysis<Rc<IndexMap<SmolStr, FunctionId>>>;
    #[salsa::cycle(queries::enums::enum_cycle)]
    #[salsa::invoke(queries::enums::enum_dependency_graph)]
    fn enum_dependency_graph(&self, id: EnumId) -> Analysis<DepGraphWrapper>;
    #[salsa::invoke(queries::enums::enum_variant_kind)]
    fn enum_variant_kind(&self, id: EnumVariantId) -> Analysis<Result<EnumVariantKind, TypeError>>;

    // Trait
    #[salsa::invoke(queries::traits::trait_all_functions)]
    fn trait_all_functions(&self, id: TraitId) -> Rc<[FunctionSigId]>;
    #[salsa::invoke(queries::traits::trait_function_map)]
    fn trait_function_map(&self, id: TraitId) -> Analysis<Rc<IndexMap<SmolStr, FunctionSigId>>>;
    #[salsa::invoke(queries::traits::trait_is_implemented_for)]
    fn trait_is_implemented_for(&self, id: TraitId, typ: TypeId) -> bool;

    // Impl
    #[salsa::invoke(queries::impls::impl_all_functions)]
    fn impl_all_functions(&self, id: ImplId) -> Rc<[FunctionId]>;
    #[salsa::invoke(queries::impls::impl_function_map)]
    fn impl_function_map(&self, id: ImplId) -> Analysis<Rc<IndexMap<SmolStr, FunctionId>>>;

    // Type
    #[salsa::invoke(queries::types::all_impls)]
    fn all_impls(&self, ty: TypeId) -> Rc<[ImplId]>;
    #[salsa::invoke(queries::types::impl_for)]
    fn impl_for(&self, ty: TypeId, treit: TraitId) -> Option<ImplId>;
    #[salsa::invoke(queries::types::function_sigs)]
    fn function_sigs(&self, ty: TypeId, name: SmolStr) -> Rc<[FunctionSigId]>;

    // Type alias
    #[salsa::invoke(queries::types::type_alias_type)]
    #[salsa::cycle(queries::types::type_alias_type_cycle)]
    fn type_alias_type(&self, id: TypeAliasId) -> Analysis<Result<TypeId, TypeError>>;
}

#[salsa::database(AnalyzerDbStorage, SourceDbStorage)]
#[derive(Default)]
pub struct TestDb {
    storage: salsa::Storage<TestDb>,
}
impl salsa::Database for TestDb {}

impl Upcast<dyn SourceDb> for TestDb {
    fn upcast(&self) -> &(dyn SourceDb + 'static) {
        self
    }
}

impl UpcastMut<dyn SourceDb> for TestDb {
    fn upcast_mut(&mut self) -> &mut (dyn SourceDb + 'static) {
        &mut *self
    }
}
