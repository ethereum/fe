use std::rc::Rc;

use fe_common::db::{Upcast, UpcastMut};
use fe_mir::{
    db::MirDb,
    ir::{FunctionBody, FunctionId, FunctionSignature, TypeId},
};
use fe_new_abi::{function::AbiFunction, types::AbiType};

mod queries;

#[salsa::query_group(CodegenDbStorage)]
pub trait CodegenDb: MirDb + Upcast<dyn MirDb> + UpcastMut<dyn MirDb> {
    #[salsa::invoke(queries::function::legalized_signature)]
    fn codegen_legalized_signature(&self, function_id: FunctionId) -> Rc<FunctionSignature>;
    #[salsa::invoke(queries::function::legalized_body)]
    fn codegen_legalized_body(&self, function_id: FunctionId) -> Rc<FunctionBody>;

    #[salsa::invoke(queries::abi::abi_type)]
    fn codegen_abi_type(&self, ty: TypeId) -> AbiType;
    #[salsa::invoke(queries::abi::abi_function)]
    fn codegen_abi_function(&self, function_id: FunctionId) -> Rc<AbiFunction>;
}
