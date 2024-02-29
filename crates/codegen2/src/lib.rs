use mir::MirDb;

pub mod yul;

// mod abi;
// mod constant;
// mod contract;
mod function;
// mod types;

#[salsa::jar(db = CodegenDb)]
pub struct Jar(
    function::legalized_signature,
    function::legalized_body,
    function::symbol_name,
    // types::legalized_type,
    // abi::abi_type,
    // abi::abi_function,
    // abi::abi_event,
    // abi::abi_contract,
    // abi::abi_type_maximum_size,
    // abi::abi_type_minimum_size,
    // abi::abi_function_argument_maximum_size,
    // abi::abi_function_return_maximum_size,
    // contract::symbol_name,
    // contract::deployer_symbol_name,
    // constant::string_symbol_name,
);

pub trait CodegenDb: salsa::DbWithJar<Jar> + MirDb {
    fn as_hir_db(&self) -> &dyn CodegenDb {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db::<'_>(self)
    }
}
impl<DB> CodegenDb for DB where DB: salsa::DbWithJar<Jar> + MirDb {}

// #![allow(clippy::arc_with_non_send_sync)]
// use std::rc::Rc;

// use fe_abi::{contract::AbiContract, event::AbiEvent, function::AbiFunction, types::AbiType};
// use fe_analyzer::{db::AnalyzerDbStorage, namespace::items::ContractId, AnalyzerDb};
// use fe_common::db::{SourceDb, SourceDbStorage, Upcast, UpcastMut};
// use fe_mir::{
//     db::{MirDb, MirDbStorage},
//     ir::{FunctionBody, FunctionId, FunctionSignature, TypeId},
// };

// mod queries;

// #[salsa::query_group(CodegenDbStorage)]
// pub trait CodegenDb: MirDb + Upcast<dyn MirDb> + UpcastMut<dyn MirDb> {
// }

// // TODO: Move this to driver.
// #[salsa::database(SourceDbStorage, AnalyzerDbStorage, MirDbStorage, CodegenDbStorage)]
// #[derive(Default)]
// pub struct Db {
//     storage: salsa::Storage<Db>,
// }
// impl salsa::Database for Db {}

// impl Upcast<dyn MirDb> for Db {
//     fn upcast(&self) -> &(dyn MirDb + 'static) {
//         self
//     }
// }

// impl UpcastMut<dyn MirDb> for Db {
//     fn upcast_mut(&mut self) -> &mut (dyn MirDb + 'static) {
//         &mut *self
//     }
// }

// impl Upcast<dyn SourceDb> for Db {
//     fn upcast(&self) -> &(dyn SourceDb + 'static) {
//         self
//     }
// }

// impl UpcastMut<dyn SourceDb> for Db {
//     fn upcast_mut(&mut self) -> &mut (dyn SourceDb + 'static) {
//         &mut *self
//     }
// }

// impl Upcast<dyn AnalyzerDb> for Db {
//     fn upcast(&self) -> &(dyn AnalyzerDb + 'static) {
//         self
//     }
// }

// impl UpcastMut<dyn AnalyzerDb> for Db {
//     fn upcast_mut(&mut self) -> &mut (dyn AnalyzerDb + 'static) {
//         &mut *self
//     }
// }
