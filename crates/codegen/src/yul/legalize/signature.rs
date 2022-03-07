use fe_mir::ir::FunctionSignature;

use crate::db::CodegenDb;

pub fn legalize_func_signature(_db: &dyn CodegenDb, _sig: &mut FunctionSignature) {
    // TODO: Remove zero sized types from arguments, also remove return type if
    // it's zero-sized
}
