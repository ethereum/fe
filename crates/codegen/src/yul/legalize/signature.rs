use fe_mir::{db::MirDb, ir::FunctionSignature};

pub fn legalize_func_signature(_db: &dyn MirDb, _sig: &mut FunctionSignature) {
    // TODO: Remove zero sized types from arguments, also remove return type if
    // it's zero-sized
}
