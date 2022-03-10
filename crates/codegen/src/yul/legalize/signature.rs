use fe_mir::ir::FunctionSignature;

use crate::db::CodegenDb;

pub fn legalize_func_signature(db: &dyn CodegenDb, sig: &mut FunctionSignature) {
    // Remove param if the type is contract or zero-sized.
    let params = &mut sig.params;
    params
        .retain(|param| !param.ty.is_contract(db.upcast()) && !param.ty.is_zero_sized(db.upcast()));

    // Legalize param types.
    for param in params.iter_mut() {
        param.ty = db.codegen_legalized_type(param.ty);
    }

    if let Some(ret_ty) = sig.return_type {
        // Remove return type  if the type is contract or zero-sized.
        if ret_ty.is_contract(db.upcast()) || ret_ty.is_zero_sized(db.upcast()) {
            sig.return_type = None;
        } else {
            // Legalize param types.
            sig.return_type = Some(db.codegen_legalized_type(ret_ty));
        }
    }
}
