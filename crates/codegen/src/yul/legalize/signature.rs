use fe_mir::ir::{FunctionSignature, TypeKind};

use crate::db::CodegenDb;

pub fn legalize_func_signature(db: &dyn CodegenDb, sig: &mut FunctionSignature) {
    // Remove param if the type is contract or zero-sized.
    let params = &mut sig.params;
    params.retain(|param| match param.ty.data(db.upcast()).kind {
        TypeKind::Contract(_) => false,
        _ => !param.ty.deref(db.upcast()).is_zero_sized(db.upcast()),
    });

    // Legalize param types.
    for param in params.iter_mut() {
        param.ty = db.codegen_legalized_type(param.ty);
    }

    if let Some(ret_ty) = sig.return_type {
        // Remove return type  if the type is contract or zero-sized.
        if ret_ty.is_contract(db.upcast()) || ret_ty.deref(db.upcast()).is_zero_sized(db.upcast()) {
            sig.return_type = None;
        } else {
            // Legalize param types.
            sig.return_type = Some(db.codegen_legalized_type(ret_ty));
        }
    }
}
