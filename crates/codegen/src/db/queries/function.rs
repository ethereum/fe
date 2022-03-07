use std::rc::Rc;

use fe_mir::ir::{FunctionBody, FunctionId, FunctionSignature};

use crate::{db::CodegenDb, yul::legalize};

pub fn legalized_signature(db: &dyn CodegenDb, function: FunctionId) -> Rc<FunctionSignature> {
    let mut sig = function.signature(db.upcast()).as_ref().clone();
    legalize::legalize_func_signature(db, &mut sig);
    sig.into()
}

pub fn legalized_body(db: &dyn CodegenDb, function: FunctionId) -> Rc<FunctionBody> {
    let mut body = function.body(db.upcast()).as_ref().clone();
    legalize::legalize_func_body(db, &mut body);
    body.into()
}
