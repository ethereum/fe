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

pub fn symbol_name(db: &dyn CodegenDb, function: FunctionId) -> Rc<String> {
    let module = function.signature(db.upcast()).module_id;
    let module_name = module.name(db.upcast());
    let func_name = function.name_with_class(db.upcast()).replace("::", "$");

    format!("{}${}", module_name, func_name).into()
}
