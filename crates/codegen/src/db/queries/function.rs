use std::rc::Rc;

use fe_analyzer::{
    display::Displayable,
    namespace::{
        items::{FunctionId, Item},
        types::{Type, TypeId},
    },
};
use fe_mir::ir::{FunctionBody, FunctionSigId, FunctionSignature};
use salsa::InternKey;
use smol_str::SmolStr;

use crate::{db::CodegenDb, yul::legalize};

pub fn legalized_signature(db: &dyn CodegenDb, function: FunctionSigId) -> Rc<FunctionSignature> {
    let mut sig = function.data(db.upcast()).as_ref().clone();
    legalize::legalize_func_signature(db, &mut sig);
    sig.into()
}

pub fn legalized_body(db: &dyn CodegenDb, func: FunctionId) -> Rc<FunctionBody> {
    let mut body = (*db.mir_lowered_func_body(func)).clone();
    legalize::legalize_func_body(db, &mut body);
    body.into()
}

pub fn symbol_name(db: &dyn CodegenDb, function: FunctionSigId) -> Rc<String> {
    let module = function.data(db.upcast()).module_id;
    let module_name = module.name(db.upcast());

    let analyzer_func = function.analyzer_sig(db.upcast());
    let func_name = format!("{}", analyzer_func.name(db.upcast()),);

    let func_name = match analyzer_func.self_item(db.upcast()) {
        Some(Item::Impl(id)) => {
            let class_name = format!(
                "{}${}",
                id.trait_id(db.upcast()).name(db.upcast()),
                safe_name(db, id.receiver(db.upcast()))
            );
            format!("{}${}", class_name, func_name)
        }
        Some(class) => {
            let class_name = class.name(db.upcast());
            format!("{}${}", class_name, func_name)
        }
        _ => func_name,
    };

    format!("{}${}", module_name, func_name).into()
}

fn safe_name(db: &dyn CodegenDb, ty: TypeId) -> SmolStr {
    match ty.typ(db.upcast()) {
        // TODO: Would be nice to get more human friendly names here
        Type::Array(_) => format!("array_{:?}", ty.as_intern_id()).into(),
        Type::Tuple(_) => format!("tuple_{:?}", ty.as_intern_id()).into(),
        _ => format!("{}", ty.display(db.upcast())).into(),
    }
}
