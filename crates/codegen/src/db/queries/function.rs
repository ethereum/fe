use std::rc::Rc;

use fe_analyzer::{display::Displayable, namespace::items::Item};
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

    let analyzer_func = function.analyzer_func(db.upcast());
    let func_name = format!(
        "{}{}",
        analyzer_func.name(db.upcast()),
        function.type_suffix(db.upcast())
    );

    let func_name = match analyzer_func.sig(db.upcast()).self_item(db.upcast()) {
        Some(Item::Impl(id)) => {
            let class_name = format!(
                "{}${}",
                id.trait_id(db.upcast()).name(db.upcast()),
                id.receiver(db.upcast()).display(db.upcast())
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
