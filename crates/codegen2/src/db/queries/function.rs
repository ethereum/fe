use std::rc::Rc;

use fe_analyzer::{
    display::Displayable,
    namespace::{
        items::Item,
        types::{Type, TypeId},
    },
};
use fe_mir::ir::{FunctionBody, FunctionId, FunctionSignature};
use salsa::InternKey;
use smol_str::SmolStr;

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
        type_suffix(function, db)
    );

    let func_name = match analyzer_func.sig(db.upcast()).self_item(db.upcast()) {
        Some(Item::Impl(id)) => {
            let class_name = format!(
                "{}${}",
                id.trait_id(db.upcast()).name(db.upcast()),
                safe_name(db, id.receiver(db.upcast()))
            );
            format!("{class_name}${func_name}")
        }
        Some(class) => {
            let class_name = class.name(db.upcast());
            format!("{class_name}${func_name}")
        }
        _ => func_name,
    };

    format!("{module_name}${func_name}").into()
}

fn type_suffix(function: FunctionId, db: &dyn CodegenDb) -> SmolStr {
    function
        .signature(db.upcast())
        .resolved_generics
        .values()
        .fold(String::new(), |acc, param| {
            format!("{}_{}", acc, safe_name(db, *param))
        })
        .into()
}

fn safe_name(db: &dyn CodegenDb, ty: TypeId) -> SmolStr {
    match ty.typ(db.upcast()) {
        // TODO: Would be nice to get more human friendly names here
        Type::Array(_) => format!("array_{:?}", ty.as_intern_id()).into(),
        Type::Tuple(_) => format!("tuple_{:?}", ty.as_intern_id()).into(),
        _ => format!("{}", ty.display(db.upcast())).into(),
    }
}
