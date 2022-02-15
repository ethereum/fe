use std::rc::Rc;

use fe_analyzer::namespace::{items as analyzer_items, types as analyzer_types};
use smol_str::SmolStr;

use crate::{
    db::MirDb,
    ir::{function::Linkage, FunctionId, FunctionParam, FunctionSignature},
};

pub fn lower_func_signature(db: &dyn MirDb, func_id: analyzer_items::FunctionId) -> FunctionId {
    // TODO: Remove this when an analyzer's function signature contains `self` type.
    let mut params = vec![];
    let has_self = if let Some(self_ty) = func_id.self_typ(db.upcast()) {
        params.push(make_param(db, "self", self_ty));
        true
    } else {
        false
    };

    let analyzer_signature = func_id.signature(db.upcast());
    params.extend(
        analyzer_signature
            .params
            .iter()
            .map(|param| make_param(db, param.name.clone(), param.typ.clone().unwrap())),
    );
    let return_type = db.mir_lowered_type(analyzer_signature.return_type.clone().unwrap().into());

    let linkage = if func_id.is_public(db.upcast()) {
        if has_self {
            Linkage::Export
        } else {
            Linkage::Public
        }
    } else {
        Linkage::Private
    };

    let sig = FunctionSignature {
        params,
        return_type,
        module_id: func_id.module(db.upcast()),
        analyzer_func_id: func_id,
        linkage,
        has_self,
    };

    db.mir_intern_function(Rc::new(sig))
}

fn make_param(
    db: &dyn MirDb,
    name: impl Into<SmolStr>,
    ty: impl Into<analyzer_types::Type>,
) -> FunctionParam {
    FunctionParam {
        name: name.into(),
        ty: db.mir_lowered_type(ty.into()),
    }
}
