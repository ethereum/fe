use std::rc::Rc;

use fe_analyzer::namespace::items::{self as analyzer_items, FunctionId};

use crate::{db::MirDb, ir::FunctionSigId};

pub fn mir_lower_struct_all_functions(
    db: &dyn MirDb,
    struct_: analyzer_items::StructId,
) -> Rc<Vec<(FunctionSigId, FunctionId)>> {
    struct_
        .all_functions(db.upcast())
        .iter()
        .map(|func| (db.mir_lowered_func_signature(func.sig(db.upcast())), *func))
        .collect::<Vec<_>>()
        .into()
}
