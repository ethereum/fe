use std::rc::Rc;

use fe_analyzer::namespace::items::{self as analyzer_items, FunctionId};

use crate::{db::MirDb, ir::FunctionSigId};

pub fn mir_lower_enum_all_functions(
    db: &dyn MirDb,
    enum_: analyzer_items::EnumId,
) -> Rc<Vec<(FunctionSigId, FunctionId)>> {
    enum_
        .all_functions(db.upcast())
        .iter()
        .map(|func| {
            let sig = func.sig(db.upcast());
            (db.mir_lowered_func_signature(sig), *func)
        })
        .collect::<Vec<_>>()
        .into()
}
