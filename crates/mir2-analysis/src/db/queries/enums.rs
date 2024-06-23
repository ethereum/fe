use std::rc::Rc;

use crate::{db::MirDb, ir::FunctionId};

pub fn mir_lower_enum_all_functions(
    db: &dyn MirDb,
    enum_: analyzer_items::EnumId,
) -> Rc<Vec<FunctionId>> {
    enum_
        .all_functions(db.upcast())
        .iter()
        .map(|func| db.mir_lowered_func_signature(*func))
        .collect::<Vec<_>>()
        .into()
}
