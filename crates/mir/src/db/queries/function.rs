use fe_analyzer::namespace::items as analyzer_items;

use crate::{db::MirDb, ir, lower::function::lower_func_signature};

pub fn mir_lowered_func_signature(
    db: &dyn MirDb,
    analyzer_func: analyzer_items::FunctionId,
) -> ir::FunctionId {
    lower_func_signature(db, analyzer_func)
}
