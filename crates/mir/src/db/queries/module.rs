use fe_analyzer::namespace::items as analyzer_items;

use crate::{db::MirDb, ir::ModuleId};

pub fn lowered_module(_db: &dyn MirDb, _analyzer_module: analyzer_items::ModuleId) -> ModuleId {
    todo!()
}
