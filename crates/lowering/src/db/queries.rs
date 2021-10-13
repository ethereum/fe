use crate::db::LoweringDb;
use crate::mappers;
use fe_analyzer::namespace::items::{IngotId, ModuleFileContent, ModuleId};
use std::rc::Rc;

pub fn lowered_module(db: &dyn LoweringDb, module_id: ModuleId) -> ModuleId {
    let db = db.upcast();
    let mut module = (*module_id.data(db)).clone();
    module.ast = mappers::module::module(db, module_id);
    db.intern_module(Rc::new(module))
}

pub fn lowered_ingot(lowering_db: &dyn LoweringDb, ingot_id: IngotId) -> IngotId {
    let db = lowering_db.upcast();

    let mut ingot = (*ingot_id.data(db)).clone();
    ingot.fe_files = ingot_id
        .all_modules(db)
        .iter()
        .filter_map(|module_id| match module_id.file_content(db) {
            // dir modules do not yet have ASTs to lower
            ModuleFileContent::Dir { .. } => None,
            ModuleFileContent::File { file: file_id } => Some((
                file_id,
                (
                    ingot_id.data(db).fe_files[&file_id].0.clone(),
                    lowered_module(lowering_db, *module_id).ast(db),
                ),
            )),
        })
        .collect();
    db.intern_ingot(Rc::new(ingot))
}
