use crate::namespace::items::{IngotId, IngotMode, ModuleId};
use crate::AnalyzerDb;

pub fn ingot_root_module(db: &dyn AnalyzerDb, ingot: IngotId) -> Option<ModuleId> {
    let filename = match ingot.data(db).mode {
        IngotMode::Lib => "lib.fe",
        IngotMode::Main => "main.fe",
        IngotMode::StandaloneModule => return Some(ingot.all_modules(db)[0]),
    };

    ingot
        .all_modules(db)
        .iter()
        .find(|modid| modid.file_path_relative_to_src_dir(db) == filename)
        .copied()
}
