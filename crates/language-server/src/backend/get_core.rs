use camino::Utf8PathBuf;
use common::{
    input::{IngotKind, Version},
    InputDb, InputFile, InputIngot,
};
use tracing::info;

#[derive(rust_embed::RustEmbed)]
#[folder = "../../library/core"]
pub struct CoreLib;

#[salsa::tracked]
pub fn get_core_ingot(db: &dyn InputDb) -> InputIngot {
    InputIngot::new(
        db,
        "core", // this should be handled by the driver crate default behavior
        IngotKind::Core,
        Version::new(0, 0, 1),
        Default::default(),
    )
}

pub fn init_core_ingot(db: &mut dyn InputDb) -> InputIngot {
    info!("Loading std lib...");

    // First collect all files and create the InputFiles
    let mut std_files = Vec::new();
    let mut root_file = None;

    for path in CoreLib::iter() {
        let path: Utf8PathBuf = path.as_ref().into();
        info!("Loading stdlib file: {}", path);

        if let Some(file) = CoreLib::get(path.as_str()) {
            if !path.starts_with("src") {
                continue;
            };
            if let Ok(contents) = String::from_utf8(file.data.as_ref().to_vec()) {
                // Create InputFile with paths relative to std root
                let input_file = InputFile::new(db, path.clone().into(), contents);

                // Identify the root file (probably src/lib.fe or similar)
                if path.as_str() == "src/lib.fe" {
                    root_file = Some(input_file);
                }

                std_files.push(input_file);
            }
        }
    }

    let core_ingot = get_core_ingot(db);
    // Set up the ingot structure
    if let Some(root) = root_file {
        core_ingot.set_root_file(db, root);
    }

    assert!(root_file.is_some(), "std library must have a root file");

    // Add all files to the ingot
    core_ingot.set_files(db, std_files.into_iter().collect());

    core_ingot
}

#[cfg(test)]
mod tests {
    use crate::backend::db::LanguageServerDatabase;

    use super::*;

    #[test]
    fn is_core_deduplicated() {
        let mut db = LanguageServerDatabase::default();
        let core_1 = get_core_ingot(&db);
        let core_2 = get_core_ingot(&db);

        let foo = InputFile::new(&db, "src/mod1/foo.fe".into(), "".into());

        core_2.set_root_file(&mut db, foo);

        assert!(core_1.eq(&core_2));
        assert!(core_1.root_file(&db).eq(&core_2.root_file(&db)));
    }
}
