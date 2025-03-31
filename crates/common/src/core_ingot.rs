use camino::Utf8PathBuf;
use rust_embed::Embed;

use crate::{
    indexmap::IndexSet,
    input::{IngotKind, Version},
    InputDb, InputFile, InputIngot,
};
#[derive(Embed)]
#[folder = "../../library/core"]
struct Core;

#[salsa::tracked]
pub fn core(db: &dyn InputDb) -> InputIngot {
    let mut files = IndexSet::new();
    let mut root_file = None;
    let ingot_path = Utf8PathBuf::from("core");

    for file in Core::iter() {
        if file.ends_with(".fe") {
            let path = ingot_path.join(Utf8PathBuf::from(&file));
            if let Some(content) = Core::get(&file) {
                let is_root = path == "core/src/lib.fe";
                let input_file = InputFile::__new_impl(
                    db,
                    path,
                    String::from_utf8(content.data.into_owned()).unwrap(),
                );
                if is_root {
                    root_file = Some(input_file);
                }
                files.insert(input_file);
            }
        }
    }

    if root_file.is_none() {
        panic!("root file missing from core")
    }

    InputIngot::__new_impl(
        db,
        ingot_path,
        IngotKind::Core,
        Version::new(0, 0, 0),
        IndexSet::default(),
        files,
        root_file,
    )
}

#[cfg(test)]
mod tests {
    use crate::impl_db_traits;

    use super::*;

    #[derive(Clone, Default)]
    #[salsa::db]
    pub(crate) struct TestDb {
        storage: salsa::Storage<Self>,
    }
    impl_db_traits!(TestDb, InputDb);

    #[test]
    fn is_core_deduplicated() {
        // this is a sanity check
        let mut db = TestDb::default();
        let core_1 = core(&db);
        let core_2 = core(&db);

        let foo = InputFile::new(&db, "src/mod1/foo.fe".into(), core_1);

        core_2.set_root_file(&mut db, foo);

        assert!(core_1.eq(&core_2));
        assert!(core_1.root_file(&db).eq(&core_2.root_file(&db)));
    }
}
