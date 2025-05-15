use camino::Utf8PathBuf;
use rust_embed::Embed;
use url::Url;

use crate::{
    ingot::{IngotBaseUrl, IngotDescription, IngotIndex},
    InputDb,
};

pub static BUILTIN_CORE_BASE_URL: &str = "builtin-core:///";

#[derive(Embed)]
#[folder = "../../library/core"]
pub struct Core;

pub trait HasBuiltinCore: InputDb {
    fn initialize_builtin_core(&self, db: &mut dyn InputDb);
    fn builtin_core<'db>(&self, db: &'db dyn InputDb) -> IngotDescription<'db>;
}

impl<T: InputDb> HasBuiltinCore for T {
    fn initialize_builtin_core(&self, db: &mut dyn InputDb) {
        let base = Url::parse(BUILTIN_CORE_BASE_URL).unwrap();

        for (path, contents) in Core::iter().filter_map(|path| {
            Core::get(&path).map(|content| {
                let contents = String::from_utf8(content.data.into_owned()).unwrap();
                // Store paths relative to the ingot root
                (Utf8PathBuf::from(path.to_string()), contents)
            })
        }) {
            base.touch(db, path, contents.into());
        }
    }

    fn builtin_core<'db>(&self, db: &'db dyn InputDb) -> IngotDescription<'db> {
        let core = self
            .file_index()
            .containing_ingot(db, Url::parse(BUILTIN_CORE_BASE_URL).as_ref().unwrap());
        core.expect("Built-in core ingot failed to initialize")
    }
}
