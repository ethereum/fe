pub mod index;

use camino::Utf8PathBuf;
pub use index::FileIndex;
use url::Url;

use crate::{
    ingot::{IngotDescription, IngotIndex},
    InputDb,
};

#[salsa::input(constructor = __new_impl)]
#[derive(Debug)]
pub struct File {
    #[return_ref]
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IngotFileKind {
    /// A source file containing Fe code.
    Source,

    /// A configuration file for the ingot.
    Config,
}

#[salsa::tracked]
impl File {
    #[salsa::tracked]
    pub fn containing_ingot(self, db: &dyn InputDb) -> Option<IngotDescription<'_>> {
        let index = db.file_index();
        self.url(db)
            .map(|url| index.containing_ingot(db, &url))
            .flatten()
    }

    #[salsa::tracked(return_ref)]
    pub fn path(self, db: &dyn InputDb) -> Option<Utf8PathBuf> {
        let index = db.file_index();
        self.containing_ingot(db)
            .map(|ingot| index.get_relative_path(db, ingot.base(db), self))
            .flatten()
    }

    #[salsa::tracked]
    pub fn kind(self, db: &dyn InputDb) -> Option<IngotFileKind> {
        self.path(db).as_ref().and_then(|path| {
            if path.as_str().ends_with(".fe") {
                Some(IngotFileKind::Source)
            } else if path.as_str().ends_with("fe.toml") {
                Some(IngotFileKind::Config)
            } else {
                None
            }
        })
    }

    pub fn url(self, db: &dyn InputDb) -> Option<Url> {
        let index = db.file_index();
        index.get_path(db, self)
    }
}
