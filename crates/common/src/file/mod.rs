pub mod workspace;

use camino::Utf8PathBuf;
use url::Url;
pub use workspace::Workspace;

use crate::{ingot::Ingot, InputDb};

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
    pub fn containing_ingot(self, db: &dyn InputDb) -> Option<Ingot<'_>> {
        let index = db.workspace();
        self.url(db)
            .and_then(|url| index.containing_ingot(db, &url))
    }

    #[salsa::tracked(return_ref)]
    pub fn path(self, db: &dyn InputDb) -> Option<Utf8PathBuf> {
        let index = db.workspace();
        self.containing_ingot(db)
            .and_then(|ingot| index.get_relative_path(db, ingot.base(db), self))
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
        let index = db.workspace();
        index.get_path(db, self)
    }
}
