use std::collections::BTreeSet;

use camino::Utf8PathBuf;
use smol_str::SmolStr;

use crate::InputDb;

/// An ingot is a collection of files which are compiled together.
/// Ingot can depend on other ingots.
#[salsa::input]
pub struct InputIngot {
    /// An absolute path to the ingot root directory.
    #[return_ref]
    pub path: Utf8PathBuf,

    /// Specifies the kind of the ingot.
    pub kind: IngotKind,

    /// A version of the ingot.
    #[return_ref]
    pub version: Version,

    pub root_file: InputFile,

    /// A list of ingots which the current ingot depends on.
    #[return_ref]
    pub dependency: BTreeSet<IngotDependency>,

    /// A list of files which the current ingot contains.
    #[return_ref]
    pub files: BTreeSet<InputFile>,
}

#[salsa::input]
pub struct InputFile {
    /// A ingot id which the file belongs to.
    pub ingot: InputIngot,

    /// A path to the file from the ingot root directory.
    #[return_ref]
    pub path: Utf8PathBuf,

    #[return_ref]
    pub text: String,
}

impl InputFile {
    pub fn abs_path(&self, db: &dyn InputDb) -> Utf8PathBuf {
        self.ingot(db).path(db).join(self.path(db))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IngotKind {
    /// A standalone ingot is a dummy ingot when the compiler is invoked
    /// directly on a file.
    StandAlone,

    /// A local ingot which is the current ingot being compiled.
    Local,

    /// An external ingot which is depended on by the current ingot.
    External,

    /// Standard library ingot.
    Std,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IngotDependency {
    /// The ingot may have a alias name from the original ingot name.
    pub name: SmolStr,
    /// An ingot which the current ingot depends on.
    pub ingot: InputIngot,
}

impl PartialOrd for IngotDependency {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.ingot.cmp(&other.ingot))
    }
}
impl Ord for IngotDependency {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.ingot.cmp(&other.ingot)
    }
}

pub type Version = semver::Version;
