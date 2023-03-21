use camino::Utf8PathBuf;
use smol_str::SmolStr;

/// An ingot is a collection of files which are compiled together.
/// Ingot can depend on other ingots.
#[salsa::input]
pub struct Ingot {
    /// A path to the ingot root directory.
    #[return_ref]
    pub path: Utf8PathBuf,

    /// Specifies the kind of the ingot.
    pub kind: IngotKind,

    /// A version of the ingot.
    #[return_ref]
    pub version: Version,

    /// A list of ingots which the current ingot depends on.
    #[return_ref]
    pub dependency: Vec<IngotDependency>,

    /// A list of files which the current ingot contains.
    #[return_ref]
    pub files: Vec<File>,
}

#[salsa::input(constructor = __new_priv)]
pub struct File {
    /// A ingot id which the file belongs to.
    pub ingot: Ingot,

    /// A path to the file from the ingot root directory.
    #[return_ref]
    pub path: Utf8PathBuf,

    #[return_ref]
    pub text: String,
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

    /// A std ingot.
    Std,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IngotDependency {
    /// The ingot may have a alias name from the original ingot name.
    pub name: SmolStr,
    /// An ingot which the current ingot depends on.
    pub ingot: Ingot,
}

pub type Version = semver::Version;
