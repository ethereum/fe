use core::panic;

use camino::Utf8PathBuf;
use rust_embed::Embed;
use salsa::Setter;
use smol_str::SmolStr;

use crate::{indexmap::IndexSet, InputDb};

#[derive(Embed)]
#[folder = "../../library/core"]
struct Core;

/// An ingot is a collection of files which are compiled together.
/// Ingot can depend on other ingots.
#[salsa::input]
pub struct InputIngot {
    /// An absolute path to the ingot root directory.
    /// The all files in the ingot should be located under this directory.
    #[return_ref]
    pub path: Utf8PathBuf,

    /// Specifies the kind of the ingot.
    pub kind: IngotKind,

    /// A version of the ingot.
    #[return_ref]
    pub version: Version,

    /// A list of ingots which the current ingot depends on.
    #[return_ref]
    pub external_ingots: IndexSet<IngotDependency>,

    /// A list of files which the current ingot contains.
    #[return_ref]
    #[set(__set_files_impl)]
    pub files: IndexSet<InputFile>,

    #[set(__set_root_file_impl)]
    #[get(__get_root_file_impl)]
    root_file: Option<InputFile>,
}
impl InputIngot {
    pub fn core(db: &dyn InputDb) -> InputIngot {
        let mut files = IndexSet::new();
        let mut root_file = None;
        let ingot_path = Utf8PathBuf::from("core");

        for file in Core::iter() {
            if file.ends_with(".fe") {
                let path = ingot_path.join(Utf8PathBuf::from(&file));
                if let Some(content) = Core::get(&file) {
                    let is_root = path == "core/src/lib.fe";
                    let input_file = InputFile::new(
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

        Self::new(
            db,
            ingot_path,
            IngotKind::Core,
            Version::new(0, 0, 0),
            IndexSet::default(),
            files,
            root_file,
        )
    }

    /// Set the root file of the ingot.
    /// The root file must be set before the ingot is used.
    pub fn set_root_file(self, db: &mut dyn InputDb, file: InputFile) {
        self.__set_root_file_impl(db).to(Some(file));
    }

    /// Set the list of files which the ingot contains.
    /// All files must bee set before the ingot is used.
    pub fn set_files(self, db: &mut dyn InputDb, files: IndexSet<InputFile>) {
        self.__set_files_impl(db).to(files);
    }

    /// Returns the root file of the ingot.
    /// Panics if the root file is not set.
    pub fn root_file(&self, db: &dyn InputDb) -> InputFile {
        self.__get_root_file_impl(db).unwrap()
    }
}

#[salsa::input]
pub struct InputFile {
    /// A path to the file from the ingot root directory.
    #[return_ref]
    pub path: Utf8PathBuf,

    #[return_ref]
    pub text: String,
}

impl InputFile {
    pub fn abs_path(&self, db: &dyn InputDb, ingot: InputIngot) -> Utf8PathBuf {
        ingot.path(db).join(self.path(db))
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

    /// Core library ingot.
    Core,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IngotDependency {
    /// The ingot may have a alias name from the original ingot name.
    pub name: SmolStr,
    /// An ingot which the current ingot depends on.
    pub ingot: InputIngot,
}
impl IngotDependency {
    pub fn new(name: &str, ingot: InputIngot) -> Self {
        Self {
            name: SmolStr::new(name),
            ingot,
        }
    }
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
