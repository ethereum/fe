use camino::Utf8PathBuf;
use rust_embed::Embed;
use salsa::Setter;
use smol_str::SmolStr;

use crate::{
    indexmap::{IndexMap, IndexSet},
    InputDb,
};

#[derive(Embed)]
#[folder = "../../library/core"]
struct Core;

/// An ingot is a collection of files which are compiled together.
/// Ingot can depend on other ingots.
#[salsa::input]
#[derive(Debug)]
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
    pub files: IngotFiles,

    #[set(__set_root_file_impl)]
    #[get(__get_root_file_impl)]
    root_file: Option<InputFile>,
}

impl InputIngot {
    /// Set the root file of the ingot.
    /// The root file must be set before the ingot is used.
    pub fn set_root_file(self, db: &mut dyn InputDb, file: InputFile) {
        self.__set_root_file_impl(db).to(Some(file));
    }

    /// Returns the root file of the ingot.
    /// Panics if the root file is not set.
    pub fn root_file(&self, db: &dyn InputDb) -> InputFile {
        self.__get_root_file_impl(db).unwrap()
    }

    pub fn files_map<'db>(&self, db: &'db dyn InputDb) -> &'db IndexMap<Utf8PathBuf, InputFile> {
        self.files(db).map(db)
    }
}

#[salsa::input]
#[derive(Debug)]
pub struct IngotFiles {
    #[return_ref]
    pub map: IndexMap<Utf8PathBuf, InputFile>,
}
#[salsa::tracked]
impl IngotFiles {
    pub fn default(db: &dyn InputDb) -> Self {
        Self::new(db, IndexMap::default())
    }

    pub fn from_files<I>(db: &dyn InputDb, files: I) -> Self
    where
        I: IntoIterator<Item = InputFile>,
    {
        let mut files_index = IndexMap::new();
        for file in files {
            files_index.insert(file.path(db).clone(), file.clone());
        }
        Self::new(db, files_index)
    }

    pub fn get_file(self, db: &dyn InputDb, path: &Utf8PathBuf) -> Option<InputFile> {
        self.map(db).get(path).cloned()
    }

    pub fn touch(self, db: &mut dyn InputDb, path: Utf8PathBuf) -> InputFile {
        // Check if the file already exists in the ingot
        let files_index = self.map(db);
        if let Some(&file) = files_index.get(&path) {
            return file;
        }

        // Create a new file
        let file = InputFile::__new_impl(db, path.clone(), String::new());

        let mut files = self.map(db).clone();
        files.insert(path, file);
        self.set_map(db).to(files);

        file
    }

    pub fn remove_file(self, db: &mut dyn InputDb, path: &Utf8PathBuf) -> Option<InputFile> {
        let mut files = self.map(db).clone();
        let removed_file = files.remove(path);
        self.set_map(db).to(files);

        removed_file
    }
}

#[salsa::input(constructor = __new_impl)]
#[derive(Debug)]
pub struct InputFile {
    /// A path to the file from the ingot root directory.
    #[return_ref]
    #[salsa::set(constructor = _____do_not_set)]
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
