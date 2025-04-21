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
    #[set(__set_files_map_impl)]
    pub files_map: IndexMap<Utf8PathBuf, InputFile>,
}

#[salsa::tracked]
impl InputIngot {
    // This can be more sophisticated potentially
    #[salsa::tracked]
    pub fn file_kind(self, db: &dyn InputDb, file: InputFile) -> Option<IngotFileKind> {
        let path = file.path(db);
        if path.as_str() == "fe.toml" {
            Some(IngotFileKind::Config)
        } else if path.extension() == Some("fe") {
            Some(IngotFileKind::Source)
        } else {
            None
        }
    }

    #[salsa::tracked]
    pub fn config_file(self, db: &dyn InputDb) -> Option<InputFile> {
        let config_path = &Utf8PathBuf::from("fe.toml");
        self.get_file(db, config_path)
    }

    #[salsa::tracked]
    pub fn root_file(self, db: &dyn InputDb) -> InputFile {
        match self.kind(db) {
            IngotKind::StandAlone => {
                // For standalone ingots, get the single file
                let files_map = self.files_map(db);
                assert!(
                    files_map.len() == 1,
                    "Standalone ingot must have exactly one file"
                );
                *files_map.values().next().unwrap()
            }
            _ => {
                // For other ingots, use src/lib.fe or src/main.fe
                let lib_path = Utf8PathBuf::from("src/lib.fe");
                let main_path = Utf8PathBuf::from("src/main.fe");
                let files = self.files_map(db);

                // Try lib.fe first, then main.fe
                if let Some(file) = self.get_file(db, &lib_path) {
                    file
                } else if let Some(file) = self.get_file(db, &main_path) {
                    file
                } else {
                    let available_files = files
                        .keys()
                        .map(|p| p.as_str())
                        .collect::<Vec<_>>()
                        .join(", ");
                    panic!(
                        "Missing src/lib.fe or src/main.fe file in ingot. Available files: {}",
                        available_files
                    );
                }
            }
        }
    }

    /// Get a file from this ingot by its path
    pub fn get_file(self, db: &dyn InputDb, path: &Utf8PathBuf) -> Option<InputFile> {
        self.files_map(db).get(path).cloned()
    }

    /// Create a new file in this ingot or return an existing one
    pub fn touch(self, db: &mut dyn InputDb, mut path: Utf8PathBuf) -> InputFile {
        // Normalize the path - make it relative to the ingot path if it's absolute
        if path.is_absolute() {
            let ingot_path = self.path(db);

            // Verify the path is within the ingot directory
            if !path.as_str().starts_with(ingot_path.as_str()) {
                panic!(
                    "Cannot touch a file outside of the ingot directory. File: {}, Ingot: {}",
                    path, ingot_path
                );
            }

            // Convert absolute path to relative path
            path = path
                .strip_prefix(ingot_path)
                .expect("Failed to strip prefix after validation")
                .to_path_buf();
        }

        // Check if the file already exists in the ingot
        let files_map = self.files_map(db);
        if let Some(&file) = files_map.get(&path) {
            return file;
        }

        // Create a new file
        let file = InputFile::__new_impl(db, path.clone(), String::new());

        let mut files = files_map.clone();
        files.insert(path, file);
        self.__set_files_map_impl(db).to(files);

        file
    }

    /// Remove a file from this ingot
    pub fn remove_file(self, db: &mut dyn InputDb, path: &Utf8PathBuf) -> Option<InputFile> {
        let mut files = self.files_map(db).clone();
        let removed_file = files.remove(path);
        self.__set_files_map_impl(db).to(files);

        removed_file
    }

    /// Create an ingot from a collection of files
    pub fn from_files<I>(
        db: &dyn InputDb,
        path: Utf8PathBuf,
        kind: IngotKind,
        version: Version,
        external_ingots: IndexSet<IngotDependency>,
        files: I,
    ) -> Self
    where
        I: IntoIterator<Item = InputFile>,
    {
        let mut files_index = IndexMap::new();
        for file in files {
            files_index.insert(file.path(db).clone(), file);
        }
        Self::new(db, path, kind, version, external_ingots, files_index)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IngotFileKind {
    /// A source file containing Fe code.
    Source,

    /// A configuration file for the ingot.
    Config,
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
