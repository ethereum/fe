use bimap::BiMap;
use camino::{Utf8Path, Utf8PathBuf};
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

    // question: maybe this ought to be handled by the dependency graph thing?
    /// A list of ingots which the current ingot depends on.
    #[return_ref]
    pub external_ingots: IndexSet<IngotDependency>,
    pub files: IngotFiles,
}

#[salsa::input]
#[derive(Debug)]
pub struct IngotFiles {
    /// The collection of files belonging to an ingot.
    #[return_ref]
    pub map: BiMap<Utf8PathBuf, FileContents>,
}

impl IngotFiles {
    pub fn default(db: &dyn InputDb) -> Self {
        Self::new(db, BiMap::default())
    }
    pub fn from_contents<U: Into<Utf8PathBuf>, V: Into<String>>(
        db: &dyn InputDb,
        contents: Vec<(U, V)>,
    ) -> Self {
        let mut map = BiMap::new();
        for (path, contents) in contents {
            let file_contents = FileContents::new(db, contents.into());
            map.insert(path.into(), file_contents);
        }
        Self::new(db, map)
    }
}

#[salsa::tracked]
pub fn ingot_root_file(db: &dyn InputDb, ingot: InputIngot) -> InputFile {
    let files = ingot.files(db);
    let map = files.map(db);
    let root_path = Utf8PathBuf::from("src/lib.rs");
    match ingot.kind(db) {
        IngotKind::StandAlone => {
            let map_len = map.len();
            if map_len != 1 {
                panic!(
                    "Standalone ingot {:?} should contain exactly one file, but found {}. Files: {:?}",
                    ingot.path(db),
                    map_len,
                    map
                );
            }
            // The single file in the map is the root file.
            let (_, file_contents) = map.iter().next().unwrap();
            InputFile::new(db, ingot, file_contents.clone())
        },
        _ => {
        map.get_by_left(&root_path)
            .map(|file| InputFile::new(db, ingot, file.clone()))
            .unwrap_or_else(|| {
                panic!(
                    "Could not find root file '{}' in ingot {:?}. Files actually in this ingot: {:?}",
                    root_path,
                    ingot.path(db),
                    ingot.files(db).map(db)
                )
            })
        }

    }
}

impl InputIngot {
    pub fn input_files<'db>(
        &'db self,
        db: &'db dyn InputDb,
    ) -> impl Iterator<Item = InputFile> + 'db {
        let files = self.files(db);
        let map = files.map(db);
        map.iter()
            .map(|(_, file)| InputFile::new(db, self.clone(), file.clone()))
    }

    pub fn input_file(&self, db: &dyn InputDb, path: Utf8PathBuf) -> Option<InputFile> {
        let contents = self.files(db).map(db).get_by_left(&path);
        contents.map(|c| InputFile::new(db, self.clone(), c.clone()))
    }

    // /// Set the root file of the ingot.
    // /// The root file must be set before the ingot is used.
    // pub fn set_root_file(self, db: &mut dyn InputDb, file: InputFile) {
    //     self.__set_root_file_impl(db).to(Some(file));
    // }

    // /// Set the list of files which the ingot contains.
    // /// All files must bee set before the ingot is used.
    // pub fn set_files(self, db: &mut dyn InputDb, files: IndexSet<InputFile>) {
    //     self.__set_files_impl(db).to(files);
    // }

    // /// Returns the root file of the ingot.
    // /// Panics if the root file is not set.
    // pub fn root_file(&self, db: &dyn InputDb) -> InputFile {
    //     self.__get_root_file_impl(db).unwrap()
    // }
}

// #[salsa::interned]
// #[derive(Debug)]
// pub struct RelativeFileId {
//     path: Utf8PathBuf,
// }

#[salsa::input]
#[derive(Debug)]
pub struct FileContents {
    #[return_ref]
    pub text: String,
}

#[salsa::input]
#[derive(Debug)]
pub struct InputFile {
    pub ingot: InputIngot,
    pub contents: FileContents,
}

impl InputFile {
    /// Looks up the relative path of this file within its ingot.
    pub fn path<'db>(&self, db: &'db dyn InputDb) -> &'db Utf8Path {
        let ingot = self.ingot(db);
        let contents = self.contents(db);
        let files_map = ingot.files(db).map(db);
        files_map.get_by_right(&contents).unwrap_or_else(|| {
            // This should be unreachable if File instances are constructed correctly,
            // ensuring that the FileContents genuinely belongs to the InputIngot.
            panic!(
                "Internal error: Could not find file contents in ingot {:?}",
                ingot.path(db)
            )
        })
    }
    pub fn abs_path(&self, db: &dyn InputDb) -> Utf8PathBuf {
        self.path(db).join(self.path(db))
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
