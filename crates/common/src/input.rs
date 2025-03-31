use camino::Utf8PathBuf;
use salsa::Setter;
use smol_str::SmolStr;

use crate::{core_ingot, indexmap::IndexSet, InputDb};

/// An ingot is a collection of files which are compiled together.
/// Ingot can depend on other ingots.
#[salsa::input(constructor = __new_impl)]
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
    pub fn new(
        db: &dyn InputDb,
        path: &str,
        kind: IngotKind,
        version: Version,
        external_ingots: IndexSet<IngotDependency>,
    ) -> InputIngot {
        let path = Utf8PathBuf::from(path);
        let root_file = None;
        Self::__new_impl(
            db,
            path,
            kind,
            version,
            external_ingots,
            IndexSet::default(),
            root_file,
        )
    }

    pub fn core(db: &dyn InputDb) -> InputIngot {
        core_ingot::core(db)
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

#[salsa::interned]
pub struct FilePath<'db> {
    path: Utf8PathBuf,
}

impl<'db> FilePath<'db> {
    pub fn from(db: &'db dyn InputDb, path: impl Into<Utf8PathBuf>) -> Self {
        FilePath::new(db, path.into())
    }
}

#[salsa::input(constructor = __new_impl)]
pub struct InputFile {
    /// A path to the file from the ingot root directory.
    #[return_ref]
    #[set(__set_path_impl)]
    pub path: Utf8PathBuf,

    #[return_ref]
    pub text: String,
}

impl InputFile {
    pub fn abs_path(&self, db: &dyn InputDb, ingot: InputIngot) -> Utf8PathBuf {
        ingot.path(db).join(self.path(db))
    }

    pub fn new(db: &dyn InputDb, path: Utf8PathBuf, ingot: InputIngot) -> Self {
        input_for_file_path(db, FilePath::from(db, path), ingot)
    }
}

#[salsa::tracked]
pub fn input_for_file_path<'db>(
    db: &'db dyn InputDb,
    path: FilePath<'db>,
    ingot: InputIngot,
) -> InputFile {
    // Check if the ingot already has a file with the same path
    for file in ingot.files(db).iter() {
        if file.path(db).as_path() == path.path(db) {
            return *file;
        }
    }

    // If no existing file is found, create a new one
    InputFile::__new_impl(db, path.path(db), String::new())
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

#[cfg(test)]
mod tests {
    use crate::{impl_db_traits, input::*};

    #[derive(Default, Clone)]
    #[salsa::db]
    pub struct TestDatabase {
        storage: salsa::Storage<Self>,
    }

    impl_db_traits!(TestDatabase, InputDb);

    #[test]
    fn test_input_file_equals() {
        let path = Utf8PathBuf::from("test.foo");

        let mut db = TestDatabase::default();
        let ingot = InputIngot::core(&db);

        let file1 = InputFile::new(&db, path.clone(), ingot);
        let file2 = InputFile::new(&db, path, ingot);
        assert_eq!(file1, file2);

        file2.set_text(&mut db).to("Hello, world!".into());
        assert_eq!(file1.text(&db), file2.text(&db));
    }
}
