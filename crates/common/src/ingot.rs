use camino::Utf8PathBuf;

use crate::{
    indexmap::IndexSet,
    input::{IngotDependency, IngotKind, Version},
    Core, InputDb, InputFile, InputIngot,
};

// Grant suggested that this `IngotBuilder` could eventually
// implement the `ResolutionHandler` trait directly (once it's implemented)
/// A builder for creating InputIngot instances
pub struct IngotBuilder<'a> {
    db: &'a dyn InputDb,
    ingot_path: Utf8PathBuf, // Absolute path to the base of the ingot
    kind: IngotKind,
    version: Version,
    dependencies: IndexSet<IngotDependency>,
    files: IndexSet<InputFile>,
    entrypoint_path: Option<Utf8PathBuf>, // Path to main file, relative to ingot_path
    entrypoint_file: Option<InputFile>,
    core_ingot: Option<InputIngot>,
}

impl<'a> IngotBuilder<'a> {
    /// Create a new InputIngot builder
    /// `ingot_path` should be the absolute path to the base of the ingot
    pub fn new(db: &'a dyn InputDb, ingot_path: impl Into<Utf8PathBuf>) -> Self {
        let path = ingot_path.into();
        // Use the path directly without attempting to modify it
        Self {
            db,
            ingot_path: path,
            kind: IngotKind::Local,
            version: Version::new(0, 0, 0),
            dependencies: IndexSet::default(),
            files: IndexSet::default(),
            entrypoint_path: None,
            entrypoint_file: None,
            core_ingot: None,
        }
    }

    /// Create a standalone ingot with a single file and path
    pub fn standalone(
        db: &'a dyn InputDb,
        file_path: impl Into<Utf8PathBuf>,
        contents: impl Into<String>,
    ) -> Self {
        let full_path = file_path.into();
        // For standalone ingots, the parent path of the file must exist
        let ingot_path = full_path
            .parent()
            .map(|p| p.to_path_buf())
            .expect("Standalone ingot file must have a parent directory");

        // For standalone files, derive a relative path from the original
        let relative_file_path = if full_path.is_absolute() && full_path.starts_with(&ingot_path) {
            // If path is under ingot_path, make it relative
            match full_path.strip_prefix(&ingot_path) {
                Ok(rel_path) => rel_path.to_path_buf(),
                Err(_) => Utf8PathBuf::from(full_path.file_name().unwrap_or("file.fe")),
            }
        } else if full_path.is_absolute() {
            // Just use the filename if it's absolute but not under ingot_path
            Utf8PathBuf::from(full_path.file_name().unwrap_or("file.fe"))
        } else {
            // Already relative, use as-is
            full_path
        };

        Self::new(db, ingot_path)
            .kind(IngotKind::StandAlone)
            .file_from_contents(relative_file_path, contents)
    }

    /// Create a library ingot
    pub fn external(db: &'a dyn InputDb, ingot_path: impl Into<Utf8PathBuf>) -> Self {
        Self::new(db, ingot_path).kind(IngotKind::External)
    }

    /// Create a local ingot (default)
    pub fn local(db: &'a dyn InputDb, ingot_path: impl Into<Utf8PathBuf>) -> Self {
        Self::new(db, ingot_path).kind(IngotKind::Local)
    }

    /// Set the ingot kind
    pub fn kind(mut self, kind: IngotKind) -> Self {
        self.kind = kind;
        self
    }

    /// Set the ingot version
    pub fn version(mut self, version: Version) -> Self {
        self.version = version;
        self
    }

    /// Add a dependency to the ingot
    pub fn dependency(mut self, name: &str, ingot: InputIngot) -> Self {
        self.dependencies.insert(IngotDependency::new(name, ingot));
        self
    }

    /// Add multiple dependencies to the ingot
    pub fn dependencies(mut self, deps: IndexSet<IngotDependency>) -> Self {
        self.dependencies = deps;
        self
    }

    /// Add a file to the ingot
    pub fn file(mut self, file: InputFile) -> Self {
        self.files.insert(file);
        self
    }

    /// Add multiple files to the ingot
    pub fn files(mut self, files: IndexSet<InputFile>) -> Self {
        self.files = files;
        self
    }

    /// Add a file to the ingot from a path and contents
    /// The path should be relative to the ingot base
    pub fn file_from_contents(
        mut self,
        path: impl Into<Utf8PathBuf>,
        contents: impl Into<String>,
    ) -> Self {
        let relative_path = path.into();
        // Create a new InputFile with the given path and contents
        let file = InputFile::new(self.db, relative_path, contents.into());
        self.files.insert(file);
        self
    }

    /// Add multiple files to the ingot from path-content pairs
    /// Paths should be relative to the ingot base
    pub fn files_from_contents(
        mut self,
        files: impl IntoIterator<Item = (impl Into<Utf8PathBuf>, impl Into<String>)>,
    ) -> Self {
        for (path, contents) in files {
            let relative_path = path.into();
            // Create a new InputFile for each path-content pair
            let file = InputFile::new(self.db, relative_path, contents.into());
            self.files.insert(file);
        }
        self
    }

    /// Set the root file of the ingot directly
    pub fn root_file(mut self, root_file: InputFile) -> Self {
        self.entrypoint_file = Some(root_file);
        self
    }

    /// Set the entrypoint file path of the ingot
    /// This will only take effect during build if no root_file is explicitly set
    /// The path should be relative to the ingot base
    pub fn entrypoint(mut self, path: impl Into<Utf8PathBuf>) -> Self {
        self.entrypoint_path = Some(path.into());
        self
    }

    /// Set the core ingot of the ingot
    pub fn with_core_ingot(mut self, core_ingot: InputIngot) -> Self {
        self.core_ingot = Some(core_ingot);
        self
    }

    /// Build the InputIngot
    pub fn build(self) -> (InputIngot, InputFile) {
        // For standalone ingots, ensure there's only one file and set it as the root file
        let (root_file, files) = match self.kind {
            IngotKind::StandAlone => {
                if self.files.len() != 1 {
                    panic!(
                        "A standalone ingot must have exactly one file, but found {}",
                        self.files.len()
                    );
                }
                // Use the single file as the root file
                let single_file = self.files.iter().next().cloned().unwrap();
                (Some(single_file), self.files)
            }
            _ => {
                // For other non-standalone ingots, use the specified root file or find it by path
                let root_file = self.entrypoint_file.or_else(|| {
                    self.entrypoint_path
                        .as_ref()
                        .and_then(|path| find_root_file(self.db, &self.files, path.as_str()))
                });
                (root_file, self.files)
            }
        };

        let mut dependencies = self.dependencies;
        // Add core ingot as a dependency if provided
        if let Some(core) = self.core_ingot {
            dependencies.insert(IngotDependency::new("core", core));
        }

        (
            InputIngot::new(
                self.db,
                self.ingot_path,
                self.kind,
                self.version,
                dependencies,
                files,
                root_file,
            ),
            root_file.expect("Root file not found"),
        )
    }
}

/// Find the root file in a collection of input files matching the given path.
/// Returns None if files is empty or no matching file is found.
fn find_root_file(
    db: &dyn InputDb,
    files: &IndexSet<InputFile>,
    entrypoint_path: &str,
) -> Option<InputFile> {
    if files.is_empty() {
        return None;
    }

    files
        .iter()
        .find(|input_file| {
            let input_path = input_file.path(db);
            // Simple string comparison is more reliable across platforms
            input_path == entrypoint_path
        })
        .cloned()
}

pub fn builtin_core(db: &dyn InputDb) -> InputIngot {
    // Use a virtual path that doesn't depend on the filesystem
    let ingot_path = Utf8PathBuf::from("/virtual/core");

    let builder = IngotBuilder::new(db, ingot_path)
        .kind(IngotKind::Core)
        .version(Version::new(0, 0, 0))
        .entrypoint("src/lib.fe");

    let files = Core::iter()
        .filter(|file| file.ends_with(".fe"))
        .filter_map(|file| {
            Core::get(&file).map(|content| {
                let contents = String::from_utf8(content.data.into_owned()).unwrap();
                // Store paths relative to the ingot root
                (file.as_ref().to_string(), contents)
            })
        });

    builder.files_from_contents(files).build().0
}
