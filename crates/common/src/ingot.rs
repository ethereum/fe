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
        Self {
            db,
            ingot_path: ingot_path.into(),
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

        // For standalone files, we need to derive a relative path from the original
        let relative_file_path = ensure_relative_path(&ingot_path, full_path)
            .expect("Could not create a relative path for standalone file");

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
    /// The path can be either relative to the ingot base or absolute
    /// (which will be converted to a relative path)
    pub fn file_from_contents(
        mut self,
        path: impl Into<Utf8PathBuf>,
        contents: impl Into<String>,
    ) -> Self {
        let input_path = path.into();
        let relative_path = ensure_relative_path(&self.ingot_path, input_path)
            .expect("Path must be relative or a child of the ingot base path");

        // Create a new InputFile with the relative path and contents
        let file = InputFile::new(self.db, relative_path, contents.into());
        self.files.insert(file);
        self
    }

    /// Add multiple files to the ingot from path-content pairs
    /// Paths can be either relative to the ingot base or absolute
    /// (which will be converted to relative paths)
    pub fn files_from_contents(
        mut self,
        files: impl IntoIterator<Item = (impl Into<Utf8PathBuf>, impl Into<String>)>,
    ) -> Self {
        for (path, contents) in files {
            let input_path = path.into();
            let relative_path = ensure_relative_path(&self.ingot_path, input_path)
                .expect("Path must be relative or a child of the ingot base path");

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
        let input_path = path.into();
        let relative_path = ensure_relative_path(&self.ingot_path, input_path)
            .expect("Entrypoint path must be relative or a child of the ingot base path");

        self.entrypoint_path = Some(relative_path);
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

/// Helper function to ensure a path is relative to a base path
/// Returns a relative path if:
/// - The path is already relative (returned as-is)
/// - The path is absolute and is a child of the base path (converted to relative)
///   Returns an error if the path is absolute but not a child of the base path
fn ensure_relative_path(
    base_path: &Utf8PathBuf,
    path: Utf8PathBuf,
) -> Result<Utf8PathBuf, &'static str> {
    if path.is_relative() {
        // Path is already relative, return as-is
        Ok(path)
    } else if path.starts_with(base_path) {
        // Path is absolute and under the base path, make it relative
        path.strip_prefix(base_path)
            .map(|p| p.to_path_buf())
            .map_err(|_| "Failed to strip prefix from path")
    } else {
        // Path is absolute but not under the base path
        Err("Absolute path must be a child of the ingot base path")
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
