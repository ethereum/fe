use camino::Utf8PathBuf;
use imbl::{HashSet, OrdMap};
use salsa::Setter;
use url::Url;

use crate::{InputDb, InputFile};

#[salsa::input(constructor=__new_impl)]
#[derive(Debug)]
pub struct IngotWorkspace {
    ingots: HashSet<Url>, // these could be found implicitly... but keep them explicit for now
    files: OrdMap<Url, InputFile>,
    paths: OrdMap<InputFile, Url>,
}

#[salsa::tracked]
impl<'a> IngotWorkspace {
    pub fn new(db: &'a dyn InputDb) -> Self {
        IngotWorkspace::__new_impl(db, HashSet::new(), OrdMap::new(), OrdMap::new())
    }

    #[salsa::tracked]
    pub fn absolute_file_path(self, db: &'a dyn InputDb, file: &'a InputFile) -> Option<Url> {
        self.paths(db).get(file).map(|path| path.clone())
    }

    #[salsa::tracked]
    pub fn find_ingot_root(self, db: &'a dyn InputDb, path: &'a Url) -> Option<Url> {
        for ingot_root in self.ingots(db).iter() {
            if path.as_str().starts_with(ingot_root.as_str()) {
                return Some(ingot_root.clone());
            }
        }

        // If we didn't find a direct match, try with the parent path
        if let Some(parent_path) = path.parent().map() {
            if &parent_path != path {
                return self.find_ingot_root(db, &parent_path);
            }
        }

        None
    }

    #[salsa::tracked]
    pub fn ingot_for_file(self, db: &'a dyn InputDb, file: &'a InputFile) -> Option<Url> {
        let file_path = self.absolute_file_path(db, file)?;
        self.find_ingot_root(db, &file_path)
    }

    #[salsa::tracked]
    pub fn ingot_file_path(self, db: &'a dyn InputDb, file: &'a InputFile) -> Option<Utf8PathBuf> {
        let file_path = self.absolute_file_path(db, file)?;
        self.find_ingot_root(db, &file_path)
            .map(|ingot| self.ingot_path(db, &ingot))
            .flatten()
    }

    /// Create a new file in this ingot or return an existing one
    pub fn touch_with_initial_content(
        self,
        db: &mut dyn InputDb,
        path: Utf8PathBuf,
        initial_content: Option<String>,
    ) -> InputFile {
        // Ensure path is absolute
        if !path.is_absolute() {
            panic!("Path must be absolute: {}", path);
        }

        // Check if the file already exists
        let files = self.files(db);
        if let Some(&file) = files.get(&path) {
            return file;
        }
        let initial = initial_content.unwrap_or_default();

        // Create a new file
        let file = InputFile::__new_impl(db, initial);

        // Update the files and paths maps
        let mut new_files = files.clone();
        let mut new_paths = self.paths(db).clone();

        new_files.insert(path.clone(), file);
        self.set_files(db).to(new_files);

        new_paths.insert(file, path.clone());
        self.set_paths(db).to(new_paths);

        file
    }

    // Create a new file in this workspace using an absolute path
    pub fn touch(&self, db: &mut dyn InputDb, path: Utf8PathBuf) -> InputFile {
        self.touch_with_initial_content(db, path, None)
    }

    /// Create a new file in this ingot using a relative path
    pub fn touch_in_ingot(
        self,
        db: &mut dyn InputDb,
        ingot: &InputIngot,
        rel_path: Utf8PathBuf,
        initial_content: Option<String>,
    ) -> InputFile {
        // Get the ingot's path
        let ingot_path = match self.ingot_path(db, ingot) {
            Some(path) => path,
            None => panic!("Cannot find path for ingot"),
        };

        // Combine ingot path with relative path
        let absolute_path = ingot_path.join(rel_path);

        // Use the existing touch method to create the file
        self.touch_with_initial_content(db, absolute_path, initial_content)
    }

    /// Remove a file from this ingot
    pub fn verify_file_path_consistency(&self, db: &dyn InputDb) {
        // Verify that both maps have the same number of entries
        debug_assert_eq!(
            self.files(db).len(),
            self.paths(db).len(),
            "Map size mismatch in file-path maps"
        );

        // Verify that every entry in files has a corresponding entry in paths
        for (_path, file) in self.files(db).iter() {
            debug_assert!(
                self.paths(db).contains_key(file),
                "Consistency error: file found in files map but not in paths map"
            );
        }

        // Verify that every entry in paths has a corresponding entry in files
        for (_file, path) in self.paths(db).iter() {
            debug_assert!(
                self.files(db).contains_key(path),
                "Consistency error: path found in paths map but not in files map"
            );
        }
    }

    pub fn remove_file(self, db: &mut dyn InputDb, path: &Utf8PathBuf) -> Option<InputFile> {
        let mut files = self.files(db).clone();
        let mut paths = self.paths(db).clone();

        // Get the file associated with this path
        let removed_file = files.remove(path);

        // Remove from paths map if file was found
        if let Some(file) = &removed_file {
            paths.remove(file);
        }

        // Update both maps
        self.set_files(db).to(files);
        self.set_paths(db).to(paths);

        // Verify consistency after modification
        #[cfg(debug_assertions)]
        self.verify_file_path_consistency(db);

        removed_file
    }
}

fn increment_last_char(path: &Utf8PathBuf) -> Utf8PathBuf {
    let mut s = path.to_string();
    if let Some(last) = s.pop() {
        s.push((last as u8 + 1) as char);
    }
    Utf8PathBuf::from(s)
}

trait UrlExt {
    fn parent(&self) -> Option<Url>;
}

impl UrlExt for Url {
    fn parent(&self) -> Option<Url> {
        let mut segments: Vec<_> = self.path_segments()?.collect();

        // Remove the last segment (if not empty already)
        if !segments.is_empty() {
            segments.pop();
        }

        // If we've removed everything, this is the root path, return None
        if segments.is_empty() && (self.path() == "/" || self.path().is_empty()) {
            return None;
        }

        // Try to create a base URL by removing the path
        let mut base = self.clone();
        base.set_path("");

        // Join the remaining segments
        let path = segments.join("/");
        base.set_path(&path);

        // Ensure the path ends with a slash
        if !base.path().ends_with('/') {
            base.set_path(&format!("{}/", base.path()));
        }

        Some(base)
    }
}
