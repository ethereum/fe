use url::Url;

use crate::{file::IndexedFile, urlext::UrlExt, InputDb};

use super::{index::FileIndex, File};

// #[salsa::input(constructor=__new_impl)]
// #[derive(Debug)]
pub trait Workspace {
    fn touch(&self, db: &mut dyn InputDb, url: Url) -> IndexedFile;
}

#[salsa::tracked]
impl<'a> Workspace for FileIndex {
    // #[salsa::tracked]
    // pub fn ingot_for_file(self, db: &'a dyn InputDb, file: &'a InputFile) -> Option<Url> {
    //     let file_path = self.absolute_file_path(db, file)?;
    //     self.find_ingot_root(db, &file_path)
    // }

    // #[salsa::tracked]
    // pub fn ingot_file_path(self, db: &'a dyn InputDb, file: &'a InputFile) -> Option<Utf8PathBuf> {
    //     let file_path = self.absolute_file_path(db, file)?;
    //     self.find_ingot_root(db, &file_path)
    //         .map(|ingot| self.ingot_path(db, &ingot))
    //         .flatten()
    // }

    // Create a new file in this workspace using an absolute path
    fn touch(&self, db: &mut dyn InputDb, url: Url) -> IndexedFile {
        self.touch_with_initial_content(db, url, None)
    }
}

#[salsa::tracked]
impl FileIndex {
    /// Create a new file in this ingot or return an existing one
    pub fn touch_with_initial_content(
        &self,
        db: &mut dyn InputDb,
        url: Url,
        initial_content: Option<String>,
    ) -> IndexedFile {
        // Check if the file already exists
        if let Some(file) = self.get(db, &url) {
            return file;
        }
        let initial = initial_content.unwrap_or_default();

        let input_file = File::__new_impl(db, initial);
        self.set(db, url, input_file)
            .expect("Failed to create file")
    }

    #[salsa::tracked]
    pub fn locate_related_config_url(self, db: &dyn InputDb, file: Url) -> Option<Url> {
        let dir = file.directory()?;
        let config = dir.join("fe.toml").ok()?;

        if self.get(db, &config).is_some() {
            return Some(config);
        } else if let Some(parent) = dir.parent() {
            self.locate_related_config_url(db, parent)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default, Clone)]
    #[salsa::db]
    pub struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    #[salsa::db]
    impl salsa::Database for TestDatabase {
        fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
    }
    #[test]
    fn test_locate_config() {
        let mut db = TestDatabase::default();
        let index = FileIndex::empty(&db);

        // Create our test files - a library file, a config file, and a standalone file
        let url_lib = Url::parse("file:///foo/src/lib.fe").unwrap();
        let lib = File::__new_impl(&db, "lib".to_string());

        let url_config = Url::parse("file:///foo/fe.toml").unwrap();
        let config = File::__new_impl(&db, "config".to_string());

        let url_standalone = Url::parse("file:///bar/standalone.fe").unwrap();
        let standalone = File::__new_impl(&db, "standalone".to_string());

        // Add the files to the index
        index
            .set(&mut db, url_lib.clone(), lib)
            .expect("Failed to set lib file");
        index
            .set(&mut db, url_config.clone(), config)
            .expect("Failed to set config file");
        index
            .set(&mut db, url_standalone.clone(), standalone)
            .expect("Failed to set standalone file");

        // Test recursive search: lib.fe is in /foo/src/ but config is in /foo/
        // This tests that we correctly search up the directory tree
        let found_config = index.locate_related_config_url(&db, url_lib);
        assert!(found_config.is_some());
        assert_eq!(found_config.unwrap(), url_config);

        // Test that standalone file without a config returns None
        let no_config = index.locate_related_config_url(&db, url_standalone);
        assert!(no_config.is_none());
    }
}
