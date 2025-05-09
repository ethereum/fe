use camino::Utf8PathBuf;
use radix_immutable::{StringPrefixView, StringTrie, Trie};
use salsa::Setter;
use url::Url;

use crate::{
    file::{File, IndexedFile},
    indexmap::IndexMap,
    InputDb,
};

#[derive(Debug)]
pub enum InputIndexError {
    CannotReuseInput,
}

#[salsa::input]
#[derive(Debug)]
pub(crate) struct FileIndex {
    files: StringTrie<Url, File>,
    paths: IndexMap<File, Url>,
}

#[salsa::tracked]
impl FileIndex {
    fn indexed_file(&self, file: File) -> IndexedFile {
        IndexedFile {
            index: self.clone(),
            file,
        }
    }

    pub fn empty(db: &dyn InputDb) -> Self {
        FileIndex::new(db, Trie::new(), IndexMap::new())
    }

    pub fn set(
        &self,
        db: &mut dyn InputDb,
        url: Url,
        file: File,
    ) -> Result<IndexedFile, InputIndexError> {
        // Check if the file is already associated with another URL
        let paths = self.paths(db);
        if let Some(existing_url) = paths.get(&file) {
            if existing_url != &url {
                return Err(InputIndexError::CannotReuseInput);
            }
        }

        let files = self.files(db);
        self.set_files(db).to(files.insert(url.clone(), file));
        let mut paths = self.paths(db);
        paths.insert(file, url);
        self.set_paths(db).to(paths);
        Ok(self.indexed_file(file))
    }

    pub fn get(&self, db: &dyn InputDb, url: &Url) -> Option<IndexedFile> {
        self.files(db)
            .get(&url)
            .cloned()
            .map(|file| self.indexed_file(file))
    }

    pub fn remove(&self, db: &mut dyn InputDb, url: &Url) -> Option<File> {
        if let Some(_file) = self.files(db).get(url) {
            let files = self.files(db);
            if let (files, Some(file)) = files.remove(&url) {
                self.set_files(db).to(files);
                let mut paths = self.paths(db);
                paths.remove(&file);
                Some(file)
            } else {
                None
            }
        } else {
            None
        }
    }

    #[salsa::tracked]
    pub fn items_at_base(self, db: &dyn InputDb, base: Url) -> StringPrefixView<'_, Url, File> {
        self.files(db).view_subtrie(base)
    }

    pub fn get_path(&self, db: &dyn InputDb, file: File) -> Option<Url> {
        self.paths(db).get(&file).cloned()
    }

    #[salsa::tracked]
    pub fn get_relative_path(self, db: &dyn InputDb, base: Url, file: File) -> Option<Utf8PathBuf> {
        // Get the file path
        let file_url = match self.paths(db).get(&file) {
            Some(url) => url.clone(),
            None => return None,
        };

        // Get the relative path between the base URL and file URL
        if let Some(relative) = base.make_relative(&file_url) {
            // Convert the relative URL string to a PathBuf
            Some(Utf8PathBuf::from(relative))
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
    fn test_input_index_basic() {
        let mut db = TestDatabase::default();
        let index = FileIndex::empty(&db);

        // Create a file and add it to the index
        let file = File::__new_impl(&db, "test content".to_string());
        let url = Url::parse("file:///test.fe").unwrap();

        index
            .set(&mut db, url.clone(), file)
            .expect("Failed to set file");

        // Test we can look up the file
        let retrieved_file = index.get(&db, &url);
        assert!(retrieved_file.is_some());
        assert_eq!(retrieved_file.unwrap(), IndexedFile { index, file });

        // Test removal
        let removed_file = index.remove(&mut db, &url);
        assert!(removed_file.is_some());
        assert_eq!(removed_file.unwrap(), file);

        // Verify it's gone
        let retrieved_file = index.get(&db, &url);
        assert!(retrieved_file.is_none());
    }

    #[test]
    fn test_input_index_path() {
        let mut db = TestDatabase::default();
        let index = FileIndex::empty(&db);

        // Create a file and add it to the index
        let file = File::__new_impl(&db, "test content".to_string());
        let url = Url::parse("file:///test.fe").unwrap();

        index
            .set(&mut db, url.clone(), file)
            .expect("Failed to set file");

        // Test we can look up the path
        let path = index.get_path(&db, file);
        assert!(path.is_some());
        assert_eq!(path.unwrap(), url);

        // Test we can look up a cloned file's path
        let cloned_file = file.clone();
        let path = index.get_path(&db, cloned_file);
        assert!(path.is_some());
        assert_eq!(path.unwrap(), url);
    }
}
