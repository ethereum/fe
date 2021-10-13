use fe_common::files::{FileLoader, FileStore};
use include_dir::{include_dir, Dir};
use std::path::Path;

const FIXTURES: Dir = include_dir!("fixtures");

pub fn fixture(path: &str) -> &'static str {
    FIXTURES
        .get_file(path)
        .unwrap_or_else(|| panic!("bad fixture file path {}", path))
        .contents_utf8()
        .expect("fixture file isn't utf8")
}

pub fn fixture_bytes(path: &str) -> &'static [u8] {
    FIXTURES
        .get_file(path)
        .unwrap_or_else(|| panic!("bad fixture file path {}", path))
        .contents()
}

struct FixtureLoader {}

impl FileLoader for FixtureLoader {
    fn load_file(&self, path: &Path) -> std::io::Result<String> {
        Ok(FIXTURES
            .get_file(path)
            .unwrap_or_else(|| panic!("bad fixture file path {:?}", path))
            .contents_utf8()
            .expect("fixture file isn't utf8")
            .to_string())
    }
}

pub fn build_filestore(path: &str) -> FileStore {
    let mut files = FileStore::with_loader(Box::new(FixtureLoader {}));

    for path in all_file_paths_in_fixture_dir(path) {
        files.load_file(&path).unwrap();
    }

    files
}

fn all_file_paths_in_fixture_dir(path: &str) -> Vec<String> {
    let dir = FIXTURES
        .get_dir(path)
        .unwrap_or_else(|| panic!("no fixture dir named \"{}\"", path));

    let mut files = vec![];

    for file in dir.files() {
        files.push(file.path().to_string_lossy().to_string())
    }

    for sub_dir in dir.dirs() {
        files.append(&mut all_file_paths_in_fixture_dir(
            &sub_dir.path().to_string_lossy().to_string(),
        ));
    }

    files
}
