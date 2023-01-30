use include_dir::{include_dir, Dir};

const FIXTURES: Dir = include_dir!("$CARGO_MANIFEST_DIR/fixtures");

pub fn fixture(path: &str) -> &'static str {
    FIXTURES
        .get_file(path)
        .unwrap_or_else(|| panic!("bad fixture file path {path}"))
        .contents_utf8()
        .expect("fixture file isn't utf8")
}

pub fn fixture_bytes(path: &str) -> &'static [u8] {
    FIXTURES
        .get_file(path)
        .unwrap_or_else(|| panic!("bad fixture file path {path}"))
        .contents()
}

pub fn fixture_dir(path: &str) -> &Dir<'static> {
    FIXTURES
        .get_dir(path)
        .unwrap_or_else(|| panic!("no fixture dir named \"{path}\""))
}

/// Returns `(file_path, file_content)`
pub fn fixture_dir_files(path: &str) -> Vec<(&'static str, &'static str)> {
    let dir = FIXTURES
        .get_dir(path)
        .unwrap_or_else(|| panic!("no fixture dir named \"{path}\""));

    fe_library::static_dir_files(dir)
}
