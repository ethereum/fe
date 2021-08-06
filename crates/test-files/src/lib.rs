use include_dir::{include_dir, Dir};

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
