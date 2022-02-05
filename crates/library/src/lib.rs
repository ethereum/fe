pub use ::include_dir;
use include_dir::{include_dir, Dir};

pub const STD: Dir = include_dir!("$CARGO_MANIFEST_DIR/std");

pub fn std_src_files() -> Vec<(&'static str, &'static str)> {
    static_dir_files(STD.get_dir("src").unwrap())
}

pub fn static_dir_files(dir: &'static Dir) -> Vec<(&'static str, &'static str)> {
    fn add_files(dir: &'static Dir, accum: &mut Vec<(&'static str, &'static str)>) {
        accum.extend(dir.files().map(|file| {
            (
                file.path().to_str().unwrap(),
                file.contents_utf8().expect("non-utf8 static file"),
            )
        }));

        for sub_dir in dir.dirs() {
            add_files(sub_dir, accum)
        }
    }

    let mut files = vec![];
    add_files(dir, &mut files);
    files
}
