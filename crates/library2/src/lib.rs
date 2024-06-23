use std::collections::BTreeSet;

pub use ::include_dir;
use common::{
    input::{IngotKind, Version},
    InputDb, InputFile, InputIngot,
};
use include_dir::{include_dir, Dir};

pub const STD: Dir = include_dir!("$CARGO_MANIFEST_DIR/std");

fn std_src_input_files(db: &mut dyn InputDb, ingot: InputIngot) -> BTreeSet<InputFile> {
    static_dir_files(&STD)
        .into_iter()
        .map(|(path, content)| InputFile::new(db, ingot, path.into(), content.into()))
        .collect()
}

pub fn std_lib_input_ingot(db: &mut dyn InputDb) -> InputIngot {
    let ingot = InputIngot::new(
        db,
        "std",
        IngotKind::Std,
        Version::new(0, 0, 0),
        BTreeSet::default(),
    );

    let input_files = std_src_input_files(db, ingot);
    ingot.set_files(db, input_files);
    ingot
}

// pub fn std_src_files() -> Vec<(&'static str, &'static str)> {
//     static_dir_files(STD.get_dir("src").unwrap())
// }

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
