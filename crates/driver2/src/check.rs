use crate::CheckArgs;
use common::indexmap::IndexSet;
use common::input::IngotDependency;
use common::{input::IngotKind, InputIngot};
use fe_driver2::DriverDataBase;
use include_dir::Dir;
use semver::Version;
use std::fs;
use std::{collections::BTreeSet, path::Path};

pub fn check_standalone(
    path: &Path,
    source: String,
    std_ingot: InputIngot,
    db: &mut DriverDataBase,
    dump_scope_graph: bool,
) {
    let mut dependencies = IndexSet::default();
    dependencies.insert(IngotDependency::new("std", std_ingot));
    let ingot = InputIngot::new(
        db,
        path.parent().unwrap().to_str().unwrap(),
        IngotKind::StandAlone,
        Version::new(0, 1, 0),
        dependencies.clone(),
    );

    // let input_file = InputFile::new(
    //     db,
    //     ingot.clone(),
    //     path.file_name().unwrap().to_str().unwrap().into(),
    //     source,
    // );
    // ingot.set_files(db, BTreeSet::from([input_file.clone()]));
    // ingot.set_root_file(db, input_file);

    let top_mod = db.standalone(path, &source);
    // db.run_on_top_mod(top_mod);
    // db.emit_diags();

    // if dump_scope_graph {
    //     println!("{}", dump_scope_graph(db, top_mod));
    // }
}

pub fn check_ingot(
    path: &Path,
    std_ingot: InputIngot,
    db: &mut DriverDataBase,
    dump_scope_graph: bool,
) {
    // let mut dependencies = BTreeSet::from([IngotDependency::new("std", std_ingot)]);
    // let mut dependencies = IndexSet::default();
    // let mut main_ingot = load_ingot(path, db, IngotKind::Local, &mut dependencies);

    // main_ingot.set_external_ingots(db, dependencies);

    let diags = db.run_on_ingot(std_ingot);

    // let diags = db.format_diags();
    // if !diags.is_empty() {
    //     panic!("{diags}")
    // }

    // db.run_on_ingot(main_ingot);

    // let diags = db.format_diags();
    // if !diags.is_empty() {
    // panic!("{:?}", diags)
    // }

    // if dump_scope_graph {
    //     println!("{}", dump_scope_graph(db, top_mod));
    // }
}

// use include_dir::{include_dir, Dir};
// use std::path::PathBuf;
// use std::{collections::BTreeSet, path::Path};

// fn check_std_lib_exists(std_lib_files: &Dir, std_lib_path: &Path) -> bool {
//     true
// }

// fn write_std_lib(std_lib_files: &Dir, std_lib_path: &Path) {
//     write_files_recursive(std_lib_files, std_lib_path);
// }

// fn default_std_lib_path() -> PathBuf {
//     let home_dir = dirs::home_dir().expect("Failed to get user home directory");
//     home_dir.join(".fe/std")
// }

// fn write_files_recursive(dir: &Dir<'_>, base_path: &Path) {
//     for file in dir.files() {
//         let file_path = base_path.join(file.path());
//         if let Some(parent_dir) = file_path.parent() {
//             std::fs::create_dir_all(parent_dir).unwrap();
//         }
//         std::fs::write(file_path, file.contents()).unwrap();
//     }

//     for subdir in dir.dirs() {
//         let subdir_path = base_path.join(subdir.path());
//         std::fs::create_dir_all(&subdir_path).unwrap();
//         write_files_recursive(subdir, &base_path);
//     }
// }
