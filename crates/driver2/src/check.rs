use crate::{load_ingot, CheckArgs};
use common::indexmap::IndexSet;
use common::input::IngotDependency;
use common::{input::IngotKind, InputIngot};
use fe_driver2::DriverDataBase;
use include_dir::{include_dir, Dir};
use semver::Version;
use std::fs;
use std::path::PathBuf;
use std::{collections::BTreeSet, path::Path};

const STD_DIR: Dir = include_dir!("$CARGO_MANIFEST_DIR/../../library/std");

fn write_std_files(std_path: &Path) {
    write_files_recursive(&STD_DIR, std_path);
}

fn write_files_recursive(dir: &Dir<'_>, base_path: &Path) {
    for file in dir.files() {
        let file_path = base_path.join(file.path());
        if let Some(parent_dir) = file_path.parent() {
            std::fs::create_dir_all(parent_dir).unwrap();
        }
        std::fs::write(file_path, file.contents()).unwrap();
    }

    for subdir in dir.dirs() {
        let subdir_path = base_path.join(subdir.path());
        std::fs::create_dir_all(&subdir_path).unwrap();
        write_files_recursive(subdir, &base_path);
    }
}

pub fn run_check(args: &CheckArgs) {
    let std_path = match &args.std_path {
        Some(path) => PathBuf::from(path.to_owned()),
        None => {
            let home_dir = dirs::home_dir().expect("Failed to get user home directory");
            home_dir.join(".fe/std")
        }
    };
    if !std_path.exists() {
        println!("The standard library is not installed. Do you want to perform the write? (y/n)");
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim().to_lowercase();
        if input == "y" || input == "yes" {
            write_std_files(&std_path);
        } else {
            eprintln!(
                "Cannot perform the write without the standard library being installed on disk"
            );
            std::process::exit(2);
        }
    }

    let path = Path::new(&args.path);
    if !path.exists() {
        eprintln!("Path '{}' does not exist", path.display());
        std::process::exit(2);
    }

    let mut db = DriverDataBase::default();

    let std_ingot = load_ingot(&std_path, &mut db, IngotKind::Std, &mut IndexSet::default());

    if path.is_file() {
        let source = fs::read_to_string(path).unwrap();
        check_single_file(path, source, std_ingot, &mut db, args.dump_scope_graph);
    } else if path.is_dir() {
        check_ingot(path, std_ingot, &mut db, args.dump_scope_graph);
    } else {
        eprintln!(
            "Path '{}' is neither a file nor a directory",
            path.display()
        );
        std::process::exit(2);
    }
}

pub fn check_single_file(
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
    let mut dependencies = IndexSet::default();
    let mut main_ingot = load_ingot(path, db, IngotKind::Local, &mut dependencies);

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
