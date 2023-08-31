use std::path::Path;

use clap::Args;
use fe_common::{
    diagnostics::{print_diagnostics, Diagnostic},
    utils::files::BuildFiles,
};
use fe_driver::Db;

#[derive(Args)]
#[clap(about = "Analyze the current project and report errors, but don't build artifacts")]
pub struct CheckArgs {
    input_path: String,
}

fn check_single_file(db: &mut Db, input_path: &str) -> Vec<Diagnostic> {
    let content = match std::fs::read_to_string(input_path) {
        Err(err) => {
            eprintln!("Failed to load file: `{}`. Error: {}", &input_path, err);
            std::process::exit(1)
        }
        Ok(content) => content,
    };

    fe_driver::check_single_file(db, input_path, &content)
}

fn check_ingot(db: &mut Db, input_path: &str) -> Vec<Diagnostic> {
    let build_files = match BuildFiles::load_fs(input_path) {
        Ok(files) => files,
        Err(err) => {
            eprintln!("Failed to load project files.\nError: {err}");
            std::process::exit(1)
        }
    };

    fe_driver::check_ingot(db, &build_files)
}

pub fn check(args: CheckArgs) {
    let mut db = fe_driver::Db::default();
    let input_path = args.input_path;

    // check project
    let diags = if Path::new(&input_path).is_file() {
        check_single_file(&mut db, &input_path)
    } else {
        check_ingot(&mut db, &input_path)
    };

    if !diags.is_empty() {
        print_diagnostics(&db, &diags);
        std::process::exit(1);
    }

    eprintln!("Finished");
}
