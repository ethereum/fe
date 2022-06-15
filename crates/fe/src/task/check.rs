use std::path::Path;

use clap::Args;
use fe_common::diagnostics::{print_diagnostics, Diagnostic};
use fe_driver::Db;

use super::utils::load_files_from_dir;

const DEFAULT_INGOT_NAME: &str = "main";

#[derive(Args)]
#[clap(about = "Analyze the current project and report errors, but don't build artifacts")]
pub struct CheckArgs {
    input_path: String,
}

fn check_single_file(db: &mut Db, input_path: &str) -> Vec<Diagnostic> {
    let content = match std::fs::read_to_string(&input_path) {
        Err(err) => {
            eprintln!("Failed to load file: `{}`. Error: {}", &input_path, err);
            std::process::exit(1)
        }
        Ok(content) => content,
    };

    fe_driver::check_single_file(db, input_path, &content)
}

fn check_ingot(db: &mut Db, input_path: &str) -> Vec<Diagnostic> {
    if !Path::new(input_path).exists() {
        eprintln!("Input directory does not exist: `{}`.", input_path);
        std::process::exit(1)
    }

    let files = match load_files_from_dir(input_path) {
        Ok(files) if files.is_empty() => {
            eprintln!("Input directory is not an ingot: `{}`", input_path);
            std::process::exit(1)
        }
        Ok(files) => files,
        Err(err) => {
            eprintln!("Failed to load project files. Error: {}", err);
            std::process::exit(1)
        }
    };

    fe_driver::check_ingot(db, DEFAULT_INGOT_NAME, &files)
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
