use clap::Args;
use fe_common::diagnostics::print_diagnostics;
#[derive(Args)]
pub struct CheckArg {
    input_path: String,
}

pub fn check(check_arg: CheckArg) {
    let mut db = fe_driver::Db::default();
    let input_path = check_arg.input_path;

    let content = match std::fs::read_to_string(&input_path) {
        Err(err) => {
            eprintln!("Failed to load file: `{}`. Error: {}", &input_path, err);
            std::process::exit(1)
        }
        Ok(content) => content,
    };

    let diags = fe_driver::check_single_file(&mut db, &input_path, &content);

    if !diags.is_empty() {
        print_diagnostics(&mut db, &diags);
        std::process::exit(1);
    }
    println!("No error!!!");
}
