use clap::Args;
use std::{fs, path::Path};
use include_dir::{include_dir, Dir};

const TEMPLATE: Dir = include_dir!("$CARGO_MANIFEST_DIR/src/template/src");

#[derive(Args)]
pub struct InitArgs {
    name: String,
}

fn create_project(p: &Path) {
    for file in TEMPLATE.entries() {
        let f = file.as_file().unwrap();
        fs::write(p.join(f.path()),f.contents()).unwrap();
    }
}

pub fn init(args: InitArgs) {
    if Path::new(&args.name).exists() {
        eprintln!("Project  {} does exist. Please choice another name!", args.name);
        std::process::exit(1)
    }
    
    let p = Path::new(&args.name).join("src");

    match fs::create_dir_all(p.as_path()) {
        Ok(_) => create_project(p.as_path()),
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
    println!("Created new fe project!!!");
}
