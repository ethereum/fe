use clap::Args;
use include_dir::{include_dir, Dir};
use std::{fs, path::Path};

const TEMPLATE_DIR: Dir = include_dir!("$CARGO_MANIFEST_DIR/src/template/src");

#[derive(Args)]
#[clap(about = "Create new fe project")]
pub struct NewProjectArgs {
    name: String,
}

fn create_project(p: &Path) {
    for file in TEMPLATE_DIR.entries() {
        let f = file.as_file().unwrap();
        fs::write(p.join(f.path()), f.contents()).unwrap();
    }
}

pub fn create_new_project(args: NewProjectArgs) {
    let project_path = Path::new(&args.name);

    if project_path.exists() {
        eprintln!(
            "Error: destination directory {} already exists",
            project_path.canonicalize().unwrap().display(),
        );
        std::process::exit(1)
    }

    let source_path = project_path.join("src");

    match fs::create_dir_all(source_path.as_path()) {
        Ok(_) => create_project(source_path.as_path()),
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
    eprintln!("Created {}", project_path.display());
}
