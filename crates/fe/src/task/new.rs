use clap::Args;
use include_dir::{include_dir, Dir};
use std::{fs, path::Path};

const SRC_TEMPLATE_DIR: Dir = include_dir!("$CARGO_MANIFEST_DIR/src/template/src");

#[derive(Args)]
#[clap(about = "Create new fe project")]
pub struct NewProjectArgs {
    name: String,
}

fn create_project(name: &str, path: &Path) {
    for src_file in SRC_TEMPLATE_DIR.entries() {
        let file = src_file.as_file().unwrap();
        fs::write(path.join("src").join(file.path()), file.contents()).unwrap();
    }

    let manifest_content = format!(
        "name = \"{name}\"
version = \"1.0\"

[dependencies]
# my_lib = \"../my_lib\"
# my_lib = {{ path = \"../my_lib\", version = \"1.0\" }}"
    );
    fs::write(path.join("fe.toml"), manifest_content).unwrap();
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

    match fs::create_dir_all(project_path.join("src")) {
        Ok(_) => create_project(&args.name, project_path),
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
    eprintln!("Created {}", project_path.display());
}
