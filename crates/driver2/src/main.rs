use clap::{Args, Parser, Subcommand};
use common::input::{IngotDependency, InputFile};
use common::{input::IngotKind, InputDb, InputIngot};
use semver::Version;
use serde::Deserialize;
use std::{collections::BTreeSet, path::Path};
use walkdir::WalkDir;
mod check;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Check(CheckArgs),
}

#[derive(Args, Debug)]
struct CheckArgs {
    /// The path to check, either a single file or a directory.
    #[arg()]
    path: String,

    /// The std lib path.
    #[arg(short, long)]
    std_path: Option<String>,

    /// Dump a graphviz dot file of the scope graph for the given file.
    #[arg(long = "dump-scope-graph", default_value_t = false)]
    dump_scope_graph: bool,
}

#[derive(Deserialize, Debug)]
struct Manifest {
    package: Package,
    dependencies: Option<BTreeSet<String>>,
}

#[derive(Deserialize, Debug)]
struct Package {
    name: String,
    version: String,
}

pub fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Check(args) => check::run_check(args),
    }
}

// fn dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
//     let mut s = vec![];
//     top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
//     String::from_utf8(s).unwrap()
// }

fn load_ingot(
    path: &Path,
    db: &mut dyn InputDb,
    ingot_kind: IngotKind,
    dependencies: &mut BTreeSet<IngotDependency>,
) -> InputIngot {
    let manifest_path = path.join("fe.toml");
    let manifest_content =
        std::fs::read_to_string(&manifest_path).expect("Unable to read manifest file");
    let manifest: Manifest = toml::from_str(&manifest_content).expect("Invalid TOML format");

    let project_name = &manifest.package.name;
    let project_version = &manifest.package.version;

    let version = Version::parse(project_version).expect("Invalid version format");

    let ingot = InputIngot::new(
        db,
        path.to_str().unwrap(),
        ingot_kind,
        version,
        BTreeSet::default(),
    );

    if let Some(deps) = &manifest.dependencies {
        for dep in deps {
            let dep_path = path.join(dep);
            let dep_ingot = load_ingot(&dep_path, db, IngotKind::External, dependencies);
            dependencies.insert(IngotDependency::new(dep, dep_ingot));
        }
    }

    let src_path = path.join("src");
    set_src_files(&src_path, db, ingot);

    ingot
}

fn set_src_files(path: &Path, db: &mut dyn InputDb, ingot: InputIngot) {
    let input_files: BTreeSet<_> = WalkDir::new(path)
        .into_iter()
        // .expect("read_dir call failed")
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().is_file())
        .map(|entry| {
            let file_path = entry.path().to_str().unwrap().to_owned();
            let content = std::fs::read_to_string(&file_path).unwrap();
            InputFile::new(db, ingot, file_path.into(), content)
        })
        .collect();

    let root_file = input_files
        .iter()
        .find(|file| file.path(db).ends_with("lib.fe"))
        .expect("Root file 'lib.fe' not found")
        .clone();

    ingot.set_root_file(db, root_file);
    ingot.set_files(db, input_files);
}
