use clap::{Args, Parser, Subcommand};
use common::input::{IngotDependency, InputFile};
use common::{input::IngotKind, InputDb, InputIngot};
use fe_driver2::DriverDataBase;
use hir::hir_def::TopLevelMod;
use semver::Version;
use serde::Deserialize;
use std::{collections::BTreeSet, path::Path};
use walkdir::WalkDir;
mod check;
use common::indexmap::IndexSet;

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

    // let mut db = DriverDataBase::default();
    // let input_file = db.standalone(path, &source);
    // let top_mod = db.top_mod(input_file);
    // let diags = db.run_on_top_mod(top_mod);
    // diags.emit(&db);

    // if args.dump_scope_graph {
    // println!("{}", dump_scope_graph(&db, top_mod));
    match &cli.command {
        Commands::Check(args) => check::run_check(args),
    }
}

// fn dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
//     let mut s = vec![];
//     top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
//     String::from_utf8(s).unwrap()
// }

fn run_check(args: &CheckArgs) {
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
