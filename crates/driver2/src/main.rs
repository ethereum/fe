use core::panic;
use std::str::FromStr;

use camino::Utf8PathBuf;
use clap::{Args, Parser, Subcommand};
use common::home_dir::HomeDir;
use fe_driver2::DriverDataBase;
use resolver::{
    ingot::{dependency_graph::DependencyGraphResolver, src_files::SourceFilesResolver},
    Resolver,
};
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

    /// Dump a graphviz dot file of the scope graph for the given file.
    #[arg(long = "dump-scope-graph", default_value_t = false)]
    dump_scope_graph: bool,
}

pub fn main() {
    let cli = Cli::parse();

    let Commands::Check(args) = &cli.command;

    // let home_dir = HomeDir::load();
    // let mut user_config_resolver = UserConfigResolver;
    // let user_config = user_config_resolver.resolve(&home_dir).unwrap();
    //
    // let core_path: Utf8PathBuf = match user_config.core {
    //     CoreIngotDescription::Local(path_description) => todo!(),
    //     CoreIngotDescription::Remote(git_description) => todo!(),
    // };

    let core_path = Utf8PathBuf::from_str("/home/grantwuerker/.fe/library/core").unwrap();

    let mut db = DriverDataBase::default();

    let local_path = Utf8PathBuf::from_str(&args.path).unwrap();

    let mut dependency_graph_resolver = DependencyGraphResolver::new();
    let dependency_graph = dependency_graph_resolver.resolve(&local_path).unwrap();

    let core_ingot = db.core_ingot(&core_path);
    let (local_ingot, external_ingots) = db.local_ingot(&dependency_graph);

    // panic!("{:#?}", dependency_graph);
    std::fs::write("graph.dot", format!("{}", dependency_graph.dot()))
        .expect("Unable to write file");

    // let mut source_files_resolver = SourceFilesResolver::new();
    // for (path, ingot) in external_ingots
    //     .iter()
    //     .chain([(&local_path, &local_ingot), (&core_path, &core_ingot)])
    // {
    //     let files = source_files_resolver
    //         .resolve(path)
    //         .expect(&path.to_string());
    //     db.set_ingot_files(*ingot, files);
    // }
    //
    // let diags = db.run_on_ingot(local_ingot);
    // let diags = diags.format_diags(&db);
    // if !diags.is_empty() {
    //     panic!("{diags}")
    // }
}

// let input_file = db.standalone(path, &source);
// let top_mod = db.top_mod(input_file);
// let diags = db.run_on_top_mod(top_mod);
// diags.emit(&db);

// if args.dump_scope_graph {
// println!("{}", dump_scope_graph(&db, top_mod));
// match &cli.command {
//     Commands::Check(args) => check::run_check(args),
// }

// fn dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
//     let mut s = vec![];
//     top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
//     String::from_utf8(s).unwrap()
// }

fn run_check(args: &CheckArgs) {
    // let mut db = DriverDataBase::default();
    //
    // let std_ingot = load_ingot(&std_path, &mut db, IngotKind::Std, &mut IndexSet::default());
    //
    // if path.is_file() {
    //     let source = fs::read_to_string(path).unwrap();
    //     check_single_file(path, source, std_ingot, &mut db, args.dump_scope_graph);
    // } else if path.is_dir() {
    //     check_ingot(path, std_ingot, &mut db, args.dump_scope_graph);
    // } else {
    //     eprintln!(
    //         "Path '{}' is neither a file nor a directory",
    //         path.display()
    //     );
    //     std::process::exit(2);
    // }
}
