pub mod db;
pub mod diagnostics;
pub mod files;
use camino::Utf8PathBuf;
pub use db::{DriverDataBase, DriverDb};

use clap::{Parser, Subcommand};
use hir::hir_def::TopLevelMod;

pub fn run(opts: &Options) {
    match &opts.command {
        Command::Build => eprintln!("`fe build` doesn't work at the moment"),
        Command::Check { path } => {
            if !path.exists() {
                eprintln!("file '{}' does not exist", path);
                std::process::exit(2);
            }
            let source = std::fs::read_to_string(path).unwrap();

            let mut db = DriverDataBase::default();
            let (ingot, file) = db.standalone(path, &source);
            let top_mod = db.top_mod(ingot, file);
            let diags = db.run_on_top_mod(top_mod);
            diags.emit(&db);
        }
        Command::New => eprintln!("`fe new` doesn't work at the moment"),
    }
}

#[derive(Debug, Clone, Parser)]
#[command(version, about, long_about = None)]
pub struct Options {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    Build,
    Check {
        // #[clap(default_value_t = find_project_root().unwrap_or(Utf8PathBuf::from(".")))]
        path: Utf8PathBuf,
    },
    New,
}

fn _dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
    let mut s = vec![];
    top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
    String::from_utf8(s).unwrap()
}
