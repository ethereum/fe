mod check;
mod tree;

use camino::Utf8PathBuf;
use check::check;
use clap::{Parser, Subcommand};

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
        #[arg(short, long)]
        core: Option<Utf8PathBuf>,
    },
    Tree {
        path: Utf8PathBuf,
    },
    New,
}

fn main() {
    let opts = Options::parse();
    run(&opts);
}
pub fn run(opts: &Options) {
    match &opts.command {
        Command::Build => eprintln!("`fe build` doesn't work at the moment"),
        Command::Check { path, core: _ } => {
            //: TODO readd custom core
            check(path);
        }
        Command::Tree { path } => {
            tree::print_tree(path);
        }
        Command::New => eprintln!("`fe new` doesn't work at the moment"),
    }
}
