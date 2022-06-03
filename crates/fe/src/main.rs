pub mod task;

use clap::{Args, Parser, Subcommand};
use fe_common::panic::install_panic_hook;


#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct FelangCli {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Compile(task::CompileArg),
    Check(task::CheckArg),
    New(task::InitArg),
}



fn main() {
    install_panic_hook();

    let cli = FelangCli::parse();

    match cli.command {
        Commands::Compile(arg) => {
            task::compile(arg);
        }
        Commands::Check(arg) => {
            task::check(arg);
        }
        Commands::New(arg) => {
            task::init(arg);
        }
    }
}
