mod task;

use clap::{Parser, Subcommand};
use fe_common::panic::install_panic_hook;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct FelangCli {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Compile(task::CompileArgs),
    Check(task::CheckArgs),
    Init(task::InitArgs),
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
        Commands::Init(arg) => {
            task::init(arg);
        }
    }
}
