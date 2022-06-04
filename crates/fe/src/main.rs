mod task;

use clap::Parser;
use fe_common::panic::install_panic_hook;
use task::Commands;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct FelangCli {
    #[clap(subcommand)]
    command: Commands,
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
