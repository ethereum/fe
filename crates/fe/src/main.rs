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
        Commands::Build(arg) => {
            task::build(arg);
        }
        Commands::Check(arg) => {
            task::check(arg);
        }
        Commands::New(arg) => {
            task::create_new_project(arg);
        }
        #[cfg(feature = "solc-backend")]
        Commands::Test(arg) => {
            task::test(arg);
        }
    }
}
