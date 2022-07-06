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
        #[cfg(feature = "lsp-support")]
        Commands::Lsp(args) => {
            let rt = tokio::runtime::Runtime::new().unwrap();
            rt.block_on(async {
                task::start_lsp_sever(args).await;
            });
        }
    }
}
