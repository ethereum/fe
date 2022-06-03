use clap::{Parser, Subcommand, Args};
use fe_common::panic::install_panic_hook;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct FelangCli {
    #[clap(subcommand)]
    command:Commands
}

#[derive(Subcommand)]
enum Commands {
    Compile(CompileArg),
    Check(CheckArg),
    New(NewArg)
}

#[derive(Args)]
#[clap(args_conflicts_with_subcommands = true)]
struct NewArg {
    name: String
}

#[derive(Args)]
#[clap(args_conflicts_with_subcommands = true)]
struct CompileArg {
    input: String,
    #[clap(short, long, default_value = "output")]
    output: String
}

#[derive(Args)]
#[clap(args_conflicts_with_subcommands = true)]
struct CheckArg {
    input: String
}


fn main() {
    install_panic_hook();

    let cli = FelangCli::parse();

    match cli.command {
        Commands::Compile(arg) => {
            println!("{}", arg.input);
            println!("{}", arg.output);
        }
        Commands::Check(arg) => {
            println!("{}", arg.input);
        }
        Commands::New(arg) => {
            println!("{}", arg.name);
        }
    }
}