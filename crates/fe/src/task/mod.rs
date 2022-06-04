mod check;
mod compile;
mod init;
mod utils;

pub use check::{check, CheckArgs};
use clap::Subcommand;
pub use compile::{compile, CompileArgs};
pub use init::{init, InitArgs};

#[derive(Subcommand)]
pub enum Commands {
    Compile(CompileArgs),
    Check(CheckArgs),
    Init(InitArgs),
}
