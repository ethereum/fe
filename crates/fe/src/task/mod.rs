mod check;
mod compile;
mod init;
mod utils;

use clap::{Subcommand};
pub use check::{check, CheckArgs};
pub use compile::{compile, CompileArgs};
pub use init::{init, InitArgs};

#[derive(Subcommand)]
pub enum Commands {
    Compile(CompileArgs),
    Check(CheckArgs),
    Init(InitArgs),
}