mod build;
mod check;
mod lsp;
mod new;
mod utils;

pub use build::{build, BuildArgs};
pub use check::{check, CheckArgs};
use clap::Subcommand;
pub use lsp::{start_lsp_sever, LspArgs};
pub use new::{create_new_project, NewProjectArgs};

#[derive(Subcommand)]
pub enum Commands {
    Build(BuildArgs),
    Check(CheckArgs),
    New(NewProjectArgs),
    Lsp(LspArgs),
}
