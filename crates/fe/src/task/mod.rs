mod build;
mod check;
#[cfg(feature = "lsp-support")]
mod lsp;
mod new;
mod utils;

pub use build::{build, BuildArgs};
pub use check::{check, CheckArgs};
use clap::Subcommand;
#[cfg(feature = "lsp-support")]
pub use lsp::{start_lsp_sever, LspArgs};
pub use new::{create_new_project, NewProjectArgs};

#[derive(Subcommand)]
pub enum Commands {
    Build(BuildArgs),
    Check(CheckArgs),
    New(NewProjectArgs),
    #[cfg(feature = "lsp-support")]
    Lsp(LspArgs),
}
