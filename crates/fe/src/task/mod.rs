mod build;
mod check;
mod new;
#[cfg(feature = "solc-backend")]
mod test;
mod verify;

pub use build::{build, BuildArgs};
pub use check::{check, CheckArgs};
use clap::Subcommand;
pub use new::{create_new_project, NewProjectArgs};
#[cfg(feature = "solc-backend")]
pub use test::{test, TestArgs};
#[cfg(feature = "solc-backend")]
pub use verify::{verify, VerifyArgs};

#[derive(Subcommand)]
pub enum Commands {
    Build(BuildArgs),
    Check(CheckArgs),
    New(NewProjectArgs),
    #[cfg(feature = "solc-backend")]
    Verify(VerifyArgs),
    #[cfg(feature = "solc-backend")]
    Test(TestArgs),
}
