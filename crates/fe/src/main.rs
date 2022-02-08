//! The `fe` command-line interface.

#![allow(unused_imports, dead_code)] // XXX

// XXX #![deny(missing_docs)]

use clap::Parser;
use fe_common::diagnostics::print_diagnostics;
use fe_common::files::SourceFileId;
use fe_common::panic::install_panic_hook;
use fe_driver::CompiledModule;
use std::ffi::OsStr;
use std::fs;
use std::io::{Error, Write};
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

pub mod commands;
use commands::{build, check, new};

const DEFAULT_OUTPUT_DIR_NAME: &str = "output";

/// Compiler for the Fe language
#[derive(Parser)]
#[clap(version)]
struct FeCli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    /// Compile the current project, or a single Fe source file.
    Build(commands::build::BuildOpts),

    /// Analyze the current project (or single file) and report errors.
    Check(commands::check::CheckOpts),

    /// Create a new fe project.
    New(commands::new::NewOpts),
    // Clean,
}

// XXX see cargo_test_support::project;

pub fn main() -> anyhow::Result<()> {
    install_panic_hook();

    let cli = FeCli::parse();
    match cli.command {
        Command::Build(opts) => build::build(opts),
        Command::Check(opts) => check::check(opts),
        Command::New(opts) => new::new(opts),
    }
}
