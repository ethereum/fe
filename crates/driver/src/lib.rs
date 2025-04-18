pub mod db;
pub mod diagnostics;
pub mod files;
use camino::Utf8PathBuf;
use common::{
    ingot::{builtin_core, IngotBuilder},
    input::IngotKind,
};
pub use db::DriverDataBase;

use clap::{Parser, Subcommand};
use hir::hir_def::TopLevelMod;
use resolver::{
    ingot::{config::Config, source_files::SourceFiles, Ingot, IngotResolver},
    Resolver,
};

pub fn run(opts: &Options) {
    match &opts.command {
        Command::Build => eprintln!("`fe build` doesn't work at the moment"),
        Command::Check { path, core } => {
            let db = DriverDataBase::default();
            let mut ingot_resolver = IngotResolver::default();

            let core_ingot = if let Some(core_path) = core {
                match ingot_resolver.resolve(core_path) {
                    Ok(Ingot::Folder {
                        config:
                            Some(Config {
                                version: Some(version),
                                ..
                            }),
                        source_files:
                            Some(SourceFiles {
                                root: Some(root),
                                files,
                            }),
                    }) => {
                        let diagnostics = ingot_resolver.take_diagnostics();
                        if !diagnostics.is_empty() {
                            eprintln!("an error was encountered while resolving `{core_path}`");
                            for diagnostic in diagnostics {
                                eprintln!("{diagnostic}")
                            }
                            std::process::exit(2)
                        }
                        IngotBuilder::new(&db, "core")
                            .kind(IngotKind::Core)
                            .version(version)
                            .entrypoint(root)
                            .files_from_contents(files)
                            .build()
                    }
                    Ok(Ingot::SingleFile { .. }) => {
                        eprintln!("standalone core ingot not supported");
                        std::process::exit(2)
                    }
                    Ok(_) => {
                        eprintln!("an error was encountered while resolving `{core_path}`");
                        for diagnostic in ingot_resolver.take_diagnostics() {
                            eprintln!("{diagnostic}")
                        }
                        std::process::exit(2)
                    }
                    Err(error) => {
                        eprintln!("an error was encountered while resolving `{core_path}`");
                        eprintln!("{error}");
                        std::process::exit(2)
                    }
                }
            } else {
                builtin_core(&db)
            };

            let local_ingot = match ingot_resolver.resolve(path) {
                Ok(Ingot::Folder {
                    config:
                        Some(Config {
                            version: Some(version),
                            ..
                        }),
                    source_files:
                        Some(SourceFiles {
                            root: Some(root),
                            files,
                        }),
                }) => {
                    let diagnostics = ingot_resolver.take_diagnostics();
                    if !diagnostics.is_empty() {
                        eprintln!("an error was encountered while resolving `{path}`");
                        for diagnostic in diagnostics {
                            eprintln!("{diagnostic}")
                        }
                        std::process::exit(2)
                    }
                    // db.local_ingot(path, &version, &root, files, core_ingot).0
                    IngotBuilder::new(&db, path)
                        .kind(IngotKind::Local)
                        .version(version)
                        .entrypoint(root)
                        .files_from_contents(files)
                        .build()
                }
                Ok(Ingot::SingleFile { path, content }) => {
                    IngotBuilder::standalone(&db, path, content)
                        .with_core_ingot(core_ingot)
                        .build()
                }
                Ok(_) => {
                    eprintln!("an error was encountered while resolving `{path}`");
                    for diagnostic in ingot_resolver.take_diagnostics() {
                        eprintln!("{diagnostic}")
                    }
                    std::process::exit(2)
                }
                Err(error) => {
                    eprintln!("an error was encountered while resolving `{path}`");
                    eprintln!("{error}");
                    std::process::exit(2)
                }
            };

            let core_diags = db.run_on_ingot(core_ingot);
            let local_diags = db.run_on_ingot(local_ingot);

            if !core_diags.is_empty() || !local_diags.is_empty() {
                core_diags.emit(&db);
                local_diags.emit(&db);
                std::process::exit(1);
            }
        }
        Command::New => eprintln!("`fe new` doesn't work at the moment"),
    }
}

#[derive(Debug, Clone, Parser)]
#[command(version, about, long_about = None)]
pub struct Options {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Clone, Subcommand)]
pub enum Command {
    Build,
    Check {
        // #[clap(default_value_t = find_project_root().unwrap_or(Utf8PathBuf::from(".")))]
        path: Utf8PathBuf,
        #[arg(short, long)]
        core: Option<Utf8PathBuf>,
    },
    New,
}

fn _dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
    let mut s = vec![];
    top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
    String::from_utf8(s).unwrap()
}
