pub mod db;
pub mod diagnostics;
pub mod files;
use camino::Utf8PathBuf;
use common::indexmap::IndexMap;
pub use db::{DriverDataBase, DriverDb};

use clap::{Parser, Subcommand};
use hir::hir_def::TopLevelMod;
use resolver::{
    ingot::{
        config::Config,
        source_files::{SourceFiles, SourceFilesResolver},
        Ingot, IngotResolver,
    },
    Resolver,
};

pub fn run(opts: &Options) {
    match &opts.command {
        Command::Build => eprintln!("`fe build` doesn't work at the moment"),
        Command::Check { path, core } => {
            let mut db = DriverDataBase::default();
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
                        ..
                    }) => {
                        let diagnostics = ingot_resolver.take_diagnostics();
                        if !diagnostics.is_empty() {
                            eprintln!("an error was encountered while resolving `{core_path}`");
                            for diagnostic in diagnostics {
                                eprintln!("{diagnostic}")
                            }
                            std::process::exit(2)
                        }
                        db.core_ingot(core_path, &version, &root, files).0
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
                db.static_core_ingot().0
            };

            let (local_ingot, external_ingots) = match ingot_resolver.resolve(path) {
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
                    dependency_graph: Some(dependency_graph),
                }) => {
                    let diagnostics = ingot_resolver.take_diagnostics();
                    if !diagnostics.is_empty() {
                        eprintln!("an error was encountered while resolving `{path}`");
                        for diagnostic in diagnostics {
                            eprintln!("{diagnostic}")
                        }
                        std::process::exit(2)
                    }
                    std::fs::write("dependencies.dot", dependency_graph.dot()).expect("failed");
                    let (local_ingot, external_ingots) =
                        db.local_ingot(&dependency_graph, core_ingot);
                    db.set_ingot_source_files(local_ingot, &root, files);
                    let mut source_files_resolver = SourceFilesResolver::default();
                    for (external_path, external_ingot) in external_ingots.iter() {
                        match source_files_resolver.resolve(&external_path) {
                            Ok(SourceFiles {
                                root: Some(root),
                                files,
                            }) => {
                                db.set_ingot_source_files(*external_ingot, &root, files);
                            }
                            Ok(_) => todo!(),
                            Err(_) => todo!(),
                        }
                    }
                    (local_ingot, external_ingots)
                }
                Ok(Ingot::SingleFile { path, content }) => (
                    db.standalone(&path, &content, core_ingot).0,
                    IndexMap::new(),
                ),
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
            core_diags.emit(&db);
            local_diags.emit(&db);
            for external_ingot in external_ingots.values() {
                let diags = db.run_on_ingot(*external_ingot);
                diags.emit(&db);
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
