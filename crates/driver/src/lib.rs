#![allow(clippy::print_stderr)]

pub mod db;
pub mod diagnostics;
pub mod files;

use camino::Utf8PathBuf;
use common::core::HasBuiltinCore;
use common::ingot::IngotBaseUrl;

use common::InputDb;
pub use db::DriverDataBase;

use clap::{Parser, Subcommand};
use hir::hir_def::TopLevelMod;
use resolver::{
    ingot::{source_files::SourceFiles, Ingot, IngotResolver},
    Resolver,
};
use url::Url;

pub fn run(opts: &Options) {
    match &opts.command {
        Command::Build => eprintln!("`fe build` doesn't work at the moment"),
        Command::Check { path, core } => {
            let mut db = DriverDataBase::default();
            let mut ingot_resolver = IngotResolver::default();

            let core_url = if let Some(core_path) = core {
                if !core_path.exists() {
                    eprintln!("the core path `{core_path}` does not exist");
                    std::process::exit(1)
                }

                let core_url = match core_path.canonicalize_utf8() {
                    Ok(canonical_path) => {
                        if canonical_path.is_file() {
                            Url::from_file_path(canonical_path)
                                .expect("unable to create file url from directory path ")
                        } else {
                            Url::from_directory_path(canonical_path)
                                .expect("unable to create directory url from canonical path")
                        }
                    }
                    Err(err) => {
                        eprintln!("failed to canonicalize path `{core_path}`: {err}");
                        std::process::exit(1)
                    }
                };

                match ingot_resolver.resolve(&core_url) {
                    Ok(Ingot::Folder {
                        config,
                        source_files:
                            Some(SourceFiles {
                                root: Some(_root),
                                files,
                            }),
                    }) => {
                        let core_base_url = Url::parse("core-ingot:///").unwrap();
                        let diagnostics = ingot_resolver.take_diagnostics();
                        if !diagnostics.is_empty() {
                            eprintln!("an error was encountered while resolving `{core_path}`");
                            for diagnostic in diagnostics {
                                eprintln!("{diagnostic}");
                            }
                            std::process::exit(1)
                        }
                        let index = db.workspace();
                        if let Some(config) = config {
                            let config_url = config.url;
                            index.touch_ingot(&mut db, &core_base_url, Some(config.content));
                            let config = core_base_url
                                .ingot(&db)
                                .expect("core ingot should exist")
                                .config(&db)
                                .expect("core ingot config should exist");
                            if let Some(diagnostics) = config.formatted_diagnostics() {
                                eprintln!(
                                    "there are issues with the core fe.toml file {config_url}"
                                );
                                eprintln!("{diagnostics}");
                                std::process::exit(1)
                            }
                        } else {
                            index.touch_ingot(&mut db, &core_base_url, None);
                        };
                        for (file_url, content) in files {
                            let rebased_file_url = core_base_url
                                .join(&file_url.path()[core_url.path().len()..])
                                .unwrap();
                            index.touch(&mut db, rebased_file_url, Some(content));
                        }
                        core_base_url
                    }
                    Ok(Ingot::SingleFile { .. }) => {
                        eprintln!("standalone core ingot not supported");
                        std::process::exit(1)
                    }
                    Ok(_) => {
                        eprintln!("an error was encountered while resolving `{core_path}`");
                        for diagnostic in ingot_resolver.take_diagnostics() {
                            eprintln!("{diagnostic}");
                        }
                        std::process::exit(1)
                    }
                    Err(error) => {
                        eprintln!("an error was encountered while resolving `{core_path}`");
                        eprintln!("{error}");
                        std::process::exit(1)
                    }
                }
            } else {
                db.builtin_core().base(&db)
            };

            if !path.exists() {
                eprintln!("the path `{path}` does not exist");
                std::process::exit(1)
            }

            let path_url = match path.canonicalize_utf8() {
                Ok(canonical_path) => {
                    if canonical_path.is_file() {
                        Url::from_file_path(canonical_path)
                            .expect("unable to create file url from directory path ")
                    } else {
                        Url::from_directory_path(canonical_path)
                            .expect("unable to create directory url from canonical path")
                    }
                }
                Err(err) => {
                    eprintln!("failed to canonicalize path `{path}`: {err}");
                    std::process::exit(1)
                }
            };

            let local_url = match ingot_resolver.resolve(&path_url) {
                Ok(Ingot::Folder {
                    config,
                    source_files:
                        Some(SourceFiles {
                            root: Some(_root),
                            files,
                        }),
                }) => {
                    let base_url = Url::from_directory_path(path.canonicalize_utf8().unwrap())
                        .expect("failed to parse base URL");

                    let diagnostics = ingot_resolver.take_diagnostics();
                    if !diagnostics.is_empty() {
                        eprintln!("an error was encountered while resolving `{path}`");
                        for diagnostic in diagnostics {
                            eprintln!("{diagnostic}");
                        }
                        std::process::exit(1)
                    }
                    let index = db.workspace();
                    if let Some(config) = config {
                        let config_url = config.url;
                        index.touch_ingot(&mut db, &base_url, Some(config.content));
                        let config = base_url
                            .ingot(&db)
                            .expect("local ingot should exist")
                            .config(&db)
                            .expect("local ingot config should exist");
                        if let Some(diagnostics) = config.formatted_diagnostics() {
                            eprintln!("there are issues with the local fe.toml file {config_url}",);
                            eprintln!("{diagnostics}");
                            std::process::exit(1)
                        }
                    } else {
                        index.touch_ingot(&mut db, &base_url, None);
                    };

                    for (file_url, content) in files {
                        index.touch(&mut db, file_url, Some(content));
                    }
                    base_url
                }
                Ok(Ingot::SingleFile { url, content }) => {
                    db.workspace().touch(&mut db, url.clone(), Some(content));
                    url
                }
                Ok(_) => {
                    for diagnostic in ingot_resolver.take_diagnostics() {
                        eprintln!("{diagnostic}");
                    }
                    std::process::exit(1)
                }
                Err(error) => {
                    eprintln!("{error}: {path}");
                    std::process::exit(1)
                }
            };

            let core_source_diags =
                db.run_on_ingot(core_url.ingot(&db).expect("core ingot should exist"));
            if !core_source_diags.is_empty() {
                eprintln!("errors in {core_url}\n");
                core_source_diags.emit(&db);
                std::process::exit(1);
            }

            let local_source_diags = db.run_on_ingot(local_url.ingot(&db).unwrap());
            if !local_source_diags.is_empty() {
                eprintln!("errors in {local_url}\n");
                local_source_diags.emit(&db);
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

// Maybe the driver should eventually only support WASI?
