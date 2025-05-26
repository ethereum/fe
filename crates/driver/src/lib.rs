pub mod db;
pub mod diagnostics;
pub mod files;
use camino::Utf8PathBuf;
use common::core::HasBuiltinCore;
use common::ingot::IngotBaseUrl;

use common::InputDb;
pub use db::DriverDataBase;

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

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
            let base_url = Url::from_directory_path(path.canonicalize_utf8().unwrap())
                .expect("failed to parse base URL");
            let mut db = DriverDataBase::default();
            let mut ingot_resolver = IngotResolver::default();

            let core_ingot = if let Some(core_path) = core {
                match ingot_resolver.resolve(core_path) {
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
                                eprintln!("{diagnostic}")
                            }
                            std::process::exit(2)
                        }
                        let index = db.workspace();
                        index.touch_ingot(
                            &mut db,
                            &core_base_url,
                            config.expect("config is required"),
                        );
                        for (path, content) in files {
                            index.touch(
                                &mut db,
                                Url::from_file_path(
                                    path.canonicalize().expect("Failed to canonicalize path"),
                                )
                                .expect("Failed to create URL"),
                                Some(content),
                            );
                        }
                        core_base_url
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
                db.builtin_core().base(&db)
            };

            let local_ingot = match ingot_resolver.resolve(path) {
                Ok(Ingot::Folder {
                    config,
                    source_files:
                        Some(SourceFiles {
                            root: Some(_root),
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
                    let index = db.workspace();
                    index.touch_ingot(&mut db, &base_url, config.expect("config is required"));
                    for (path, content) in files {
                        index.touch(
                            &mut db,
                            Url::from_file_path(
                                path.canonicalize().expect("Failed to canonicalize path"),
                            )
                            .expect("Failed to create URL"),
                            Some(content),
                        );
                    }
                    base_url
                }
                Ok(Ingot::SingleFile { path, content }) => {
                    let url = Url::from_file_path(path.canonicalize_utf8().unwrap()).unwrap();
                    db.workspace().touch(&mut db, url.clone(), Some(content));
                    db.workspace()
                        .containing_ingot(&db, &url)
                        .expect("Failed to find ingot")
                        .base(&db)
                }
                Ok(_) => {
                    eprintln!("an error was encountered while resolving `{base_url}`");
                    for diagnostic in ingot_resolver.take_diagnostics() {
                        eprintln!("{diagnostic}")
                    }
                    std::process::exit(2)
                }
                Err(error) => {
                    eprintln!("an error was encountered while resolving `{base_url}`");
                    eprintln!("{error}");
                    std::process::exit(2)
                }
            };

            let core_diags =
                db.run_on_ingot(core_ingot.ingot(&db).expect("core ingot should exist"));
            let local_diags =
                db.run_on_ingot(local_ingot.ingot(&db).expect("local ingot should exist"));

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
