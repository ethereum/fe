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
use tracing::{error, warn};
use url::Url;

pub fn run(opts: &Options) {
    match &opts.command {
        Command::Build => warn!("`fe build` doesn't work at the moment"),
        Command::Check { path, core } => {
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
                            error!("an error was encountered while resolving `{core_path}`");
                            for diagnostic in diagnostics {
                                error!("{diagnostic}")
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
                                url_from_file_path(
                                    path.canonicalize().expect("Failed to canonicalize path"),
                                )
                                .expect("Failed to create URL"),
                                Some(content),
                            );
                        }
                        core_base_url
                    }
                    Ok(Ingot::SingleFile { .. }) => {
                        error!("standalone core ingot not supported");
                        std::process::exit(2)
                    }
                    Ok(_) => {
                        error!("an error was encountered while resolving `{core_path}`");
                        for diagnostic in ingot_resolver.take_diagnostics() {
                            error!("{diagnostic}")
                        }
                        std::process::exit(2)
                    }
                    Err(error) => {
                        error!("an error was encountered while resolving `{core_path}`");
                        error!("{error}");
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
                    let base_url = url_from_directory_path(path.canonicalize_utf8().unwrap())
                        .expect("failed to parse base URL");

                    let diagnostics = ingot_resolver.take_diagnostics();
                    if !diagnostics.is_empty() {
                        error!("an error was encountered while resolving `{path}`");
                        for diagnostic in diagnostics {
                            error!("{diagnostic}")
                        }
                        std::process::exit(2)
                    }
                    let index = db.workspace();
                    index.touch_ingot(&mut db, &base_url, config.expect("config is required"));
                    for (path, content) in files {
                        index.touch(
                            &mut db,
                            url_from_file_path(
                                path.canonicalize().expect("Failed to canonicalize path"),
                            )
                            .expect("Failed to create URL"),
                            Some(content),
                        );
                    }
                    base_url.ingot(&db).expect("Failed to find ingot")
                }
                Ok(Ingot::SingleFile { path, content }) => {
                    let url = url_from_file_path(path.canonicalize_utf8().unwrap()).unwrap();
                    db.workspace().touch(&mut db, url.clone(), Some(content));
                    db.workspace()
                        .containing_ingot(&db, &url)
                        .expect("Failed to find ingot")
                }
                Ok(_) => {
                    for diagnostic in ingot_resolver.take_diagnostics() {
                        error!("{diagnostic}")
                    }
                    std::process::exit(2)
                }
                Err(error) => {
                    error!("{error}: {path}");
                    std::process::exit(2)
                }
            };

            let core_diags =
                db.run_on_ingot(core_ingot.ingot(&db).expect("core ingot should exist"));
            let local_diags = db.run_on_ingot(local_ingot);

            if !core_diags.is_empty() || !local_diags.is_empty() {
                core_diags.emit(&db);
                local_diags.emit(&db);
                std::process::exit(1);
            }
        }
        Command::New => warn!("`fe new` doesn't work at the moment"),
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

fn url_from_file_path<P: AsRef<std::path::Path>>(path: P) -> Result<Url, ()> {
    #[cfg(not(target_arch = "wasm32"))]
    {
        Url::from_file_path(path)
    }

    #[cfg(target_arch = "wasm32")]
    {
        let path_str = path.as_ref().to_string_lossy();
        let url_str = if path_str.starts_with('/') {
            format!("file://{}", path_str)
        } else {
            format!("file:///{}", path_str)
        };
        Url::parse(&url_str).map_err(|_| ())
    }
}

fn url_from_directory_path<P: AsRef<std::path::Path>>(path: P) -> Result<Url, ()> {
    #[cfg(not(target_arch = "wasm32"))]
    {
        Url::from_directory_path(path)
    }

    #[cfg(target_arch = "wasm32")]
    {
        let path_str = path.as_ref().to_string_lossy();
        let url_str = if path_str.starts_with('/') {
            if path_str.ends_with('/') {
                format!("file://{}", path_str)
            } else {
                format!("file://{}/", path_str)
            }
        } else {
            if path_str.ends_with('/') {
                format!("file:///{}", path_str)
            } else {
                format!("file:///{}/", path_str)
            }
        };
        Url::parse(&url_str).map_err(|_| ())
    }
}
