pub mod db;
pub mod diagnostics;
pub mod files;
use camino::Utf8PathBuf;

use common::InputDb;
pub use db::DriverDataBase;

use clap::Subcommand;
use common::{
    config::{Config, DependencyDescription, IngotArguments},
    file::workspace::Workspace,
};
use hir::hir_def::TopLevelMod;
use resolver::{files::FilesResolver, ingot::ingot_graph_resolver, ResolutionHandler, Resolver};
use smol_str::SmolStr;
use url::Url;

pub struct InputNodeHandler<'a> {
    pub db: &'a mut dyn InputDb,
    pub workspace: Workspace,
}

impl<'a> ResolutionHandler<FilesResolver> for InputNodeHandler<'a> {
    type Item = Vec<(Utf8PathBuf, (SmolStr, IngotArguments))>;

    fn handle_resolution(
        &mut self,
        ingot_path: &Utf8PathBuf,
        files: Vec<(Utf8PathBuf, String)>,
    ) -> Self::Item {
        let mut config = None;

        // println!("{ingot_path}: {:#?}", files);

        for (path, content) in files {
            if path.ends_with("fe.toml") {
                self.workspace.touch(
                    self.db,
                    Url::from_file_path(path).unwrap(),
                    Some(content.clone()),
                );
                config = Some(content);
            } else {
                self.workspace
                    .touch(self.db, Url::from_file_path(path).unwrap(), Some(content));
            }
        }

        if let Some(content) = config {
            let config = Config::from_string(content);
            config
                .dependencies
                .into_iter()
                .map(|dependency| match dependency.description {
                    DependencyDescription::Path(path) => (
                        ingot_path.join(path).canonicalize_utf8().unwrap(),
                        (dependency.alias, IngotArguments::default()),
                    ),
                    DependencyDescription::PathWithArguments { path, arguments } => (
                        ingot_path.join(path).canonicalize_utf8().unwrap(),
                        (dependency.alias, arguments),
                    ),
                })
                .collect()
        } else {
            vec![]
        }
    }
}

pub fn setup_workspace(db: &mut dyn InputDb, path: &Utf8PathBuf) -> Workspace {
    let workspace = db.workspace();
    let node_handler = InputNodeHandler { db, workspace };
    let mut graph_resolver = ingot_graph_resolver(node_handler);
    let graph = graph_resolver
        .transient_resolve(&path.canonicalize_utf8().unwrap())
        .unwrap();

    workspace
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
