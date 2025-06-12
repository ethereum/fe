pub mod db;
pub mod diagnostics;
pub mod files;

use camino::Utf8PathBuf;

use common::{config::DependencyDescription, urlext::canonical_url, InputDb};
pub use db::DriverDataBase;

use common::{
    config::{Config, IngotArguments},
    file::workspace::Workspace,
};
use hir::hir_def::TopLevelMod;
use resolver::{
    files::{File, FilesResolutionError, FilesResolver},
    ingot::ingot_graph_resolver,
    ResolutionHandler, Resolver,
};
use smol_str::SmolStr;
use url::Url;

pub fn setup_workspace(
    path: &Utf8PathBuf,
) -> (
    DriverDataBase,
    Workspace,
    Url,
    Vec<Url>,
    Vec<WorkspaceSetupDiagnostics>,
) {
    let mut db = DriverDataBase::default();
    let ingot_url = canonical_url(path).unwrap();
    let workspace = db.workspace();
    let node_handler = InputNodeHandler {
        db: &mut db,
        workspace,
    };
    let mut ingot_graph_resolver = ingot_graph_resolver(node_handler);
    let graph = ingot_graph_resolver.transient_resolve(&ingot_url).unwrap();
    let dependency_urls = graph
        .node_weights()
        .filter(|weight| *weight != &ingot_url)
        .cloned()
        .collect();

    let diagnostics = ingot_graph_resolver
        .take_diagnostics()
        .into_iter()
        .map(
            |diagnostic| WorkspaceSetupDiagnostics::UnresolvableIngotDependency {
                target: diagnostic.0,
                error: diagnostic.1,
            },
        )
        .chain(
            ingot_graph_resolver
                .node_resolver
                .take_diagnostics()
                .into_iter()
                .map(|diagnostic| WorkspaceSetupDiagnostics::FileError),
        )
        .collect();

    (db, workspace, ingot_url, dependency_urls, diagnostics)
}

fn _dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
    let mut s = vec![];
    top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
    String::from_utf8(s).unwrap()
}

// Maybe the driver should eventually only support WASI?

#[derive(Debug)]
pub enum WorkspaceSetupDiagnostics {
    UnresolvableIngotDependency {
        target: Url,
        error: FilesResolutionError,
    },
    IngotDependencyCycle,
    FileError,
}

pub struct InputNodeHandler<'a> {
    pub db: &'a mut dyn InputDb,
    pub workspace: Workspace,
}

impl<'a> ResolutionHandler<FilesResolver> for InputNodeHandler<'a> {
    type Item = Vec<(Url, (SmolStr, IngotArguments))>;

    fn handle_resolution(&mut self, ingot_url: &Url, files: Vec<File>) -> Self::Item {
        let mut config = None;

        // println!("{ingot_url}: {:#?}", files);

        for file in files {
            if file.path.ends_with("fe.toml") {
                self.workspace.touch(
                    self.db,
                    Url::from_file_path(file.path).unwrap(),
                    Some(file.content.clone()),
                );
                config = Some(file.content);
            } else {
                self.workspace.touch(
                    self.db,
                    Url::from_file_path(file.path).unwrap(),
                    Some(file.content),
                );
            }
        }

        if let Some(content) = config {
            let config = Config::from_string(content);
            config
                .based_dependencies(ingot_url)
                .into_iter()
                .map(|based_dependency| {
                    (
                        // Node weight
                        based_dependency.url,
                        // Node edge
                        (based_dependency.alias, based_dependency.arguments),
                    )
                })
                .collect()
        } else {
            vec![]
        }
    }
}

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
