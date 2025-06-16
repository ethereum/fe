pub mod db;
pub mod diagnostics;
pub mod files;

use std::{collections::HashSet, mem::take};

use common::InputDb;
pub use db::DriverDataBase;

use common::{
    config::{Config, IngotArguments},
    file::workspace::Workspace,
};
use hir::hir_def::TopLevelMod;
use resolver::{
    files::{File, FilesResolutionError, FilesResolver},
    graph::DiGraph,
    ingot::ingot_graph_resolver,
    ResolutionHandler, Resolver,
};
use smol_str::SmolStr;
use url::Url;

pub fn init_workspace_ingot(db: &mut DriverDataBase, ingot_url: &Url) {
    let workspace = db.workspace();
    let node_handler = InputNodeHandler::from_workspace(db, workspace);
    let mut ingot_graph_resolver = ingot_graph_resolver(node_handler);
    let graph = ingot_graph_resolver.transient_resolve(&ingot_url).unwrap();

    let diagnostics: Vec<WorkspaceSetupDiagnostics> = ingot_graph_resolver
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

    ingot_graph_resolver.node_handler.join_graph(graph);
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
    pub join_edges: Vec<(Url, Url, (SmolStr, IngotArguments))>,
}

impl<'a> InputNodeHandler<'a> {
    pub fn from_workspace(db: &'a mut dyn InputDb, workspace: Workspace) -> Self {
        Self {
            db,
            workspace,
            join_edges: vec![],
        }
    }

    pub fn join_graph(&mut self, graph: DiGraph<Url, (SmolStr, IngotArguments)>) {
        self.workspace
            .join_graph(self.db, graph, take(&mut self.join_edges));
    }
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

            let weights: HashSet<Url> = self
                .workspace
                .get_graph(self.db)
                .node_weights()
                .cloned()
                .collect();

            config
                .based_dependencies(ingot_url)
                .into_iter()
                .filter_map(|based_dependency| {
                    if weights.contains(&based_dependency.url) {
                        self.join_edges.push((
                            ingot_url.clone(),
                            based_dependency.url,
                            (based_dependency.alias, based_dependency.arguments),
                        ));
                        None
                    } else {
                        Some((
                            based_dependency.url,
                            (based_dependency.alias, based_dependency.arguments),
                        ))
                    }
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
