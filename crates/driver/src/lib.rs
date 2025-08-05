pub mod db;
pub mod diagnostics;
pub mod files;

use std::mem::take;

use common::{
    graph::{EdgeWeight, JoinEdge},
    InputDb,
};
pub use db::DriverDataBase;

use common::config::Config;
use hir::hir_def::TopLevelMod;
use resolver::{
    files::{File, FilesResolutionError, FilesResolver},
    graph::DiGraph,
    ingot::ingot_graph_resolver,
    GraphResolutionHandler, ResolutionHandler, Resolver,
};
use url::Url;

pub fn init_workspace_ingot(
    db: &mut DriverDataBase,
    ingot_url: &Url,
) -> Vec<WorkspaceSetupDiagnostics> {
    tracing::trace!(target: "resolver", "Starting workspace ingot resolution for: {}", ingot_url);
    let node_handler = InputNodeHandler::from_db(db);
    let mut ingot_graph_resolver = ingot_graph_resolver(node_handler);
    let graph = ingot_graph_resolver.transient_resolve(ingot_url).unwrap();

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
                .map(|_diagnostic| WorkspaceSetupDiagnostics::FileError),
        )
        .collect();

    ingot_graph_resolver.node_handler.join_graph(graph);

    if diagnostics.is_empty() {
        tracing::trace!(target: "resolver", "Workspace ingot resolution completed successfully for: {}", ingot_url);
    } else {
        tracing::warn!(target: "resolver", "Workspace ingot resolution completed with {} diagnostics for: {}", diagnostics.len(), ingot_url);
    }

    diagnostics
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

impl std::fmt::Display for WorkspaceSetupDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WorkspaceSetupDiagnostics::UnresolvableIngotDependency { target, error } => {
                write!(
                    f,
                    "Failed to resolve ingot dependency '{target}': {error}"
                )
            }
            WorkspaceSetupDiagnostics::IngotDependencyCycle => {
                write!(f, "Detected cycle in ingot dependencies")
            }
            WorkspaceSetupDiagnostics::FileError => {
                write!(f, "File resolution error occurred")
            }
        }
    }
}

pub struct InputNodeHandler<'a> {
    pub db: &'a mut dyn InputDb,
    pub join_edges: Vec<JoinEdge>,
}

impl<'a> InputNodeHandler<'a> {
    pub fn from_db(db: &'a mut dyn InputDb) -> Self {
        Self {
            db,
            join_edges: vec![],
        }
    }

    pub fn join_graph(&mut self, graph: DiGraph<Url, EdgeWeight>) {
        self.db
            .graph()
            .join_graph(self.db, graph, take(&mut self.join_edges));
    }
}

impl<'a> ResolutionHandler<FilesResolver> for InputNodeHandler<'a> {
    type Item = Vec<(Url, EdgeWeight)>;

    fn handle_resolution(&mut self, ingot_url: &Url, files: Vec<File>) -> Self::Item {
        let mut config = None;

        // println!("{ingot_url}: {:#?}", files);

        for file in files {
            if file.path.ends_with("fe.toml") {
                self.db.workspace().touch(
                    self.db,
                    Url::from_file_path(file.path).unwrap(),
                    Some(file.content.clone()),
                );
                config = Some(file.content);
            } else {
                self.db.workspace().touch(
                    self.db,
                    Url::from_file_path(file.path).unwrap(),
                    Some(file.content),
                );
            }
        }

        if let Some(content) = config {
            let config = Config::parse(&content).unwrap();

            // let weights: HashSet<Url> = self.db.graph().node_weights().cloned().collect();

            config
                .based_dependencies(ingot_url)
                .into_iter()
                .filter_map(|based_dependency| {
                    if self.db.graph().contains_url(self.db, &based_dependency.url) {
                        self.join_edges.push(JoinEdge {
                            origin: ingot_url.clone(),
                            destination: based_dependency.url,
                            weight: EdgeWeight {
                                alias: based_dependency.alias,
                                arguments: based_dependency.parameters,
                            },
                        });
                        None
                    } else {
                        Some((
                            based_dependency.url,
                            EdgeWeight {
                                alias: based_dependency.alias,
                                arguments: based_dependency.parameters,
                            },
                        ))
                    }
                })
                .collect()
        } else {
            vec![]
        }
    }
}

impl<'a> GraphResolutionHandler<FilesResolver> for InputNodeHandler<'a> {
    type Item = Vec<(Url, EdgeWeight)>;

    fn handle_graph_resolution(&mut self, ingot_url: &Url, files: Vec<File>) -> Self::Item {
        self.handle_resolution(ingot_url, files)
    }
}
