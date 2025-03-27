#![allow(clippy::print_stderr)]

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
    files::{File, FilesResolutionDiagnostic, FilesResolutionError, FilesResolver},
    graph::{DiGraph, GraphResolutionHandler, GraphResolver},
    ingot::ingot_graph_resolver,
    ResolutionHandler, Resolver,
};
use url::Url;

pub fn init_ingot(db: &mut DriverDataBase, ingot_url: &Url) -> Vec<IngotInitDiagnostics> {
    tracing::trace!(target: "resolver", "Starting workspace ingot resolution for: {}", ingot_url);
    let mut diagnostics: Vec<IngotInitDiagnostics> = {
        let mut handler = InputHandler::from_db(db);
        let mut ingot_graph_resolver = ingot_graph_resolver::<InputHandler>();
        ingot_graph_resolver
            .graph_resolve(&mut handler, ingot_url)
            .unwrap();

        // Collect diagnostics from all sources
        let mut all_diagnostics = Vec::new();

        // Add handler diagnostics (missing fe.toml)
        all_diagnostics.extend(handler.diagnostics);

        // Add graph resolver diagnostics (unresolvable dependencies)
        all_diagnostics.extend(ingot_graph_resolver.take_diagnostics().into_iter().map(
            |diagnostic| IngotInitDiagnostics::UnresolvableIngotDependency {
                target: diagnostic.0,
                error: diagnostic.1,
            },
        ));

        // Add files resolver diagnostics (file errors)
        all_diagnostics.extend(
            ingot_graph_resolver
                .node_resolver
                .take_diagnostics()
                .into_iter()
                .map(|diagnostic| IngotInitDiagnostics::FileError { diagnostic }),
        );

        all_diagnostics
    };

    // Check for cycles after graph resolution (now that handler is dropped)
    let cycles = db.graph().cycles(db);

    // Add cycle diagnostics - one per cycle
    for cycle in cycles {
        diagnostics.push(IngotInitDiagnostics::IngotDependencyCycle { cycle });
    }

    if diagnostics.is_empty() {
        tracing::trace!(target: "resolver", "Ingot resolution completed successfully for: {}", ingot_url);
    } else {
        tracing::warn!(target: "resolver", "Ingot resolution completed with {} diagnostics for: {}", diagnostics.len(), ingot_url);
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
pub enum IngotInitDiagnostics {
    UnresolvableIngotDependency {
        target: Url,
        error: FilesResolutionError,
    },
    IngotDependencyCycle {
        cycle: Vec<Url>,
    },
    FileError {
        diagnostic: FilesResolutionDiagnostic,
    },
    MissingFeToml {
        ingot_url: Url,
    },
}

impl std::fmt::Display for IngotInitDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IngotInitDiagnostics::UnresolvableIngotDependency { target, error } => {
                write!(f, "Failed to resolve ingot dependency '{target}': {error}")
            }
            IngotInitDiagnostics::IngotDependencyCycle { cycle: _ } => {
                write!(f, "Detected cycle in ingot dependencies")
            }
            IngotInitDiagnostics::FileError { diagnostic } => {
                write!(f, "File resolution error: {diagnostic}")
            }
            IngotInitDiagnostics::MissingFeToml { ingot_url } => {
                write!(f, "Missing fe.toml file in ingot: {ingot_url}")
            }
        }
    }
}

pub struct InputHandler<'a> {
    pub db: &'a mut dyn InputDb,
    pub join_edges: Vec<JoinEdge>,
    pub diagnostics: Vec<IngotInitDiagnostics>,
}

impl<'a> InputHandler<'a> {
    pub fn from_db(db: &'a mut dyn InputDb) -> Self {
        Self {
            db,
            join_edges: vec![],
            diagnostics: vec![],
        }
    }
}

impl<'a> ResolutionHandler<FilesResolver> for InputHandler<'a> {
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
            // No fe.toml file found - record this as a diagnostic
            self.diagnostics.push(IngotInitDiagnostics::MissingFeToml {
                ingot_url: ingot_url.clone(),
            });
            vec![]
        }
    }
}

impl<'a> GraphResolutionHandler<Url, DiGraph<Url, EdgeWeight>> for InputHandler<'a> {
    type Item = ();

    fn handle_graph_resolution(
        &mut self,
        _ingot_url: &Url,
        graph: DiGraph<Url, EdgeWeight>,
    ) -> Self::Item {
        // Update the graph singleton in the database with the resolved graph
        self.db
            .graph()
            .join_graph(self.db, graph, take(&mut self.join_edges));
    }
}
