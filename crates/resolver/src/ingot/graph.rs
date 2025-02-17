use core::panic;
use std::mem::take;

use camino::{Utf8Path, Utf8PathBuf};
use common::indexmap::{IndexMap, IndexSet};
use glob::glob;
use petgraph::dot;
use petgraph::visit::EdgeRef;
use petgraph::{
    dot::Dot,
    graph::{DiGraph, NodeIndex},
    Direction,
};
use smol_str::SmolStr;

use crate::Resolver;

use super::config::{self, Config, ConfigResolver, FilesDescription};

#[derive(Debug, Default)]
pub struct Graph {
    pub local_paths: IndexSet<Utf8PathBuf>,
    pub configs: IndexMap<Utf8PathBuf, Config>,
    graph: DiGraph<Utf8PathBuf, SmolStr>,
    nodes: IndexMap<Utf8PathBuf, NodeIndex>,
}

impl Graph {
    pub fn reverse_toposort(&self) -> Vec<Utf8PathBuf> {
        petgraph::algo::toposort(&self.graph, None)
            .unwrap()
            .into_iter()
            .map(|node| self.graph[node].clone())
            .rev()
            .collect()
    }

    pub fn dependencies(&self, path: &Utf8Path) -> IndexMap<SmolStr, Utf8PathBuf> {
        let node = self.nodes[path];

        self.graph
            .edges_directed(node, Direction::Outgoing)
            .map(|edge| {
                (
                    edge.weight().clone(),
                    self.graph.node_weight(edge.target()).unwrap().clone(),
                )
            })
            .collect()
    }

    pub fn dependents(&self, path: &Utf8Path) -> IndexMap<SmolStr, Utf8PathBuf> {
        let node = self.nodes[path];
        self.graph
            .edges_directed(node, Direction::Incoming)
            .map(|edge| {
                (
                    edge.weight().clone(),
                    self.graph.node_weight(edge.target()).unwrap().clone(),
                )
            })
            .collect()
    }

    pub fn contains(&self, path: &Utf8PathBuf) -> bool {
        self.nodes.contains_key(path)
    }

    pub fn add_dependency(&mut self, name: SmolStr, source: &Utf8Path, target: &Utf8Path) {
        let source_node = self.node_index(source);
        let target_node = self.node_index(target);

        self.graph.add_edge(source_node, target_node, name);
    }

    pub fn dot(&self) -> String {
        format!(
            "{:?}",
            Dot::with_config(&self.graph, &[dot::Config::EdgeNoLabel])
        )
    }

    fn node_index(&mut self, path: &Utf8Path) -> NodeIndex {
        let path = path.canonicalize_utf8().unwrap();
        if !self.contains(&path) {
            let node = self.graph.add_node(path.clone());
            self.nodes.insert(path, node);
            node
        } else {
            self.nodes[&path]
        }
    }
}

#[derive(Default)]
pub struct GraphResolver {
    config_resolver: ConfigResolver,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
pub enum Error {
    PathDoesNotExist,
}

#[derive(Debug)]
pub enum Diagnostic {
    TargetPathDoesNotExist {
        source_path: Utf8PathBuf,
        target_path: Utf8PathBuf,
    },
    TargetConfigResolutionError(Utf8PathBuf, config::Error),
    LocalConfigResolutionError(Utf8PathBuf, config::Error),
}

impl Resolver for GraphResolver {
    type Description = Utf8PathBuf;
    type Resource = Graph;
    type Error = Error;
    type Diagnostic = Diagnostic;

    fn resolve(&mut self, path: &Utf8PathBuf) -> Result<Graph, Error> {
        let path = path.canonicalize_utf8().unwrap();

        let mut graph = Graph::default();

        let glob_path = path.join("**/fe.toml");

        let local_configs: IndexMap<_, _> = glob(glob_path.as_str())
            .expect("failed to read glob pattern")
            .filter_map(|entry| {
                let path = Utf8PathBuf::from_path_buf(entry.unwrap().to_path_buf())
                    .unwrap()
                    .canonicalize_utf8()
                    .unwrap();
                match self.config_resolver.resolve(&path) {
                    Ok(config) => Some((path.parent().unwrap().to_path_buf(), config)),
                    Err(error) => {
                        self.diagnostics
                            .push(Diagnostic::LocalConfigResolutionError(path, error));
                        None
                    }
                }
            })
            .collect();

        let mut unresolved_dependencies = vec![];

        for (ingot_path, config) in local_configs.iter() {
            graph.local_paths.insert(ingot_path.clone());
            if let Some(dependencies) = &config.dependencies {
                unresolved_dependencies.push((ingot_path.clone(), dependencies.clone()));
            }
        }

        graph.configs = local_configs;

        while let Some((source_path, targets)) = unresolved_dependencies.pop() {
            for (target_name, target_description) in targets {
                let target_path = match target_description.files {
                    FilesDescription::Path(path) => {
                        source_path.join(path).canonicalize_utf8().unwrap()
                    }
                };

                if !target_path.exists() {
                    self.diagnostics.push(Diagnostic::TargetPathDoesNotExist {
                        source_path: source_path.clone(),
                        target_path: target_path.clone(),
                    });
                    continue;
                }

                if !graph.contains(&target_path) {
                    match self.config_resolver.resolve(&target_path) {
                        Ok(config) => {
                            if let Some(dependencies) = config.dependencies.clone() {
                                unresolved_dependencies.push((target_path.clone(), dependencies))
                            }
                            graph.configs.insert(source_path.clone(), config);
                        }
                        Err(error) => {
                            self.diagnostics
                                .push(Diagnostic::TargetConfigResolutionError(
                                    target_path.clone(),
                                    error,
                                ))
                        }
                    }
                }

                graph.add_dependency(target_name, &source_path, &target_path);
            }
        }

        Ok(graph)
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        take(&mut self.diagnostics)
    }
}
