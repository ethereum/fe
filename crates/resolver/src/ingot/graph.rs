use core::panic;
use std::env::current_dir;
use std::mem::take;
use std::{fs, io};

use camino::{Utf8Path, Utf8PathBuf};
use common::indexmap::{IndexMap, IndexSet};
use glob::glob;
use petgraph::visit::EdgeRef;
use petgraph::{dot, graph};
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
    DepenendencyTargetPathDoesNotExist {
        source_path: Utf8PathBuf,
        target_path: Utf8PathBuf,
    },
    ConfigResolutionError(Utf8PathBuf, config::Error),
}

impl Resolver for GraphResolver {
    type Description = Utf8PathBuf;
    type Resource = Graph;
    type Error = Error;
    type Diagnostic = Diagnostic;

    fn resolve(&mut self, path: &Utf8PathBuf) -> Result<Graph, Error> {
        if !path.exists() {
            Err(Error::PathDoesNotExist)
        } else {
            // let local_path = path.canonicalize_utf8().unwrap();

            let mut graph = Graph::default();

            // let local_config = self
            //     .config_resolver
            // .resolve(&path)
            // .map_err(Error::ConfigResolutionError)?;

            let glob_path = path.join("**/fe.toml");
            // panic!("{}", glob_path.as_str());
            // panic!("{:?}", current_dir().unwrap());

            let local_configs: IndexMap<Utf8PathBuf, Config> = glob(glob_path.as_str())
                .expect("failed to read glob pattern")
                .filter_map(|entry| match entry {
                    Ok(path) => match Utf8PathBuf::from_path_buf(path.to_path_buf()) {
                        Ok(path) => match self.config_resolver.resolve(&path) {
                            Ok(config) => Some((path.parent().unwrap().to_path_buf(), config)),
                            Err(error) => {
                                panic!("{}", error);
                                self.diagnostics
                                    .push(Diagnostic::ConfigResolutionError(path, error));
                                None
                            }
                        },
                        Err(path) => {
                            // self.diagnostics.push(Diagnostic::NonUtf8Path(path));
                            todo!();
                            None
                        }
                    },
                    Err(error) => {
                        // self.diagnostics.push(Diagnostic::GlobError(error));
                        panic!("{error}");
                        todo!();
                        None
                    }
                })
                .collect();
            // panic!("{:?}", local_configs);
            //
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
                    // add check for missing path
                    let target_path = match target_description.files {
                        FilesDescription::Path(path) => {
                            source_path.join(path).canonicalize_utf8().unwrap()
                        }
                    };

                    if !target_path.exists() {
                        self.diagnostics
                            .push(Diagnostic::DepenendencyTargetPathDoesNotExist {
                                source_path: source_path.clone(),
                                target_path: target_path.clone(),
                            });
                        continue;
                    }

                    if !graph.contains(&target_path) {
                        match self.config_resolver.resolve(&target_path) {
                            Ok(config) => {
                                if let Some(dependencies) = config.dependencies {
                                    unresolved_dependencies
                                        .push((target_path.clone(), dependencies))
                                }
                            }
                            Err(err) => panic!("{} {}", err, target_path),
                        }
                    }

                    graph.add_dependency(target_name, &source_path, &target_path);
                }
            }

            Ok(graph)
        }
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        take(&mut self.diagnostics)
    }
}
