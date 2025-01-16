use std::{mem::take, str::FromStr};

use camino::Utf8PathBuf;
use common::home_dir::HomeDir;
use indexmap::IndexMap;
use petgraph::{
    dot::{Config, Dot},
    graph::{DiGraph, NodeIndex},
    Direction,
};

use crate::{path::FullPathDescription, Resolver};

use super::{
    config::{ConfigResolutionDiagnostic, ConfigResolutionError, ConfigResolver},
    dependency::{
        Dependency, DependencyDescription, DependencyResolutionError, DependencyResolver,
    },
};

#[derive(Debug)]
pub struct DependencyGraph {
    pub local_path: Utf8PathBuf,
    graph: DiGraph<Utf8PathBuf, Dependency>,
    nodes: IndexMap<Utf8PathBuf, NodeIndex>,
    local_dependencies: Vec<DependencyDescription>,
}

impl DependencyGraph {
    pub fn new(local_path: &Utf8PathBuf) -> Self {
        let local_path = local_path.canonicalize_utf8().unwrap();
        let mut graph = DiGraph::new();
        let local_node: NodeIndex = graph.add_node(local_path.clone());
        let mut nodes = IndexMap::new();
        nodes.insert(local_path.clone(), local_node);

        Self {
            local_path,
            graph,
            nodes,
            local_dependencies: vec![],
        }
    }

    pub fn reverse_toposort(&self) -> Vec<Utf8PathBuf> {
        petgraph::algo::toposort(&self.graph, None)
            .unwrap()
            .into_iter()
            .map(|node| self.graph[node].clone())
            .rev()
            .collect()
    }

    pub fn dependencies(&self, path: &Utf8PathBuf) -> Vec<Dependency> {
        let node = self.nodes[path];
        self.graph
            .edges_directed(node, Direction::Outgoing)
            .map(|edge| edge.weight().clone())
            .collect()
    }

    pub fn contains(&self, path: &Utf8PathBuf) -> bool {
        self.nodes.contains_key(path)
    }

    pub fn add_dependency(&mut self, depedency: Dependency) {
        let source_node = self.node_index(&depedency.source_path);
        let target_node = self.node_index(&depedency.target_path);

        self.graph.add_edge(source_node, target_node, depedency);
    }

    pub fn dot(&self) -> String {
        format!(
            "{:?}",
            Dot::with_config(&self.graph, &[Config::EdgeNoLabel])
        )
    }

    fn node_index(&mut self, path: &Utf8PathBuf) -> NodeIndex {
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

pub struct DependencyGraphResolver {
    config_resolver: ConfigResolver,
    dependency_resolver: DependencyResolver,
    diagnostics: Vec<DependencyGraphResolutionDiagnostic>,
}

impl DependencyGraphResolver {
    pub fn new() -> Self {
        Self {
            config_resolver: ConfigResolver::new(),
            dependency_resolver: DependencyResolver::new(),
            diagnostics: vec![],
        }
    }
}

#[derive(Debug)]
pub enum DependencyGraphResolutionError {
    LocalPathDoesNotExist,
    LocalConfigResolutionError(ConfigResolutionError),
}

pub enum DependencyGraphResolutionDiagnostic {
    DependencyResolutonError(DependencyDescription, DependencyResolutionError),
    ConfigResolutionDiagnostic(ConfigResolutionDiagnostic),
}

impl Resolver for DependencyGraphResolver {
    type Description = Utf8PathBuf;
    type Resource = DependencyGraph;
    type Error = DependencyGraphResolutionError;
    type Diagnostic = DependencyGraphResolutionDiagnostic;

    fn resolve(
        &mut self,
        local_path: &Utf8PathBuf,
    ) -> Result<DependencyGraph, DependencyGraphResolutionError> {
        if !local_path.exists() {
            Err(DependencyGraphResolutionError::LocalPathDoesNotExist)
        } else {
            let mut graph = DependencyGraph::new(local_path);

            let local_config = self
                .config_resolver
                .resolve(local_path)
                .map_err(DependencyGraphResolutionError::LocalConfigResolutionError)?;
            let local_path_description = FullPathDescription::new_local(local_path);
            graph.local_dependencies =
                local_config.dependency_descriptions(local_path, &local_path_description);

            let mut unresolved_dependencies = graph.local_dependencies.clone();

            while let Some(dependency_description) = unresolved_dependencies.pop() {
                match self.dependency_resolver.resolve(&dependency_description) {
                    Ok(dependency) => {
                        if !graph.contains(&dependency.target_path) {
                            unresolved_dependencies.append(&mut dependency.sub_dependencies.clone())
                        }

                        graph.add_dependency(dependency);
                    }
                    Err(error) => self.diagnostics.push(
                        DependencyGraphResolutionDiagnostic::DependencyResolutonError(
                            dependency_description,
                            error,
                        ),
                    ),
                }
            }

            Ok(graph)
        }
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        // self.diagnostics.append(
        //     &mut self
        //         .config_resolver
        //         .take_diagnostics()
        //         .into_iter()
        //         .map(|diag| DependencyGraphResolutionDiagnostic::ConfigResolutionDiagnostic(diag))
        //         .collect(),
        // );

        // self.diagnostics.append(
        //     &mut self
        //         .user_config_resolver
        //         .take_diagnostics()
        //         .into_iter()
        //         .map(|diag| DependencyGraphResolutionDiagnostic::ConfigResolutionDiagnostic(diag))
        //         .collect(),
        // );

        // self.diagnostics.append(
        //     &mut self
        //         .dependency_resolver
        //         .take_diagnostics()
        //         .into_iter()
        //         .map(|diag| DependencyGraphResolutionDiagnostic::ConfigResolutionDiagnostic(diag))
        //         .collect(),
        // );

        take(&mut self.diagnostics)
    }
}
