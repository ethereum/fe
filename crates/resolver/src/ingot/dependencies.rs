use std::fmt::Debug;

use crate::path::PathDescription;
use crate::remote::GitDescription;
use crate::Resolver;
use camino::Utf8PathBuf;
use indexmap::IndexMap;
use petgraph::csr::NodeIndex;
use petgraph::graph::DiGraph;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use smol_str::SmolStr;

use super::config::{ConfigResolutionError, ConfigResolver};
use super::AnyIngotDescription;

#[derive(Debug)]
pub enum DependenciesResolutionError {
    RootConfigResolutionError(ConfigResolutionError),
}

pub struct DependencyGraph {
    root_path: Utf8PathBuf,
    graph: DiGraph<Utf8PathBuf, (SmolStr, AnyIngotDescription)>,
    nodes: IndexMap<Utf8PathBuf, NodeIndex>,
}

impl DependencyGraph {
    pub fn new(root_path: Utf8PathBuf) -> Self {
        // let mut graph = DiGraph::new();
        // let node_idx: NodeIndex = graph.add_node(root_path.clone());
        // let mut nodes: IndexMap<Utf8PathBuf, NodeIndex> = IndexMap::new();
        // nodes.insert(local_path.clone(), node_idx);

        // Self {
        //     root_path,
        //     graph,
        //     nodes,
        // }
        todo!()
    }

    pub fn reverse_toposort(&self) -> Vec<Utf8PathBuf> {
        todo!()
    }

    pub fn dependencies(
        &self,
        path: &Utf8PathBuf,
    ) -> IndexMap<SmolStr, (AnyIngotDescription, Utf8PathBuf)> {
        let node = NodeIndex::from(self.nodes[path]);
        self.graph
            .edges_directed(node, Direction::Outgoing)
            .map(|edge| {
                let (name, description) = edge.weight();
                (
                    name.to_owned(),
                    (
                        description.to_owned(),
                        self.graph.node_weight(edge.target()).unwrap().to_owned(),
                    ),
                )
            })
            .collect()
    }

    pub fn contains(&self, path: &Utf8PathBuf) -> bool {
        self.nodes.contains_key(path)
    }

    pub fn add_dependency(
        &mut self,
        name: SmolStr,
        description: AnyIngotDescription,
        from: Utf8PathBuf,
        to: Utf8PathBuf,
    ) {
        // self.graph.add_edge(
        //     *self.nodes.get(&from).unwrap(),
        //     *self.nodes.get(&to).unwrap(),
        //     (name, description),
        // );
        todo!()
    }
}

pub struct DependenciesResolver {
    pub config_resolver: ConfigResolver,
}

#[derive(Clone)]
enum DependencyPath {
    Local(Utf8PathBuf, Vec<PathDescription>),
    Remote(GitDescription, Vec<PathDescription>),
}

impl DependencyPath {
    pub fn new_remote(remote_description: GitDescription) -> Self {
        Self::Remote(remote_description, vec![])
    }

    pub fn new_local(root_path: Utf8PathBuf) -> Self {
        Self::Local(root_path, vec![])
    }

    pub fn join(&mut self, path_description: PathDescription) {
        match self {
            DependencyPath::Local(_, paths) => paths,
            DependencyPath::Remote(_, paths) => paths,
        }
        .push(path_description);
    }

    pub fn fs_path(&self) -> Result<Utf8PathBuf, String> {
        // match self {
        //     DependencyPath::Local(_, paths) => paths
        //         .iter()
        //         .fold(Utf8PathBuf::new(), |acc, path| acc.join(path)),
        //     DependencyPath::Remote(git_desc, paths) => paths
        //         .iter()
        //         .fold(Utf8PathBuf::new(), |acc, path| acc.join(path)),
        // }
        todo!()
    }
}

struct UnresolvedDependency {
    name: SmolStr,
    ingot_description: AnyIngotDescription,
    source: (Utf8PathBuf, DependencyPath),
}

impl Resolver for DependenciesResolver {
    type Description = Utf8PathBuf;
    type Resource = DependencyGraph;
    type ResolutionError = DependenciesResolutionError;

    fn resolve(
        &self,
        description: &Utf8PathBuf,
    ) -> Result<DependencyGraph, DependenciesResolutionError> {
        let root_config = self
            .config_resolver
            .resolve(description)
            .map_err(DependenciesResolutionError::RootConfigResolutionError)?;
        let mut graph = DependencyGraph::new(description.to_owned());
        let mut unresolved_dependencies = root_config
            .dependencies
            .into_iter()
            .map(|(name, ingot_description)| UnresolvedDependency {
                name,
                ingot_description,
                source: (
                    description.clone(),
                    DependencyPath::new_local(description.clone()),
                ),
            })
            .collect::<Vec<_>>();

        while let Some(dependency) = unresolved_dependencies.pop() {
            let target = match dependency.ingot_description.clone() {
                AnyIngotDescription::Path(ingot_description) => {
                    dependency
                        .source
                        .1
                        .clone()
                        .join(ingot_description.files_description);
                    dependency.source.1
                }
                AnyIngotDescription::Remote(ingot_description) => {
                    DependencyPath::new_remote(ingot_description.files_description)
                }
                AnyIngotDescription::Registered(_) => todo!(),
            };
            let target_path = target.fs_path().unwrap();

            if !graph.contains(&target_path) {
                unresolved_dependencies.append(
                    &mut self
                        .config_resolver
                        .resolve(&target_path)
                        .unwrap()
                        .dependencies
                        .into_iter()
                        .map(|(name, ingot_description)| UnresolvedDependency {
                            name,
                            ingot_description,
                            source: (target_path.clone(), target.clone()),
                        })
                        .collect::<Vec<_>>(),
                )
            }

            // add a new edge to the dependency graph
            graph.add_dependency(
                dependency.name,
                dependency.ingot_description,
                dependency.source.0,
                target_path,
            );
        }

        // finally, return the graph with all nodes and edges
        Ok(graph)
    }
}

struct DependencyPathResolver {
    root_path: Utf8PathBuf,
}
