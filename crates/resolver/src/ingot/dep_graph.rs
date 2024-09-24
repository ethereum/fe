use std::fmt::Debug;
use std::hash::Hash;

use crate::Resolver;
use indexmap::{IndexMap, IndexSet};
use petgraph::graph::{DiGraph, NodeIndex};
use serde::de::DeserializeOwned;

use super::config::ConfigResolver;
use super::IngotDesc;

#[derive(Debug)]
struct DepGraphResolutionError;

struct DepGraph<FD>
where
    FD: Hash,
{
    graph: DiGraph<IngotDesc<FD>, ()>,
    node_map: IndexMap<IngotDesc<FD>, NodeIndex>,
}

struct DepGraphResolver<FR> {
    config_resolver: ConfigResolver<FR>,
}

impl<FR> Resolver for DepGraphResolver<FR>
where
    FR: Resolver,
    FR::ResourceDesc: Hash + Clone + Eq + Debug,
    FR::Resource: DeserializeOwned,
{
    type Config = ();
    type ResourceDesc = IngotDesc<FR::ResourceDesc>;
    type Resource = DepGraph<FR::ResourceDesc>;
    type ResolutionError = DepGraphResolutionError;

    fn from_config(_config: &Self::Config) -> Self {
        todo!()
    }

    fn resolve(&self, desc: &Self::ResourceDesc) -> Result<Self::Resource, Self::ResolutionError> {
        let mut graph = DiGraph::new();
        let mut node_map = IndexMap::new();
        let mut visited = IndexSet::new();
        let mut stack = vec![desc.clone()];

        while let Some(current_desc) = stack.pop() {
            if visited.contains(&current_desc) {
                continue;
            }

            visited.insert(current_desc.clone());

            let current_node = node_map
                .entry(current_desc.clone())
                .or_insert_with(|| graph.add_node(current_desc.clone()));

            let current_config = self
                .config_resolver
                .resolve(&current_desc)
                .map_err(|_| DepGraphResolutionError)?;

            for (name, desc) in current_config.dependencies {
                stack.push(desc);
            }
        }

        Ok(DepGraph { graph, node_map })
    }
}
