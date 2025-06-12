use std::error::Error;
use std::{hash::Hash, marker::PhantomData, mem::take};

use indexmap::IndexMap;
pub use petgraph::graph::{DiGraph, NodeIndex};

pub use petgraph;

use crate::{ResolutionHandler, Resolver};

pub trait GraphResolver<NR, NH, E>: Resolver {}

impl<NR, NH, E> GraphResolver<NR, NH, E> for GraphResolverImpl<NR, NH, E>
where
    NR: Resolver,
    NH: ResolutionHandler<NR>,
    NH::Item: IntoIterator<Item = (NR::Description, E)>,
    NR::Description: Eq + std::hash::Hash + Clone,
    E: Clone,
{
}

impl<NR, NH, E> Default for GraphResolverImpl<NR, NH, E>
where
    NR: Resolver + Default,
    NH: Default,
{
    fn default() -> Self {
        Self {
            node_resolver: NR::default(),
            node_handler: NH::default(),
            diagnostics: vec![],
            _edge: PhantomData,
        }
    }
}

pub struct GraphResolverImpl<NR: Resolver, NH, E> {
    pub node_resolver: NR,
    pub node_handler: NH,
    pub diagnostics: Vec<UnresolvableNode<NR::Description, NR::Error>>,
    pub _edge: PhantomData<E>,
}

impl<NR, NH, E> GraphResolverImpl<NR, NH, E>
where
    NR: Resolver,
{
    pub fn new(node_resolver: NR, node_handler: NH) -> Self {
        Self {
            node_resolver,
            node_handler,
            diagnostics: vec![],
            _edge: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct UnresolvableNode<N, E>(pub N, pub E);

#[derive(Debug)]
pub struct UnresolvableRootNode;

impl<NR, NH, E> Resolver for GraphResolverImpl<NR, NH, E>
where
    NR: Resolver,
    NH: ResolutionHandler<NR>,
    NH::Item: IntoIterator<Item = (NR::Description, E)>,
    NR::Description: Eq + Hash + Clone,
    E: Clone,
{
    type Description = NR::Description;
    type Resource = DiGraph<NR::Description, E>;
    type Diagnostic = UnresolvableNode<NR::Description, NR::Error>;
    type Error = UnresolvableRootNode;

    fn transient_resolve(
        &mut self,
        root_node: &Self::Description,
    ) -> Result<Self::Resource, Self::Error> {
        let mut graph = DiGraph::default();
        let mut nodes: IndexMap<Self::Description, NodeIndex> = IndexMap::new();
        let mut unresolved_nodes: IndexMap<Self::Description, Vec<(NodeIndex, E)>> =
            IndexMap::new();
        let mut unresolvable_nodes: IndexMap<Self::Description, Vec<(NodeIndex, E)>> =
            IndexMap::new();

        unresolved_nodes.entry(root_node.clone()).or_default();

        while let Some((unresolved_node_description, back_nodes)) = unresolved_nodes.pop() {
            match self
                .node_resolver
                .resolve(&mut self.node_handler, &unresolved_node_description)
            {
                Ok(forward_nodes) => {
                    let resolved_node_description = unresolved_node_description;

                    let resolved_node_index = graph.add_node(resolved_node_description.clone());
                    nodes.insert(resolved_node_description.clone(), resolved_node_index);

                    for (back_node_index, back_edge) in &back_nodes {
                        graph.add_edge(*back_node_index, resolved_node_index, back_edge.clone());
                    }

                    for (forward_node_description, forward_edge) in forward_nodes {
                        if unresolvable_nodes.contains_key(&forward_node_description) {
                            unresolvable_nodes
                                .entry(forward_node_description)
                                .or_default()
                                .push((resolved_node_index, forward_edge));
                        } else if !nodes.contains_key(&forward_node_description) {
                            unresolved_nodes
                                .entry(forward_node_description)
                                .or_default()
                                .push((resolved_node_index, forward_edge));
                        } else if let Some(&existing_index) = nodes.get(&forward_node_description) {
                            graph.add_edge(resolved_node_index, existing_index, forward_edge);
                        }
                    }
                }
                Err(error) => {
                    self.diagnostics
                        .push(UnresolvableNode(unresolved_node_description.clone(), error));
                    unresolvable_nodes
                        .entry(unresolved_node_description)
                        .or_default()
                        .extend(back_nodes);
                }
            }
        }

        if graph.node_count() == 0 {
            Err(UnresolvableRootNode)
        } else {
            Ok(graph)
        }
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        take(&mut self.diagnostics)
    }
}
