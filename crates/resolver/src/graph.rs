use indexmap::{IndexMap, IndexSet};
use petgraph::{data::Build, graph::DiGraph};

use crate::{ResolutionHandler, Resolver};

pub trait GraphResolver<NR, NH>: Resolver {}

impl<NR, NH> GraphResolver<NR, NH> for GraphResolverImpl<NR, NH>
where
    NR: Resolver,
    NH: ResolutionHandler<NR>,
    NH::Item: GraphNode<NR::Description>,
{
}

impl<NR, NH> Default for GraphResolverImpl<NR, NH>
where
    NR: Default,
    NH: Default,
{
    fn default() -> Self {
        Self {
            node_resolver: NR::default(),
            node_handler: NH::default(),
        }
    }
}

pub struct GraphResolverImpl<NR, NH> {
    pub node_resolver: NR,
    pub node_handler: NH,
}

impl<NR, NH> GraphResolverImpl<NR, NH> {
    pub fn new(node_resolver: NR, node_handler: NH) -> Self {
        Self {
            node_resolver,
            node_handler,
        }
    }
}

impl<NR, NH> Resolver for GraphResolverImpl<NR, NH>
where
    NR: Resolver,
    NH: ResolutionHandler<NR>,
    NH::Item: GraphNode<NR::Description>,
{
    type Description = NR::Description;
    type Resource = DiGraph<NR::Description, ()>;
    type Diagnostic = ();
    type Error = ();

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        root_node: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: crate::ResolutionHandler<Self>,
    {
        // A graph of resolved nodes.
        let mut graph = DiGraph::default();
        let mut nodes = IndexMap::default();

        // Nodes that have not yet been resolved and their resolved back nodes.
        let mut unresolved_nodes = IndexMap::default();

        // Nodes that could not be resolved and their resolved back nodes.
        let mut unresolvable_nodes = IndexMap::default();

        while let Some((unresolved_node, back_nodes)) = unresolved_nodes.pop() {
            match self
                .node_resolver
                .resolve(&mut self.node_handler, unresolved_node)
            {
                Ok(resolved_node) => {
                    nodes[resolved_node] = graph.add_node(resolved_node);

                    for back_node in back_nodes {
                        graph.add_edge(back_node, resolved_node, ());
                    }

                    for forward_node in resolved_node.forward_ids() {
                        // There are three cases:
                        // 1.) `forward_node` is in the `unresolvable_nodes` map. In this case we
                        //   insert `resolved_node` into `unresolvable_nodes[forward_node]`.
                        // 2.) `forward_node` is not in `graph`. In this case we insert
                        //   `resolved_node` into `unresolved_nodes[forward_node]`.
                        // 3.) `forward_node` is already in the graph. In this case we insert the
                        //   edge `(resolved_node, forward_node)` into the graph.

                        if unresolvable_nodes.contains(forward_node) {
                            unresolvable_nodes.get(forward_node).insert(resolved_node)
                        } else if !graph.contains(forward_node) {
                            unresolved_nodes.insert(forward_node);
                        } else {
                            graph.insert_edge(unresolved_node, forward_node);
                        }
                    }
                }
                Err(_) => {
                    // todo: handle node resolution error as graph resolution diagnostic

                    unresolvable_nodes[unresolved_node].insert(back_nodes);
                }
            }
        }

        // todo: error if the graph is empty. failed to resolve root node
        Ok(handler.handle_resolution(root_node, graph))
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        todo!()
    }
}

pub trait GraphNode<Id> {
    fn id(&self) -> Id;
    fn forward_ids(&self) -> IndexSet<Id>;
}
