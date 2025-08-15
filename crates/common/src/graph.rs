use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::{Dfs, EdgeRef};
use salsa::Setter;
use smol_str::SmolStr;
use std::collections::HashMap;
use url::Url;

use crate::{config::DependencyArguments, InputDb};

#[salsa::input]
#[derive(Debug)]
pub struct Graph {
    graph: DiGraph<Url, EdgeWeight>,
    node_map: HashMap<Url, NodeIndex>,
}

#[derive(Debug, Clone)]
pub struct JoinEdge {
    pub origin: Url,
    pub destination: Url,
    pub weight: EdgeWeight,
}

#[derive(Clone, Debug)]
pub struct EdgeWeight {
    pub alias: SmolStr,
    pub arguments: DependencyArguments,
}

#[salsa::tracked]
impl Graph {
    pub fn default(db: &dyn InputDb) -> Self {
        Graph::new(db, DiGraph::new(), HashMap::new())
    }

    pub fn contains_url(&self, db: &dyn InputDb, url: &Url) -> bool {
        self.node_map(db).contains_key(url)
    }

    pub fn cycles(&self, db: &dyn InputDb) -> Vec<Vec<Url>> {
        use petgraph::algo::tarjan_scc;
        let graph = self.graph(db);
        let sccs = tarjan_scc(&graph);

        sccs.into_iter()
            .filter(|scc| scc.len() > 1) // Only cycles with multiple nodes
            .map(|scc| {
                scc.into_iter()
                    .map(|node_idx| graph[node_idx].clone())
                    .collect()
            })
            .collect()
    }

    pub fn join_graph(
        &self,
        db: &mut dyn InputDb,
        other: DiGraph<Url, EdgeWeight>,
        join_edges: Vec<JoinEdge>,
    ) {
        let current_graph = self.graph(db);
        let combined_graph = join_graphs(&current_graph, &other, &join_edges);

        // Build new node map
        let mut new_node_map = HashMap::new();
        for node_idx in combined_graph.node_indices() {
            let url = &combined_graph[node_idx];
            new_node_map.insert(url.clone(), node_idx);
        }

        self.set_graph(db).to(combined_graph);
        self.set_node_map(db).to(new_node_map);
    }

    pub fn dependency_urls(&self, db: &dyn InputDb, url: &Url) -> Vec<Url> {
        let node_map = self.node_map(db);
        let graph = self.graph(db);

        if let Some(&root) = node_map.get(url) {
            let mut dfs = Dfs::new(&graph, root);
            let mut visited = Vec::new();

            while let Some(node) = dfs.next(&graph) {
                if node != root {
                    visited.push(graph[node].clone());
                }
            }

            visited
        } else {
            Vec::new()
        }
    }

    pub fn add_node(&self, db: &mut dyn InputDb, url: Url) -> NodeIndex {
        let mut graph = self.graph(db);
        let mut node_map = self.node_map(db);

        if let Some(&existing_idx) = node_map.get(&url) {
            return existing_idx;
        }

        let new_idx = graph.add_node(url.clone());
        node_map.insert(url, new_idx);

        self.set_graph(db).to(graph);
        self.set_node_map(db).to(node_map);

        new_idx
    }

    pub fn add_edge(&self, db: &mut dyn InputDb, from: &Url, to: &Url, weight: EdgeWeight) -> bool {
        let node_map = self.node_map(db);

        if let (Some(&from_idx), Some(&to_idx)) = (node_map.get(from), node_map.get(to)) {
            let mut graph = self.graph(db);
            graph.add_edge(from_idx, to_idx, weight);
            self.set_graph(db).to(graph);
            true
        } else {
            false
        }
    }
}

fn join_graphs(
    g1: &DiGraph<Url, EdgeWeight>,
    g2: &DiGraph<Url, EdgeWeight>,
    join_edges: &[JoinEdge],
) -> DiGraph<Url, EdgeWeight> {
    let mut combined = DiGraph::new();
    let mut node_map = HashMap::<Url, NodeIndex>::new();

    // Insert g1 nodes
    for idx in g1.node_indices() {
        let url = &g1[idx];
        if !node_map.contains_key(url) {
            let new_idx = combined.add_node(url.clone());
            node_map.insert(url.clone(), new_idx);
        }
    }

    // Insert g2 nodes (avoid duplicates)
    for idx in g2.node_indices() {
        let url = &g2[idx];
        if !node_map.contains_key(url) {
            let new_idx = combined.add_node(url.clone());
            node_map.insert(url.clone(), new_idx);
        }
    }

    // Insert g1 edges
    for edge in g1.edge_references() {
        let source_url = &g1[edge.source()];
        let target_url = &g1[edge.target()];
        if let (Some(&from), Some(&to)) = (node_map.get(source_url), node_map.get(target_url)) {
            combined.add_edge(from, to, edge.weight().clone());
        }
    }

    // Insert g2 edges
    for edge in g2.edge_references() {
        let source_url = &g2[edge.source()];
        let target_url = &g2[edge.target()];
        if let (Some(&from), Some(&to)) = (node_map.get(source_url), node_map.get(target_url)) {
            combined.add_edge(from, to, edge.weight().clone());
        }
    }

    // Insert join edges (connecting g1 and g2)
    for join_edge in join_edges {
        if let (Some(&from), Some(&to)) = (
            node_map.get(&join_edge.origin),
            node_map.get(&join_edge.destination),
        ) {
            combined.add_edge(from, to, join_edge.weight.clone());
        }
    }

    combined
}
