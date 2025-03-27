use std::collections::{HashMap, HashSet};

use camino::Utf8PathBuf;
use common::config::{Config, IngotArguments};
use resolver::{
    graph::{petgraph, DiGraph, NodeIndex},
    ingot::basic_ingot_graph_resolver,
    Resolver,
};
use smol_str::SmolStr;

pub fn print_tree(path: &Utf8PathBuf) {
    let mut graph_resolver = basic_ingot_graph_resolver();
    let ingot_graph = graph_resolver
        .transient_resolve(&path.canonicalize_utf8().unwrap())
        .unwrap();
    print!(
        "{}",
        print_tree_impl(
            &ingot_graph,
            &path.canonicalize_utf8().unwrap(),
            &graph_resolver.node_handler.configs
        )
    );
}

#[derive(Clone)]
enum TreePrefix {
    Root,
    Fork(String),
    Last(String),
}

impl TreePrefix {
    fn new_prefix(&self) -> String {
        match self {
            TreePrefix::Root => "".to_string(),
            TreePrefix::Fork(p) => format!("{}├── ", p),
            TreePrefix::Last(p) => format!("{}└── ", p),
        }
    }

    fn child_indent(&self) -> String {
        match self {
            TreePrefix::Root => "".to_string(),
            TreePrefix::Fork(p) => format!("{}│   ", p),
            TreePrefix::Last(p) => format!("{}    ", p),
        }
    }
}

pub fn print_tree_impl(
    graph: &DiGraph<Utf8PathBuf, (SmolStr, IngotArguments)>,
    root_path: &Utf8PathBuf,
    configs: &HashMap<Utf8PathBuf, Config>,
) -> String {
    let mut output = String::new();

    let cycle_nodes = find_cycle_nodes(graph);

    if let Some(root_idx) = graph.node_indices().find(|i| graph[*i] == *root_path) {
        let mut seen = HashSet::new();
        print_node(
            graph,
            root_idx,
            TreePrefix::Root,
            &mut output,
            configs,
            &cycle_nodes,
            &mut seen,
        );
    } else {
        output.push_str("[error: root node not found]\n");
    }

    output
}

fn print_node(
    graph: &DiGraph<Utf8PathBuf, (SmolStr, IngotArguments)>,
    node: NodeIndex,
    prefix: TreePrefix,
    output: &mut String,
    configs: &HashMap<Utf8PathBuf, Config>,
    cycle_nodes: &HashSet<NodeIndex>,
    seen: &mut HashSet<NodeIndex>,
) {
    let ingot_path = &graph[node];
    let base_label = if let Some(config) = configs.get(ingot_path) {
        format!(
            "{} v{}",
            config.ingot.name.as_deref().unwrap_or("null"),
            config
                .ingot
                .version
                .as_ref()
                .map(ToString::to_string)
                .unwrap_or_else(|| "null".to_string())
        )
    } else {
        "[unknown config]".to_string()
    };

    let is_in_cycle = cycle_nodes.contains(&node);
    let will_close_cycle = seen.contains(&node);

    let mut label = base_label.clone();
    if will_close_cycle {
        label = format!("{} [cycle]", label);
    }

    if is_in_cycle {
        output.push_str(&format!("{}{}\n", prefix.new_prefix(), red(&label)));
    } else {
        output.push_str(&format!("{}{}\n", prefix.new_prefix(), label));
    }

    if will_close_cycle {
        return;
    }

    seen.insert(node);

    let children: Vec<_> = graph
        .neighbors_directed(node, petgraph::Direction::Outgoing)
        .collect();

    for (i, child) in children.iter().enumerate() {
        let child_prefix = if i == children.len() - 1 {
            TreePrefix::Last(prefix.child_indent())
        } else {
            TreePrefix::Fork(prefix.child_indent())
        };

        print_node(
            graph,
            *child,
            child_prefix,
            output,
            configs,
            cycle_nodes,
            seen,
        );
    }

    seen.remove(&node);
}

fn red(s: &str) -> String {
    format!("\x1b[31m{}\x1b[0m", s)
}

fn find_cycle_nodes<N, E>(graph: &DiGraph<N, E>) -> HashSet<NodeIndex> {
    use petgraph::algo::kosaraju_scc;

    let mut cycles = HashSet::new();
    for scc in kosaraju_scc(graph) {
        if scc.len() > 1 {
            cycles.extend(scc);
        } else {
            let node = scc[0];
            if graph
                .neighbors_directed(node, petgraph::Direction::Outgoing)
                .any(|n| n == node)
            {
                cycles.insert(node); // self-loop
            }
        }
    }
    cycles
}
