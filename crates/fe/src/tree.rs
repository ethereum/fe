use std::collections::{HashMap, HashSet};

use camino::Utf8PathBuf;
use common::{config::Config, graph::EdgeWeight, urlext::canonical_url};
use resolver::{
    files::{FilesResource, FilesResolver},
    graph::{petgraph, DiGraph, GraphResolver, GraphResolutionHandler, NodeIndex},
    ResolutionHandler,
};
use url::Url;

pub fn print_tree(path: &Utf8PathBuf) {
    let mut graph_resolver = basic_ingot_graph_resolver();
    let mut node_handler = TreeIngotNodeHandler::default();
    let ingot_url = match canonical_url(path) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {}", path);
            return;
        }
    };

    match graph_resolver.graph_resolve(&mut node_handler, &ingot_url) {
        Ok(ingot_graph) => {
            // Report diagnostics from graph resolution first
            let graph_diagnostics = graph_resolver.take_diagnostics();
            for diagnostic in &graph_diagnostics {
                eprintln!("⚠️  {}", diagnostic);
            }

            // Report diagnostics from node handler
            let node_diagnostics = node_handler.diagnostics();
            for diagnostic in node_diagnostics {
                eprintln!("⚠️  {}", diagnostic);
            }

            // Print tree after diagnostics
            print!(
                "{}",
                print_tree_impl(&ingot_graph, &ingot_url, &node_handler.configs)
            );
        }
        Err(err) => {
            // Even if resolution failed, show available diagnostics first
            let graph_diagnostics = graph_resolver.take_diagnostics();
            for diagnostic in &graph_diagnostics {
                eprintln!("⚠️  {}", diagnostic);
            }

            let node_diagnostics = node_handler.diagnostics();
            for diagnostic in node_diagnostics {
                eprintln!("⚠️  {}", diagnostic);
            }

            println!("❌ Failed to resolve dependency tree: {err}");
        }
    }
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
            TreePrefix::Fork(p) => format!("{p}├── "),
            TreePrefix::Last(p) => format!("{p}└── "),
        }
    }

    fn child_indent(&self) -> String {
        match self {
            TreePrefix::Root => "".to_string(),
            TreePrefix::Fork(p) => format!("{p}│   "),
            TreePrefix::Last(p) => format!("{p}    "),
        }
    }
}

pub fn print_tree_impl(
    graph: &DiGraph<Url, EdgeWeight>,
    root_path: &Url,
    configs: &HashMap<Url, Config>,
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
    graph: &DiGraph<Url, EdgeWeight>,
    node: NodeIndex,
    prefix: TreePrefix,
    output: &mut String,
    configs: &HashMap<Url, Config>,
    cycle_nodes: &HashSet<NodeIndex>,
    seen: &mut HashSet<NodeIndex>,
) {
    print_node_with_alias(graph, node, prefix, output, configs, cycle_nodes, seen, None);
}

fn print_node_with_alias(
    graph: &DiGraph<Url, EdgeWeight>,
    node: NodeIndex,
    prefix: TreePrefix,
    output: &mut String,
    configs: &HashMap<Url, Config>,
    cycle_nodes: &HashSet<NodeIndex>,
    seen: &mut HashSet<NodeIndex>,
    alias: Option<&str>,
) {
    let ingot_path = &graph[node];
    
    // Build the label with alias support
    let base_label = if let Some(config) = configs.get(ingot_path) {
        let ingot_name = config.metadata.name.as_deref().unwrap_or("null");
        let version = config
            .metadata
            .version
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| "null".to_string());
        
        // Show "ingot_name as alias" if alias differs from ingot name
        match alias {
            Some(alias_str) if alias_str != ingot_name => {
                format!("➖ {} as {} v{}", ingot_name, alias_str, version)
            }
            _ => format!("➖ {} v{}", ingot_name, version),
        }
    } else {
        "➖ [unknown config]".to_string()
    };

    let is_in_cycle = cycle_nodes.contains(&node);
    let will_close_cycle = seen.contains(&node);

    let mut label = base_label;
    if will_close_cycle {
        label = format!("{label} 🔄 [cycle]");
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

    // Process children with alias information from edges
    use petgraph::visit::EdgeRef;
    let children: Vec<_> = graph.edges_directed(node, petgraph::Direction::Outgoing).collect();

    for (i, edge) in children.iter().enumerate() {
        let child_prefix = if i == children.len() - 1 {
            TreePrefix::Last(prefix.child_indent())
        } else {
            TreePrefix::Fork(prefix.child_indent())
        };

        print_node_with_alias(
            graph,
            edge.target(),
            child_prefix,
            output,
            configs,
            cycle_nodes,
            seen,
            Some(&edge.weight().alias),
        );
    }

    seen.remove(&node);
}

fn red(s: &str) -> String {
    format!("\x1b[31m{s}\x1b[0m")
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

// Tree-specific ingot resolution types and functions

pub type TreeIngotGraphResolver = resolver::graph::GraphResolverImpl<FilesResolver, TreeIngotNodeHandler, EdgeWeight>;

pub fn basic_ingot_graph_resolver() -> TreeIngotGraphResolver {
    let files_resolver = FilesResolver::new()
        .with_required_file("fe.toml");
    resolver::graph::GraphResolverImpl::new(files_resolver)
}

#[derive(Debug)]
pub enum IngotResolutionDiagnostic {
    ConfigParseWarning(Url, String),
}

impl std::fmt::Display for IngotResolutionDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IngotResolutionDiagnostic::ConfigParseWarning(url, msg) => {
                write!(f, "Warning parsing config at {url}: {msg}")
            }
        }
    }
}

#[derive(Default)]
pub struct TreeIngotNodeHandler {
    pub configs: HashMap<Url, Config>,
    pub diagnostics: Vec<IngotResolutionDiagnostic>,
}

impl TreeIngotNodeHandler {
    pub fn diagnostics(&self) -> &[IngotResolutionDiagnostic] {
        &self.diagnostics
    }
}

impl ResolutionHandler<FilesResolver> for TreeIngotNodeHandler {
    type Item = Vec<(Url, EdgeWeight)>;

    fn handle_resolution(&mut self, ingot_url: &Url, resource: FilesResource) -> Self::Item {
        tracing::trace!(target: "resolver", "Handling ingot resolution for: {}", ingot_url);
        
        // Look for fe.toml file
        if let Some(config_file) = resource.files.iter().find(|f| f.path.file_name() == Some("fe.toml")) {
            match Config::parse(&config_file.content) {
                Ok(config) => {
                    tracing::trace!(target: "resolver", "Successfully parsed config for ingot: {}", ingot_url);
                    self.configs.insert(ingot_url.clone(), config.clone());
                    let dependencies = config
                        .based_dependencies(ingot_url)
                        .into_iter()
                        .map(|based_dependency| {
                            tracing::trace!(target: "resolver", "Found dependency: {} -> {}", ingot_url, based_dependency.url);
                            (
                                based_dependency.url,
                                EdgeWeight {
                                    alias: based_dependency.alias,
                                    arguments: based_dependency.parameters,
                                },
                            )
                        })
                        .collect();
                    dependencies
                }
                Err(err) => {
                    tracing::warn!(target: "resolver", "Failed to parse config for ingot {}: {}", ingot_url, err);
                    self.diagnostics
                        .push(IngotResolutionDiagnostic::ConfigParseWarning(
                            ingot_url.clone(),
                            err.to_string(),
                        ));
                    vec![]
                }
            }
        } else {
            // This case should not happen since we require fe.toml, but handle it gracefully
            vec![]
        }
    }
}

impl GraphResolutionHandler<Url, DiGraph<Url, EdgeWeight>> for TreeIngotNodeHandler {
    type Item = DiGraph<Url, EdgeWeight>;

    fn handle_graph_resolution(
        &mut self,
        _ingot_url: &Url,
        graph: DiGraph<Url, EdgeWeight>,
    ) -> Self::Item {
        graph
    }
}
