use crate::tree::print_tree_impl;
use camino::Utf8PathBuf;
use common::{urlext::canonical_url, InputDb};
use driver::DriverDataBase;
use resolver::{
    graph::{
        petgraph::{self, visit::EdgeRef},
        DiGraph, GraphResolver,
    },
    ingot::BasicIngotNodeHandler,
};
use std::collections::{HashMap, HashSet};
use url::Url;

pub fn check(path: &Utf8PathBuf) {
    let mut db = DriverDataBase::default();

    // Determine if we're dealing with a single file or an ingot directory
    let has_errors = if path.is_file() && path.extension() == Some("fe") {
        check_single_file(&mut db, path)
    } else if path.is_dir() {
        check_ingot(&mut db, path)
    } else {
        eprintln!("Error: Path must be either a .fe file or a directory containing fe.toml");
        std::process::exit(1);
    };

    if has_errors {
        std::process::exit(1);
    }
}

fn check_single_file(db: &mut DriverDataBase, file_path: &Utf8PathBuf) -> bool {
    // Create a file URL for the single .fe file
    let file_url = match Url::from_file_path(file_path.canonicalize_utf8().unwrap()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid file path: {file_path}");
            return true;
        }
    };

    // Read the file content
    let content = match std::fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {file_path}: {err}");
            return true;
        }
    };

    // Add the file to the workspace
    db.workspace().touch(db, file_url.clone(), Some(content));

    // Try to get the file and check it for errors
    if let Some(file) = db.workspace().get(db, &file_url) {
        let top_mod = db.top_mod(file);
        let diags = db.run_on_top_mod(top_mod);
        if !diags.is_empty() {
            eprintln!("errors in {file_url}");
            eprintln!();
            diags.emit(db);
            return true;
        }
    } else {
        eprintln!("Error: Could not process file {file_path}");
        return true;
    }

    false
}

fn check_ingot(db: &mut DriverDataBase, dir_path: &Utf8PathBuf) -> bool {
    let ingot_url = canonical_url(dir_path);
    let init_diagnostics = driver::init_ingot(db, &ingot_url);

    // Handle workspace setup diagnostics if any
    let mut cycles_found = Vec::new();
    let mut has_resolution_issues = false;

    if !init_diagnostics.is_empty() {
        has_resolution_issues = true;
        for diagnostic in &init_diagnostics {
            match diagnostic {
                driver::IngotInitDiagnostics::IngotDependencyCycle { cycle } => {
                    cycles_found.push(cycle.clone());
                }
                _ => {
                    eprintln!("{diagnostic}");
                }
            }
        }
    }

    // Exit early if cycles were detected
    if !cycles_found.is_empty() {
        handle_circular_dependencies(db, &cycles_found, &ingot_url);
        return true;
    }

    // Exit early if there were any ingot resolution issues
    if has_resolution_issues {
        return true;
    }

    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        // If we can't find the ingot, there's nothing to check
        return true;
    };

    // Check if the ingot has source files before trying to analyze
    if ingot.root_file(db).is_err() {
        eprintln!(
            "source files resolution error: `src` folder does not exist in the ingot directory"
        );
        return true;
    }

    let diags = db.run_on_ingot(ingot);
    let mut has_errors = false;

    if !diags.is_empty() {
        diags.emit(db);
        has_errors = true;
    }

    // Collect all dependencies with errors
    let mut dependency_errors = Vec::new();
    for dependency_url in db.graph().dependency_urls(db, &ingot_url) {
        let Some(ingot) = db.workspace().containing_ingot(db, dependency_url.clone()) else {
            // Skip dependencies that can't be resolved
            continue;
        };
        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            dependency_errors.push((dependency_url, diags));
        }
    }

    // Print dependency errors if any exist
    if !dependency_errors.is_empty() {
        has_errors = true;
        if dependency_errors.len() == 1 {
            eprintln!("❌ Error in downstream ingot");
        } else {
            eprintln!("❌ Errors in downstream ingots");
        }

        for (dependency_url, diags) in dependency_errors {
            print_dependency_info(db, &dependency_url);
            diags.emit(db);
        }
    }

    has_errors
}

fn print_dependency_info(db: &DriverDataBase, dependency_url: &Url) {
    eprintln!();

    // Get the ingot for this dependency URL to access its config
    if let Some(ingot) = db.workspace().containing_ingot(db, dependency_url.clone()) {
        if let Some(config) = ingot.config(db) {
            let name = config.metadata.name.as_deref().unwrap_or("unknown");
            if let Some(version) = &config.metadata.version {
                eprintln!("➖ {name} (version: {version})");
            } else {
                eprintln!("➖ {name}");
            }
        } else {
            eprintln!("➖ Unknown dependency");
        }
    } else {
        eprintln!("➖ Unknown dependency");
    }

    eprintln!("🔗 {dependency_url}");
    eprintln!();
}

fn extract_cycle_subgraph(
    graph: &DiGraph<Url, common::graph::EdgeWeight>,
    cycles: &[Vec<Url>],
) -> DiGraph<Url, common::graph::EdgeWeight> {
    use petgraph::Graph;

    // Collect all URLs involved in cycles
    let mut cycle_urls = HashSet::new();
    for cycle in cycles {
        for url in cycle {
            cycle_urls.insert(url.clone());
        }
    }

    // Create a new graph with only cycle nodes and their edges
    let mut subgraph = Graph::new();
    let mut url_to_node = HashMap::new();

    // Add all cycle nodes to the subgraph
    for url in &cycle_urls {
        let node_idx = subgraph.add_node(url.clone());
        url_to_node.insert(url.clone(), node_idx);
    }

    // Add edges between cycle nodes
    for node_idx in graph.node_indices() {
        let source_url = &graph[node_idx];
        if cycle_urls.contains(source_url) {
            for edge in graph.edges(node_idx) {
                let target_url = &graph[edge.target()];
                if cycle_urls.contains(target_url) {
                    if let (Some(&source_node), Some(&target_node)) =
                        (url_to_node.get(source_url), url_to_node.get(target_url))
                    {
                        subgraph.add_edge(source_node, target_node, edge.weight().clone());
                    }
                }
            }
        }
    }

    subgraph
}

fn handle_circular_dependencies(db: &DriverDataBase, cycles: &[Vec<Url>], root_url: &Url) {
    eprintln!("🔄 Cyclic dependencies detected");
    eprintln!();

    // Use the resolver to get a fresh graph with proper access
    let mut graph_resolver = resolver::ingot::basic_ingot_graph_resolver();
    let mut node_handler = BasicIngotNodeHandler::default();

    // Use the root URL to resolve the full graph
    if let Ok(full_graph) = graph_resolver.graph_resolve(&mut node_handler, root_url) {
        // Extract subgraph containing only cycle nodes
        let cycle_subgraph = extract_cycle_subgraph(&full_graph, cycles);

        // Check if the root URL is part of any cycle to determine display approach
        let root_in_cycle = cycles.iter().any(|cycle| cycle.contains(root_url));

        if root_in_cycle {
            // If root is in a cycle, use it as the tree root
            let tree_output = print_tree_impl(&cycle_subgraph, root_url, &node_handler.configs);
            print!("{tree_output}");
        } else {
            // If root is not in cycles, show each cycle separately
            for (cycle_idx, cycle) in cycles.iter().enumerate() {
                if cycles.len() > 1 {
                    eprintln!("Cycle {}:", cycle_idx + 1);
                }

                // Use the first node in the cycle as the root for tree printing
                if let Some(cycle_root) = cycle.first() {
                    let tree_output =
                        print_tree_impl(&cycle_subgraph, cycle_root, &node_handler.configs);
                    print!("{tree_output}");
                }

                if cycles.len() > 1 && cycle_idx < cycles.len() - 1 {
                    eprintln!();
                }
            }
        }
    } else {
        // Fallback to simple cycle listing if graph resolution fails
        print_cycles_fallback(cycles, db);
    }

    eprintln!();
    eprintln!("💡 To resolve circular dependencies:");
    eprintln!("   • Review your dependency declarations in fe.toml files");
    eprintln!("   • Consider breaking the cycle by removing or restructuring dependencies");
    eprintln!("   • Move shared functionality to a separate common library");
}

fn print_cycles_fallback(cycles: &[Vec<Url>], db: &DriverDataBase) {
    // Collect ingot configs for display
    let mut configs = HashMap::new();
    for cycle in cycles {
        for url in cycle {
            if let Some(ingot) = db.workspace().containing_ingot(db, url.clone()) {
                if let Some(config) = ingot.config(db) {
                    configs.insert(url.clone(), config);
                }
            }
        }
    }

    for (cycle_idx, cycle) in cycles.iter().enumerate() {
        if cycles.len() > 1 {
            eprintln!("Cycle {}:", cycle_idx + 1);
        }

        for (i, url) in cycle.iter().enumerate() {
            let package_name = if let Some(config) = configs.get(url) {
                let name = config.metadata.name.as_deref().unwrap_or("unknown");
                if let Some(version) = &config.metadata.version {
                    format!("{name} v{version}")
                } else {
                    name.to_string()
                }
            } else {
                "[unknown]".to_string()
            };

            if i == 0 {
                eprintln!("  ➖ {package_name}");
            } else {
                eprintln!("    ├─ depends on ➖ {package_name}");
            }
        }

        // Show the cycle completion
        if let Some(first_url) = cycle.first() {
            let first_package = if let Some(config) = configs.get(first_url) {
                let name = config.metadata.name.as_deref().unwrap_or("unknown");
                if let Some(version) = &config.metadata.version {
                    format!("{name} v{version}")
                } else {
                    name.to_string()
                }
            } else {
                "[unknown]".to_string()
            };
            eprintln!("    └─ which depends on ➖ {first_package} 🔄");
        }

        if cycles.len() > 1 && cycle_idx < cycles.len() - 1 {
            eprintln!();
        }
    }
}
