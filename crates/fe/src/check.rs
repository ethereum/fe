use crate::tree::print_tree_impl;
use camino::Utf8PathBuf;
use common::{urlext::canonical_url, InputDb};
use driver::DriverDataBase;
use resolver::graph::petgraph;
use std::collections::HashMap;
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
    let ingot_url = match canonical_url(dir_path) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {}", dir_path);
            eprintln!("       Make sure the directory exists and is accessible");
            return true;
        }
    };
    let init_diagnostics = driver::init_ingot(db, &ingot_url);

    // Handle workspace setup diagnostics if any
    let mut cycles_found = Vec::new();
    let mut has_resolution_issues = false;

    if !init_diagnostics.is_empty() {
        has_resolution_issues = true;
        for diagnostic in &init_diagnostics {
            match diagnostic {
                driver::IngotInitDiagnostics::IngotDependencyCycle { subgraph: _, cycles } => {
                    cycles_found.extend(cycles.clone());
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
        // Check if the issue is a missing fe.toml file
        let config_url = match ingot_url.join("fe.toml") {
            Ok(url) => url,
            Err(_) => {
                eprintln!("Error: Invalid ingot directory path");
                return true;
            }
        };
        
        if db.workspace().get(db, &config_url).is_none() {
            eprintln!("Error: No fe.toml file found in the root directory");
            eprintln!("       Expected fe.toml at: {}", config_url);
            eprintln!("       Make sure you're in an fe project directory or create a fe.toml file");
        } else {
            eprintln!("Error: Could not resolve ingot from directory");
        }
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


fn handle_circular_dependencies(db: &DriverDataBase, _cycles: &[Vec<Url>], root_url: &Url) {
    eprintln!("🔄 Cyclic dependencies detected");
    eprintln!();

    // Use the database's cyclic subgraph which includes paths to cycles
    let cyclic_subgraph = db.graph().cyclic_subgraph(db);
    
    if cyclic_subgraph.node_count() > 0 {
        // Get configs for the nodes in the cyclic subgraph
        let mut configs = HashMap::new();
        for node_idx in cyclic_subgraph.node_indices() {
            let url = &cyclic_subgraph[node_idx];
            if let Some(ingot) = db.workspace().containing_ingot(db, url.clone()) {
                if let Some(config) = ingot.config(db) {
                    configs.insert(url.clone(), config);
                }
            }
        }
        
        // Find a suitable root for tree printing
        let tree_root = if cyclic_subgraph.node_indices().any(|idx| cyclic_subgraph[idx] == *root_url) {
            // Original root is in the subgraph
            root_url.clone()
        } else {
            // Find nodes with no incoming edges (potential roots)
            let roots: Vec<_> = cyclic_subgraph.node_indices()
                .filter(|&node| cyclic_subgraph.neighbors_directed(node, petgraph::Direction::Incoming).count() == 0)
                .collect();
            
            if !roots.is_empty() {
                cyclic_subgraph[roots[0]].clone()
            } else {
                // Fallback to any node
                cyclic_subgraph[cyclic_subgraph.node_indices().next().unwrap()].clone()
            }
        };
        
        // Print tree showing paths to cycles
        let tree_output = print_tree_impl(&cyclic_subgraph, &tree_root, &configs);
        print!("{tree_output}");
    } else {
        eprintln!("No cyclic subgraph found (this shouldn't happen)");
    }

    eprintln!();
    eprintln!("💡 To resolve circular dependencies:");
    eprintln!("   • Review your dependency declarations in fe.toml files");
    eprintln!("   • Consider breaking the cycle by removing or restructuring dependencies");
    eprintln!("   • Move shared functionality to a separate common library");
}
