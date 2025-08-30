use std::collections::HashMap;

use camino::Utf8PathBuf;
use common::{config::Config, graph::EdgeWeight, tree::display_dependency_tree};
use resolver::{
    files::{FilesResolver, FilesResource},
    graph::{DiGraph, GraphResolutionHandler, GraphResolver},
    ResolutionHandler, Resolver,
};
use url::Url;

pub fn print_tree(path: &Utf8PathBuf) {
    let mut resolver = tree_resolver();
    let mut handler = TreeHandler::default();

    let canonical_path = match path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {}", path);
            return;
        }
    };

    let ingot_url = match Url::from_directory_path(canonical_path.as_str()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid directory path: {}", path);
            return;
        }
    };

    match resolver.graph_resolve(&mut handler, &ingot_url) {
        Ok(tree_output) => {
            // Print graph resolver diagnostics (unresolvable nodes)
            for unresolvable_node in resolver.take_diagnostics() {
                eprintln!(
                    "❌ Failed to resolve ingot dependency '{}': {}",
                    unresolvable_node.0, unresolvable_node.1
                );
            }

            // Print files resolver diagnostics
            for diagnostic in resolver.node_resolver.take_diagnostics() {
                eprintln!("❌ File resolution error: {}", diagnostic);
            }

            // Print the tree
            print!("{}", tree_output);
        }
        Err(err) => {
            // Print diagnostics even on failure
            for unresolvable_node in resolver.take_diagnostics() {
                eprintln!(
                    "❌ Failed to resolve ingot dependency '{}': {}",
                    unresolvable_node.0, unresolvable_node.1
                );
            }

            for diagnostic in resolver.node_resolver.take_diagnostics() {
                eprintln!("❌ File resolution error: {}", diagnostic);
            }

            println!("❌ Failed to resolve dependency tree: {err}");
        }
    }
}

pub type IngotGraphResolver =
    resolver::graph::GraphResolverImpl<FilesResolver, TreeHandler, EdgeWeight>;

pub fn tree_resolver() -> IngotGraphResolver {
    let files_resolver = FilesResolver::new().with_required_file("fe.toml");
    resolver::graph::GraphResolverImpl::new(files_resolver)
}

#[derive(Default)]
pub struct TreeHandler {
    pub configs: HashMap<Url, Config>,
}

impl ResolutionHandler<FilesResolver> for TreeHandler {
    type Item = Vec<(Url, EdgeWeight)>;

    fn handle_resolution(&mut self, ingot_url: &Url, resource: FilesResource) -> Self::Item {
        tracing::trace!(target: "resolver", "Handling ingot resolution for: {}", ingot_url);

        // Look for fe.toml file
        if let Some(config_file) = resource
            .files
            .iter()
            .find(|f| f.path.file_name() == Some("fe.toml"))
        {
            match Config::parse(&config_file.content) {
                Ok(config) => {
                    tracing::trace!(target: "resolver", "Successfully parsed config for ingot: {}", ingot_url);

                    // Report config validation diagnostics
                    for diagnostic in &config.diagnostics {
                        eprintln!(
                            "❌ Config validation error at {}: {}",
                            ingot_url, diagnostic
                        );
                    }

                    self.configs.insert(ingot_url.clone(), config.clone());
                    let dependencies = config.forward_edges(ingot_url);
                    for (url, _) in &dependencies {
                        tracing::trace!(target: "resolver", "Found dependency: {} -> {}", ingot_url, url);
                    }
                    dependencies
                }
                Err(err) => {
                    tracing::warn!(target: "resolver", "Failed to parse config for ingot {}: {}", ingot_url, err);
                    eprintln!("❌ Invalid fe.toml file at {}: {}", ingot_url, err);
                    vec![]
                }
            }
        } else {
            // This case should not happen since we require fe.toml, but handle it gracefully
            vec![]
        }
    }
}

impl GraphResolutionHandler<Url, DiGraph<Url, EdgeWeight>> for TreeHandler {
    type Item = String;

    fn handle_graph_resolution(
        &mut self,
        ingot_url: &Url,
        graph: DiGraph<Url, EdgeWeight>,
    ) -> Self::Item {
        display_dependency_tree(&graph, ingot_url, &self.configs)
    }
}
