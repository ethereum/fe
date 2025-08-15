use std::{collections::HashMap, fmt};

use common::{config::Config, graph::EdgeWeight};
use url::Url;

use crate::{
    files::{File, FilesResolver},
    graph::{DiGraph, GraphResolutionHandler, GraphResolverImpl},
    ResolutionHandler,
};

pub type IngotGraphResolver<NH> = GraphResolverImpl<FilesResolver, NH, EdgeWeight>;

pub fn basic_ingot_graph_resolver() -> IngotGraphResolver<BasicIngotNodeHandler> {
    GraphResolverImpl::new(FilesResolver::exact_file("fe.toml".into()))
}

pub fn ingot_graph_resolver<NH>() -> IngotGraphResolver<NH> {
    let files_resolver = FilesResolver::with_patterns(&["fe.toml", "src/**/*.fe"]);
    GraphResolverImpl::new(files_resolver)
}

#[derive(Debug)]
pub struct IngotConfigDoesNotExist(pub Url);

#[derive(Debug)]
pub struct UnresolvedDependency(pub String);

#[derive(Debug)]
pub enum IngotResolutionError {
    ConfigDoesNotExist(Url),
    ConfigParseError(String),
    UnresolvedDependency(String),
}

#[derive(Debug)]
pub enum IngotResolutionDiagnostic {
    ConfigParseWarning(Url, String),
    DependencyNotFound(Url, String),
}

impl fmt::Display for IngotConfigDoesNotExist {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Ingot configuration does not exist at: {}", self.0)
    }
}

impl std::error::Error for IngotConfigDoesNotExist {}

impl fmt::Display for UnresolvedDependency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Unresolved dependency: {}", self.0)
    }
}

impl std::error::Error for UnresolvedDependency {}

impl fmt::Display for IngotResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IngotResolutionError::ConfigDoesNotExist(url) => {
                write!(f, "Ingot configuration does not exist at: {url}")
            }
            IngotResolutionError::ConfigParseError(msg) => {
                write!(f, "Failed to parse ingot configuration: {msg}")
            }
            IngotResolutionError::UnresolvedDependency(dep) => {
                write!(f, "Unresolved dependency: {dep}")
            }
        }
    }
}

impl std::error::Error for IngotResolutionError {}

impl fmt::Display for IngotResolutionDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IngotResolutionDiagnostic::ConfigParseWarning(url, msg) => {
                write!(f, "Warning parsing config at {url}: {msg}")
            }
            IngotResolutionDiagnostic::DependencyNotFound(url, dep) => {
                write!(f, "Dependency '{dep}' not found for ingot at {url}")
            }
        }
    }
}

pub type BasicIngotGraphResolver = IngotGraphResolver<BasicIngotNodeHandler>;

#[derive(Default)]
pub struct BasicIngotNodeHandler {
    pub configs: HashMap<Url, Config>,
    pub diagnostics: Vec<IngotResolutionDiagnostic>,
}

impl BasicIngotNodeHandler {
    pub fn take_diagnostics(&mut self) -> Vec<IngotResolutionDiagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    pub fn diagnostics(&self) -> &[IngotResolutionDiagnostic] {
        &self.diagnostics
    }
}

impl ResolutionHandler<FilesResolver> for BasicIngotNodeHandler {
    type Item = Vec<(Url, EdgeWeight)>;

    fn handle_resolution(&mut self, ingot_url: &Url, mut files: Vec<File>) -> Self::Item {
        tracing::trace!(target: "resolver", "Handling ingot resolution for: {}", ingot_url);
        if let Some(file) = files.pop() {
            match Config::parse(&file.content) {
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
            vec![]
        }
    }
}

impl GraphResolutionHandler<Url, DiGraph<Url, EdgeWeight>> for BasicIngotNodeHandler {
    type Item = DiGraph<Url, EdgeWeight>;

    fn handle_graph_resolution(
        &mut self,
        _ingot_url: &Url,
        graph: DiGraph<Url, EdgeWeight>,
    ) -> Self::Item {
        // For graph resolution, we can process the graph and return it
        graph
    }
}
