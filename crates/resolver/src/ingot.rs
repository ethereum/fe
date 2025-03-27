use std::collections::HashMap;

use camino::Utf8PathBuf;
use common::config::{Config, DependencyDescription, IngotArguments};
use smol_str::SmolStr;

use crate::{files::FilesResolver, graph::GraphResolverImpl, ResolutionHandler};

pub type IngotGraphResolver<NH> = GraphResolverImpl<FilesResolver, NH, (SmolStr, IngotArguments)>;

pub fn basic_ingot_graph_resolver() -> IngotGraphResolver<BasicIngotNodeHandler> {
    GraphResolverImpl::new(
        FilesResolver::exact_file("fe.toml".into()),
        BasicIngotNodeHandler::default(),
    )
}

pub fn ingot_graph_resolver<NH>(node_handler: NH) -> IngotGraphResolver<NH> {
    let files_resolver = FilesResolver::with_patterns(&["fe.toml", "src/**/*.fe"]);
    GraphResolverImpl::new(files_resolver, node_handler)
}

#[derive(Debug)]
pub struct IngotConfigDoesNotExist;

#[derive(Debug)]
pub struct UnresolvedDependency;

pub type BasicIngotGraphResolver = IngotGraphResolver<BasicIngotNodeHandler>;

#[derive(Default)]
pub struct BasicIngotNodeHandler {
    pub configs: HashMap<Utf8PathBuf, Config>,
}

impl ResolutionHandler<FilesResolver> for BasicIngotNodeHandler {
    type Item = Vec<(Utf8PathBuf, (SmolStr, IngotArguments))>;

    fn handle_resolution(
        &mut self,
        ingot_path: &Utf8PathBuf,
        mut files: Vec<(Utf8PathBuf, String)>,
    ) -> Self::Item {
        if let Some((_file_path, content)) = files.pop() {
            let config = Config::from_string(content);
            self.configs.insert(ingot_path.clone(), config.clone());
            config
                .dependencies
                .into_iter()
                .map(|dependency| match dependency.description {
                    DependencyDescription::Path(path) => (
                        ingot_path.join(path).canonicalize_utf8().unwrap(),
                        (dependency.alias, IngotArguments::default()),
                    ),
                    DependencyDescription::PathWithArguments { path, arguments } => (
                        ingot_path.join(path).canonicalize_utf8().unwrap(),
                        (dependency.alias, arguments),
                    ),
                })
                .collect()
        } else {
            vec![]
        }
    }
}
