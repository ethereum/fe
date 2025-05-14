use std::collections::HashMap;

use camino::Utf8PathBuf;
use common::config::{Config, Dependency, IngotArguments};

use crate::{files::FilesResolver, graph::GraphResolverImpl, ResolutionHandler, Resolver};

pub type IngotGraphResolver<NH> = GraphResolverImpl<FilesResolver, NH, IngotArguments>;

pub fn basic_ingot_graph_resolver() -> IngotGraphResolver<BasicIngotNodeHandler> {
    GraphResolverImpl::new(
        FilesResolver::exact_file("fe.toml".into()),
        BasicIngotNodeHandler::default(),
    )
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
    type Item = Vec<(Utf8PathBuf, IngotArguments)>;

    fn handle_resolution(
        &mut self,
        ingot_path: &Utf8PathBuf,
        mut files: Vec<(Utf8PathBuf, String)>,
    ) -> Self::Item {
        if let Some((_file_path, content)) = files.pop() {
            let config = Config::from_string(content);
            self.configs.insert(ingot_path.clone(), config.clone());
            return config
                .dependencies
                .into_iter()
                .map(|dependency| match dependency {
                    Dependency::Path(path) => (
                        ingot_path.join(path).canonicalize_utf8().unwrap(),
                        IngotArguments::default(),
                    ),
                    Dependency::PathWithArguments { path, arguments } => (
                        ingot_path.join(path).canonicalize_utf8().unwrap(),
                        arguments,
                    ),
                })
                .collect();
        }

        return vec![];
    }
}
