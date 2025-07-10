use std::collections::HashMap;

use common::config::{Config, DependencyArguments};
use smol_str::SmolStr;
use url::Url;

use crate::{
    files::{File, FilesResolver},
    graph::GraphResolverImpl,
    ResolutionHandler,
};

pub type IngotGraphResolver<NH> =
    GraphResolverImpl<FilesResolver, NH, (SmolStr, DependencyArguments)>;

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

// #[derive(Debug)]
// pub struct IngotConfigDoesNotExist;
//
// #[derive(Debug)]
// pub struct UnresolvedDependency;

pub type BasicIngotGraphResolver = IngotGraphResolver<BasicIngotNodeHandler>;

#[derive(Default)]
pub struct BasicIngotNodeHandler {
    pub configs: HashMap<Url, Config>,
}

impl ResolutionHandler<FilesResolver> for BasicIngotNodeHandler {
    type Item = Vec<(Url, (SmolStr, DependencyArguments))>;

    fn handle_resolution(&mut self, ingot_url: &Url, mut files: Vec<File>) -> Self::Item {
        if let Some(file) = files.pop() {
            let config = Config::parse(&file.content).unwrap();
            self.configs.insert(ingot_url.clone(), config.clone());
            config
                .based_dependencies(ingot_url)
                .into_iter()
                .map(|based_dependency| {
                    (
                        // Node weight
                        based_dependency.url,
                        // Node edge
                        (based_dependency.alias, based_dependency.parameters),
                    )
                })
                .collect()
        } else {
            vec![]
        }
    }
}
