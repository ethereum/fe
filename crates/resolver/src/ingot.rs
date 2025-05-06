use std::str::FromStr;

use camino::Utf8PathBuf;

use crate::{files::FilesResolver, graph::GraphResolverImpl, ResolutionHandler, Resolver};

#[derive(Clone, Debug)]
pub struct IngotArguments;

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
pub struct BasicIngotNodeHandler;

impl ResolutionHandler<FilesResolver> for BasicIngotNodeHandler {
    type Item = Vec<(Utf8PathBuf, IngotArguments)>;

    fn handle_resolution(
        &mut self,
        ingot_path: &Utf8PathBuf,
        files: Vec<(Utf8PathBuf, String)>,
    ) -> Self::Item {
        if let Some((_file_path, content)) = files.first() {
            if let Ok(toml_content) = toml::from_str::<toml::value::Table>(&content) {
                if let Some(value) = toml_content.get("dependencies") {
                    if let Some(table) = value.as_table() {
                        let mut dependencies = vec![];
                        for (_alias, value) in table {
                            if let Some(path_str) = value.as_str() {
                                if let Ok(path) = Utf8PathBuf::from_str(path_str) {
                                    dependencies.push((
                                        ingot_path.join(path).canonicalize_utf8().unwrap(),
                                        IngotArguments,
                                    ));
                                }
                            }
                        }
                        return dependencies;
                    }
                }
            }
        }

        return vec![];
    }
}
