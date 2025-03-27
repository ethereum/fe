pub mod file;
pub mod git;
pub mod graph;
pub mod ingot;

pub trait Resolver: Sized {
    type Description;
    type Resource;
    type Error;
    type Diagnostic;

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>;

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic>;
}

pub trait ResolutionHandler<R>
where
    R: Resolver,
{
    type Item;

    fn handle_resolution(
        &mut self,
        description: &R::Description,
        resource: R::Resource,
    ) -> Self::Item;
}

mod Example {
    use std::str::FromStr;

    use camino::Utf8PathBuf;
    use indexmap::IndexSet;

    use crate::{
        file::FileResolver,
        graph::{GraphNode, GraphResolver, GraphResolverImpl},
        ResolutionHandler, Resolver,
    };

    struct ConfigId;
    impl ConfigId {
        pub fn ingot_dependency_paths(&self, db: &DataBase) -> IndexSet<Utf8PathBuf> {
            todo!()
        }
    }

    #[derive(Default)]
    struct DataBase;
    impl DataBase {
        pub fn config_file(&self, path: Utf8PathBuf, content: String) -> ConfigId {
            todo!()
        }
    }

    struct IngotDependencies {
        source: Utf8PathBuf,
        targets: IndexSet<Utf8PathBuf>,
    }

    impl GraphNode<Utf8PathBuf> for IngotDependencies {
        fn id(&self) -> Utf8PathBuf {
            self.source.clone()
        }

        fn forward_ids(&self) -> IndexSet<Utf8PathBuf> {
            self.targets
                .iter()
                .map(|target| self.source.join(target))
                .collect()
        }
    }

    #[derive(Default)]
    struct IngotDependenciesHandler {
        resolved_configs: Vec<ConfigId>,
        db: DataBase,
    }
    impl ResolutionHandler<FileResolver> for IngotDependenciesHandler {
        type Item = IngotDependencies;

        // We can add the config file to the database here and retain the ID in the handler. The
        // handler produces an instance of `GraphNode`, which is used to complete the graph.
        fn handle_resolution(&mut self, path: &Utf8PathBuf, content: String) -> IngotDependencies {
            let config = self.db.config_file(path.clone(), content);
            let ingot_dependencies = IngotDependencies {
                source: path.clone(),
                targets: config.ingot_dependency_paths(&self.db),
            };
            self.resolved_configs.push(config);
            ingot_dependencies
        }
    }

    #[derive(Default)]
    struct GraphHandler;

    impl<GR> ResolutionHandler<GR> for GraphHandler
    where
        GR: GraphResolver<FileResolver, IngotDependenciesHandler>,
    {
        type Item = ();

        fn handle_resolution(
            &mut self,
            description: &GR::Description,
            resource: GR::Resource,
        ) -> Self::Item {
            todo!()
        }
    }

    fn ingot_graph_resolution() {
        let mut graph_resolver: GraphResolverImpl<FileResolver, IngotDependenciesHandler> =
            GraphResolverImpl::default();
        let mut graph_handler = GraphHandler::default();

        let ingot_path = Utf8PathBuf::from_str("/path/to/ingot").unwrap();
        let graph = graph_resolver.resolve(&mut graph_handler, &ingot_path);
    }
}
