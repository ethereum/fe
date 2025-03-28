use std::str::FromStr;

use camino::Utf8PathBuf;

// base traits

pub trait Resolver<HandlerResult> {
    type Description;
    type Resource;
    type Error;
    type Diagnostic;

    fn resolve<H: ResolutionHandler<Self::Description, Self::Resource, HandlerResult>>(
        &mut self,
        handler: &H,
        description: &Self::Description,
    ) -> Result<HandlerResult, Self::Error>;

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic>;
}

pub trait ResolutionHandler<Description, Resource, HandlerResult> {
    fn already_handled(&self, description: Description) -> bool;

    fn handle_resolution(&mut self, description: Description, resource: Resource) -> HandlerResult;
}

// example usage

fn example() {
    let input_db = InputDb;
    let mut file_resolver = FileResolver;
    let my_config_path = Utf8PathBuf::from_str("foobar").unwrap();
    let my_config_id = file_resolver.resolve(&input_db, &my_config_path).unwrap();
    let mut graph_resolver: GraphResolver<FileResolver, ConfigId, InputDb> =
        GraphResolver::default();
    let my_graph_id = graph_resolver.resolve(&input_db, &()).unwrap();
}

// defs

#[derive(Default)]
pub struct InputDb;
#[derive(Default)]
pub struct FileResolver;
#[derive(Default)]
pub struct ConfigId;
#[derive(Default)]
pub struct GraphId;
#[derive(Default)]
pub struct GraphResolver<NodeResolver, Node, NodeHandler>
where
    NodeResolver: Resolver<Node>,
    NodeHandler: ResolutionHandler<NodeResolver::Description, NodeResolver::Resource, Node>,
    Node: GraphNode,
{
    nodes: Vec<Node>,
    node_handler: NodeHandler,
    node_resolver: NodeResolver,
}
pub struct Graph<Node> {
    node: Node,
}

// imputdb handler impls

impl ResolutionHandler<(), Graph<ConfigId>, ()> for InputDb {
    fn handle_resolution(&mut self, description: (), resource: Graph<ConfigId>) -> () {
        todo!()
    }

    fn already_handled(&self, description: ()) -> bool {
        todo!()
    }
}

impl ResolutionHandler<Utf8PathBuf, String, ConfigId> for InputDb {
    fn already_handled(&self, description: Utf8PathBuf) -> bool {
        // self.is_interned(description)
        todo!()
    }

    fn handle_resolution(&mut self, description: Utf8PathBuf, resource: String) -> ConfigId {
        todo!()
    }
}

// resolver impls

trait GraphNode {}

impl<GraphHandler, NodeHandler, NodeResolver, Node> Resolver<GraphHandler>
    for GraphResolver<NodeResolver, Node, NodeHandler>
where
    NodeResolver: Resolver<Node>,
    NodeHandler: ResolutionHandler<NodeResolver::Description, NodeResolver::Resource, Node>,
    Node: GraphNode,
{
    type Description = ();
    type Resource = Graph<Node>;
    type Error = ();
    type Diagnostic = ();

    fn resolve<H: ResolutionHandler<Self::Description, Self::Resource, GraphHandler>>(
        &mut self,
        handler: &H,
        description: &Self::Description,
    ) -> Result<GraphHandler, Self::Error> {
        todo!()
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        todo!()
    }
}

impl<HandlerResult> Resolver<HandlerResult> for FileResolver {
    type Description = Utf8PathBuf;
    type Resource = String;
    type Error = ();
    type Diagnostic = ();

    fn resolve<H: ResolutionHandler<Self::Description, Self::Resource, HandlerResult>>(
        &mut self,
        handler: &H,
        description: &Self::Description,
    ) -> Result<HandlerResult, Self::Error> {
        todo!()
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        todo!()
    }
}

impl GraphNode for ConfigId {}
