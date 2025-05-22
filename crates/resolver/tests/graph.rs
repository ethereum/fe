use core::panic;
use std::{collections::HashMap, hash::Hash, marker::PhantomData};

use dir_test::{dir_test, Fixture};
use fe_resolver::{graph::GraphResolverImpl, ResolutionHandler, Resolver};
use test_utils::snap_test;

struct FixtureEntryResolver<K, V>(pub Fixture<HashMap<K, V>>);

impl<K, V> FixtureEntryResolver<K, V> {}

#[derive(Debug)]
struct EntryDoesNotExist;

impl<K: Eq + Hash, V: Clone> Resolver for FixtureEntryResolver<K, V> {
    type Description = K;
    type Resource = V;
    type Error = EntryDoesNotExist;
    type Diagnostic = ();

    fn transient_resolve(
        &mut self,
        description: &Self::Description,
    ) -> Result<Self::Resource, Self::Error> {
        if let Some(value) = self.0.content().get(description) {
            Ok(value.clone())
        } else {
            Err(EntryDoesNotExist)
        }
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        panic!()
    }
}

#[derive(Default)]
pub struct MockNodeHandler;

impl ResolutionHandler<FixtureEntryResolver<String, Vec<String>>> for MockNodeHandler {
    type Item = Vec<(String, ())>;

    fn handle_resolution(&mut self, source: &String, targets: Vec<String>) -> Self::Item {
        targets.into_iter().map(|target| (target, ())).collect()
    }
}

fn load_toml(path: &str) -> HashMap<String, Vec<String>> {
    let text = std::fs::read_to_string(path).expect("Failed to read TOML file");
    toml::from_str(&text).expect("Invalid TOML")
}

type FixtureEntryGraphResolver<K, V> =
    GraphResolverImpl<FixtureEntryResolver<K, V>, MockNodeHandler, ()>;

fn fixture_resolver<K, V>(fixture: Fixture<HashMap<K, V>>) -> FixtureEntryGraphResolver<K, V> {
    GraphResolverImpl {
        node_resolver: FixtureEntryResolver(fixture),
        node_handler: MockNodeHandler::default(),
        diagnostics: vec![],
        _edge: PhantomData,
    }
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/graph",
    glob: "*.toml",
    loader: load_toml,
)]
fn graph_resolution(fixture: Fixture<HashMap<String, Vec<String>>>) {
    let fixture_path = fixture.path();
    let mut resolver = fixture_resolver(fixture);
    let graph = resolver.transient_resolve(&"a".to_string()).unwrap();
    snap_test!(
        format!("{:#?}\n{:#?}", graph, resolver.take_diagnostics()),
        fixture_path
    );
}
