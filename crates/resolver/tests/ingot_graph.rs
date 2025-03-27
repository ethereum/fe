use std::str::FromStr;

use camino::Utf8PathBuf;
use fe_resolver::{ingot::basic_ingot_graph_resolver, Resolver};
use test_utils::snap_test;

#[test]
fn ingot_graph_resolution() {
    let mut resolver = basic_ingot_graph_resolver();
    let ingot_graph = resolver
        .transient_resolve(
            &Utf8PathBuf::from_str("fixtures/ingots/A")
                .unwrap()
                .canonicalize_utf8()
                .unwrap(),
        )
        .unwrap();
    snap_test!(
        format!("{:#?}\n{:#?}", ingot_graph, resolver.take_diagnostics()),
        "../../../fixtures/ingots"
    );
}
