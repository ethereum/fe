use std::str::FromStr;

use camino::Utf8PathBuf;
use common::urlext::UrlExt;
use fe_resolver::{ingot::basic_ingot_graph_resolver, Resolver};
use test_utils::snap_test;
use url::Url;

#[test]
fn ingot_graph_resolution() {
    let mut resolver = basic_ingot_graph_resolver();
    let base_url = Url::from_directory_path(
        Utf8PathBuf::from_str(".")
            .unwrap()
            .canonicalize_utf8()
            .unwrap(),
    )
    .unwrap();
    let ingot_a_url = base_url
        .join("fixtures/ingots/A")
        .unwrap()
        .directory()
        .unwrap();
    println!("{:?}", ingot_a_url);
    let ingot_graph = resolver.transient_resolve(&ingot_a_url).unwrap();
    snap_test!(
        format!("{:#?}\n{:#?}", ingot_graph, resolver.take_diagnostics()),
        "../../../fixtures/ingots"
    );
}
