use core::panic;
use std::fs;

use camino::{Utf8Path, Utf8PathBuf};
use resolver::{ingot::graph::GraphResolver, Resolver};
use test_utils::snap_test;

#[test]
fn dependency_resolution() {
    let mut resolver = GraphResolver::default();
    let graph = resolver
        .resolve(
            &Utf8PathBuf::from("fixtures/dependency_resolution/workspace")
                .canonicalize_utf8()
                .unwrap(),
        )
        .expect("dep error");
    let diags = resolver.take_diagnostics();
    if !diags.is_empty() {
        panic!("{:#?}", diags)
    }
    fs::write("graph.dot", graph.dot()).unwrap();
    // snap_test!(graph.dot(), "path");
}
