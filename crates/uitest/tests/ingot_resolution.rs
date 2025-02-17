use core::panic;
use std::fs;

use camino::Utf8PathBuf;
use resolver::{ingot::graph::GraphResolver, Resolver};
use test_utils::snap_test;

#[test]
fn workspace_graph_resolution() {
    let mut resolver = GraphResolver::default();
    let graph = resolver
        .resolve(
            &Utf8PathBuf::from("fixtures/ingot_resolution/workspace")
                .canonicalize_utf8()
                .unwrap(),
        )
        .expect("dep error");
    let diags = resolver.take_diagnostics();
    if !diags.is_empty() {
        panic!("{:#?}", diags)
    }
    fs::write("workspace.dot", graph.dot()).unwrap();
    let snap_path = format!(
        "{}/fixtures/ingot_resolution/workspace_graph",
        env!("CARGO_MANIFEST_DIR")
    );
    snap_test!(graph.dot(), &snap_path);
}

#[test]
fn ingot_a_resolution() {
    let mut resolver = GraphResolver::default();
    let graph = resolver
        .resolve(
            &Utf8PathBuf::from("fixtures/ingot_resolution/workspace/A")
                .canonicalize_utf8()
                .unwrap(),
        )
        .expect("dep error");
    let diags = resolver.take_diagnostics();
    if !diags.is_empty() {
        panic!("{:#?}", diags)
    }
    fs::write("ingot_a.dot", graph.dot()).unwrap();
    let snap_path = format!(
        "{}/fixtures/ingot_resolution/ingot_a_graph",
        env!("CARGO_MANIFEST_DIR")
    );
    snap_test!(graph.dot(), &snap_path);
}

#[test]
fn bad_workspace_resolution() {
    let mut resolver = GraphResolver::default();
    let graph = resolver
        .resolve(
            &Utf8PathBuf::from("fixtures/ingot_resolution/bad_workspace")
                .canonicalize_utf8()
                .unwrap(),
        )
        .expect("dep error");
    let diags = resolver.take_diagnostics();

    let snap_path = format!(
        "{}/fixtures/ingot_resolution/bad_workspace_diags",
        env!("CARGO_MANIFEST_DIR")
    );

    let diags = format!("{:?}", diags);

    snap_test!(&diags, &snap_path);

    // fs::write("ingot_a.dot", graph.dot()).unwrap();
    let snap_path = format!(
        "{}/fixtures/ingot_resolution/bad_workspace_graph",
        env!("CARGO_MANIFEST_DIR")
    );
    snap_test!(graph.dot(), &snap_path);
}
