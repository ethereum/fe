use std::fmt::Display;

use camino::Utf8PathBuf;
use resolver::{
    ingot::{basic_ingot_graph_resolver, BasicIngotGraphResolver},
    Resolver,
};

pub fn print_tree(path: &Utf8PathBuf) {
    let mut graph_resolver = basic_ingot_graph_resolver();
    let ingot_graph = graph_resolver.transient_resolve(path).unwrap();
}

pub fn print_graph_as_tree(graph: ()) {
    todo!()
}
