//! Fe semantic analysis.
//!
//! This library is used to analyze the semantics of a given Fe AST. It detects
//! any semantic errors within a given AST and produces a `Context` instance
//! that can be used to query contextual information attributed to AST nodes.

pub mod builtins;
pub mod constants;
pub mod context;
mod db;
pub mod errors;
pub mod namespace;
mod operations;
mod traversal;

pub use db::{AnalyzerDb, Db};
use fe_common::diagnostics::Diagnostic;
use fe_parser::ast;
use namespace::items;
use std::rc::Rc;

/// Performs semantic analysis of the source program
pub fn analyze(
    db: &dyn AnalyzerDb,
    module: ast::Module,
) -> Result<items::ModuleId, Vec<Diagnostic>> {
    let module_id = db.intern_module(Rc::new(items::Module { ast: module }));

    let diagnostics = module_id.diagnostics(db);
    if diagnostics.is_empty() {
        Ok(module_id)
    } else {
        Err(diagnostics)
    }
}
