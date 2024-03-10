//! Fe semantic analysis.
//!
//! This library is used to analyze the semantics of a given Fe AST. It detects
//! any semantic errors within a given AST and produces a `Context` instance
//! that can be used to query contextual information attributed to AST nodes.

pub mod builtins;
pub mod constants;
pub mod context;
pub mod db;
pub mod display;
pub mod errors;
pub mod namespace;

mod operations;
mod traversal;

pub use db::{AnalyzerDb, TestDb};
pub use traversal::pattern_analysis;

use fe_common::diagnostics::Diagnostic;
use namespace::items::{IngotId, ModuleId};

pub fn analyze_ingot(db: &dyn AnalyzerDb, ingot_id: IngotId) -> Result<(), Vec<Diagnostic>> {
    let diagnostics = ingot_id.diagnostics(db);

    if diagnostics.is_empty() {
        Ok(())
    } else {
        Err(diagnostics)
    }
}

pub fn analyze_module(db: &dyn AnalyzerDb, module_id: ModuleId) -> Result<(), Vec<Diagnostic>> {
    let diagnostics = module_id.diagnostics(db);

    if diagnostics.is_empty() {
        Ok(())
    } else {
        Err(diagnostics)
    }
}
