//! Fe semantic analysis.
//!
//! This library is used to analyze the semantics of a given Fe AST. It detects
//! any semantic errors within a given AST and produces a `Context` instance
//! that can be used to query contextual information attributed to AST nodes.

pub mod builtins;
pub mod constants;
pub mod context;
pub mod errors;
pub mod namespace;
mod operations;
mod traversal;

use crate::errors::{AnalyzerError, FatalError};
use context::Context;
use fe_common::files::SourceFileId;
use fe_parser::ast as fe;

/// Performs semantic analysis of the source program and returns a `Context`
/// instance.
pub fn analyze(module: &fe::Module, file_id: SourceFileId) -> Result<Context, AnalyzerError> {
    let mut context = Context::new(file_id);
    let result = traversal::module::module(&mut context, module);

    match result {
        Ok(()) => {
            if context.diagnostics.is_empty() {
                Ok(context)
            } else {
                Err(AnalyzerError(context.diagnostics))
            }
        }
        Err(FatalError) => {
            if context.diagnostics.is_empty() {
                panic!("Expected at least one error")
            }
            Err(AnalyzerError(context.diagnostics))
        }
    }
}
