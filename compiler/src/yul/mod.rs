//! Fe to Yul compiler.

use crate::types::{FeModuleAst, NamedYulContracts};
pub use fe_analyzer::context::Context as AnalyzerContext;

pub mod constants;
pub mod constructor;
mod context;
mod mappers;
mod names;
mod operations;
pub mod runtime;
mod utils;

pub(crate) use context::Context;

/// Compiles Fe source code to Yul.
///
/// # Panics
///
/// Any failure to compile an AST to Yul is considered a bug, and thus panics.
/// Invalid ASTs should be caught by an analysis step prior to Yul generation.
pub fn compile(analysis: &AnalyzerContext, module: &FeModuleAst) -> NamedYulContracts {
    mappers::module::module(analysis, module)
        .drain()
        .map(|(name, object)| (name, object.to_string().replace("\"", "\\\"")))
        .collect::<NamedYulContracts>()
}
