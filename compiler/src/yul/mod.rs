//! Fe to Yul compiler.

use crate::types::{
    FeModuleAst,
    NamedYulContracts,
};
use fe_analyzer::Context;

pub mod constants;
mod constructor;
mod mappers;
mod names;
mod operations;
pub mod runtime;
mod utils;

/// Compiles Fe source code to Yul.
///
/// # Panics
///
/// Any failure to compile an AST to Yul is considered a bug, and thus panics.
/// Invalid ASTs should be caught by an analysis step prior to Yul generation.
pub fn compile(context: &Context, module: &FeModuleAst) -> NamedYulContracts {
    mappers::module::module(context, module)
        .drain()
        .map(|(name, object)| (name, object.to_string().replace("\"", "\\\"")))
        .collect::<NamedYulContracts>()
}
