//! Fe to Yul compiler.

use fe_analyzer::context::Context as AnalyzerContext;
use fe_parser::ast;
use std::collections::HashMap;

pub mod constants;
pub mod constructor;
mod context;
mod mappers;
pub mod names;
pub mod operations;
pub mod runtime;
mod utils;

pub(crate) use context::Context;

/// The name of a Fe contract.
pub type ContractName = String;
/// The intermediate representation of a contract as a string object.
pub type YulIr = String;
/// A mapping of contract names and their Yul IR.
pub type NamedYulContracts = HashMap<ContractName, YulIr>;

/// Compiles Fe source code to Yul.
///
/// # Panics
///
/// Any failure to compile an AST to Yul is considered a bug, and thus panics.
/// Invalid ASTs should be caught by an analysis step prior to Yul generation.
pub fn compile(analysis: &AnalyzerContext, module: &ast::Module) -> NamedYulContracts {
    mappers::module::module(analysis, module)
        .drain()
        .map(|(name, object)| (name, object.to_string().replace("\"", "\\\"")))
        .collect::<NamedYulContracts>()
}
