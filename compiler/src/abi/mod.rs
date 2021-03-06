//! Fe to ABI builder.

use crate::errors::CompileError;
use crate::types::{
    FeModuleAst,
    NamedAbis,
};
use fe_analyzer::Context;

mod builder;
pub mod utils;

/// Elements used to define contract ABIs.
pub mod elements;

/// Builds ABIs for each contract in the module.
pub fn build(context: &Context, module: &FeModuleAst) -> Result<NamedAbis, CompileError> {
    builder::module(context, module)?
        .drain()
        .map(|(name, abi)| abi.json(true).map(|json| (name, json)))
        .collect::<Result<NamedAbis, _>>()
}
