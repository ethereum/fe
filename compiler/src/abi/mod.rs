//! Fe to ABI builder.

use crate::errors::CompileError;
use crate::types::{
    FeModuleAst,
    NamedAbis,
};

mod builder;
pub mod utils;

/// Elements used to define contract ABIs.
pub mod elements;

/// Builds ABIs for each contract in the module.
pub fn build(module: &FeModuleAst) -> Result<NamedAbis, CompileError> {
    builder::module(module)?
        .drain()
        .map(|(name, abi)| abi.json(true).map(|json| (name, json)))
        .collect::<Result<NamedAbis, _>>()
}
