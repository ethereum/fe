//! Fe to ABI builder.

use fe_analyzer::context::Context;
use fe_parser::ast;
use std::collections::HashMap;

mod builder;
pub mod utils;

/// Elements used to define contract ABIs.
pub mod elements;

mod errors;
pub use errors::AbiError;

/// A mapping of contract names and their ABIs.
pub type NamedAbis = HashMap<ContractName, JsonAbi>;
/// The ABI of a contract as a string.
pub type JsonAbi = String;
/// The name of a Fe contract.
pub type ContractName = String;

/// Builds ABIs for each contract in the module.
pub fn build(context: &Context, module: &ast::Module) -> Result<NamedAbis, AbiError> {
    builder::module(context, module)?
        .drain()
        .map(|(name, abi)| abi.json(true).map(|json| (name, json)))
        .collect::<Result<NamedAbis, _>>()
}
