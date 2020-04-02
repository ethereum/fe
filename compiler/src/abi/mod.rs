use crate::errors::CompileError;
use vyper_parser as parser;


mod json_builder;

pub use json_builder::TypeDefs;

/// Builds a JSON ABI from the source file.
///
/// This API should be rethought as it only provides the ABI for the first contract in the
/// source file. See: https://github.com/ethereum/rust-vyper/issues/12
/// TODO: Improve the JSON ABI builder API.
pub fn build(src: &str) -> Result<String, CompileError> {
    let tokens = parser::get_parse_tokens(src).unwrap();
    let vyp_module = parser::parsers::file_input(&tokens[..]).unwrap().1.node;
    let contracts = json_builder::module(&vyp_module)?;

    if let Some(contract) = contracts.get(0) {
        return Ok(serde_json::to_string(&contract.items)?);
    }

    Err(CompileError::static_str(
        "No contract statements in source.",
    ))
}
