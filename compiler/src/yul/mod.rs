use crate::errors::CompileError;
use vyper_parser as parser;

mod ast_builder;
mod base;
mod constructor;
mod selectors;

/// Builds Yul code from Vyper source.
pub fn compile(src: &str) -> Result<String, CompileError> {
    let tokens = parser::get_parse_tokens(src)?;
    let vyp_module = parser::parsers::file_input(&tokens[..])?.1.node;

    // TODO: Handle multiple contracts in one Vyper module cleanly.
    if let Some(first_contract) = ast_builder::module(&vyp_module)?.get(0) {
        return Ok(first_contract.to_string());
    }

    Err(CompileError::static_str("Unable to parse tokens."))
}
