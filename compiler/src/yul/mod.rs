use crate::errors::CompileError;
use vyper_parser as parser;

mod mappers;
mod namespace;
mod runtime;

/// Builds Yul code from Vyper source.
pub fn compile(src: &str) -> Result<String, CompileError> {
    let tokens = parser::get_parse_tokens(src)?;
    let vyp_module = parser::parsers::file_input(&tokens[..])?.1.node;

    // TODO: Handle multiple contracts in one Vyper module cleanly.
    if let Some(first_contract) = mappers::module::module(&vyp_module)?.get(0) {
//        panic!(first_contract.to_string());
        return Ok(first_contract.to_string());
    }

    Err(CompileError::static_str("Unable to parse tokens."))
}
