//! Fe to Yul compiler.

use crate::errors::CompileError;

mod mappers;
mod runtime;

/// Compiles Fe to Yul.
pub fn compile(src: &str) -> Result<String, CompileError> {
    let tokens = fe_parser::get_parse_tokens(src)?;
    let fe_module = fe_parser::parsers::file_input(&tokens[..])?.1.node;
    let context = fe_semantics::analysis(&fe_module)?;

    // TODO: Handle multiple contracts in one Fe module cleanly.
    if let Some(first_contract) = mappers::module::module(&context, &fe_module)?
        .values()
        .next()
    {
        return Ok(first_contract.to_string());
    }

    Err(CompileError::static_str("unable to parse tokens."))
}
