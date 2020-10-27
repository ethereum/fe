//! Fe to Yul compiler.

use crate::errors::CompileError;

mod constructor;
mod mappers;
mod operations;
mod runtime;

pub struct CompilerOutput {
    pub tokens: String,
    pub ast: String,
    pub yul: String,
}

/// Compiles Fe to Yul.
pub fn compile(src: &str) -> Result<CompilerOutput, CompileError> {
    let tokens = fe_parser::get_parse_tokens(src)?;
    let fe_module = fe_parser::parsers::file_input(&tokens[..])?.1.node;
    let context = fe_semantics::analysis(&fe_module)?;

    // TODO: Handle multiple contracts in one Fe module cleanly.
    if let Some(first_contract) = mappers::module::module(&context, &fe_module)?
        .values()
        .next()
    {
        return Ok(CompilerOutput {
            tokens: format!("{:?}", tokens),
            ast: format!("{:?}", fe_module),
            yul: first_contract.to_string(),
        });
    }

    Err(CompileError::static_str("unable to parse tokens."))
}
