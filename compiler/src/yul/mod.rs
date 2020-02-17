use crate::errors::CompileError;
use vyper_parser as parser;

mod base;
mod constructor;
mod ast_builder;
mod selectors;

pub fn compile(src: &str) -> Result<String, CompileError> {
    let tokens = parser::get_parse_tokens(src)?;
    let vyp_module = parser::parsers::file_input(&tokens[..])?.1.node;
    if let Ok(yul_ast) = ast_builder::module(&vyp_module) {
        return Ok(yul_ast.to_string());
    }

    Err(CompileError::static_str("Unable to parse tokens."))
}
