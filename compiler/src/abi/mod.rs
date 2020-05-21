//! Vyper to ABI builder.

use crate::errors::CompileError;
use vyper_parser as parser;

mod builder;
pub mod utils;

/// Elements used to define contract ABIs.
pub mod elements;

/// Builds the appropriate ABIs for a given source `&str`.
pub fn build(src: &str) -> Result<elements::ModuleABIs, CompileError> {
    let tokens = parser::get_parse_tokens(src)?;
    let module = parser::parsers::file_input(&tokens[..])?.1.node;

    builder::module(&module)
}
