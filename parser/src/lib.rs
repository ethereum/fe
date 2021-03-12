extern crate regex;

pub mod ast;
pub mod builders;
pub mod errors;
pub mod node;
pub mod parsers;
pub mod string_utils;
pub mod tokenizer;

mod ast_traits;

use errors::ParseError;
use tokenizer::{
    tokenize,
    Token,
    TokenType,
    TokenizeError,
};

pub type Cursor<'a> = &'a [Token<'a>];
pub type ParseResult<'a, O> = Result<(Cursor<'a>, O), ParseError<'a>>;

/// Tokenize the given source code in `source` and filter out tokens not
/// relevant to parsing.
pub fn get_parse_tokens(source: &str) -> Result<Vec<Token>, TokenizeError> {
    let tokens = tokenize(source)?;

    Ok(tokens
        .into_iter()
        .filter(|t| t.typ != TokenType::NL && t.typ != TokenType::COMMENT)
        .collect())
}
