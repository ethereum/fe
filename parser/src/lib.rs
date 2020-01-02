extern crate regex;

pub mod ast;
pub mod builders;
pub mod errors;
pub mod parsers;
pub mod span;
pub mod string_utils;
pub mod tokenizer;

use errors::ParseError;
use tokenizer::Token;

pub type Cursor<'a> = &'a [Token<'a>];
pub type ParseResult<'a, O> = Result<(Cursor<'a>, O), ParseError<'a>>;
