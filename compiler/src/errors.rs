//! Errors returned by the compilers and ABI builder.

use fe_parser::errors::ParseError;
use fe_parser::tokenizer::TokenizeError;

/// Errors can either be an object or static reference.
#[derive(Debug)]
pub enum ErrorKind {
    StaticStr(&'static str),
    Str(String),
}

/// List of errors encountered during compilation.
#[derive(Debug)]
pub struct CompileError {
    errors: Vec<ErrorKind>,
}

impl Default for CompileError {
    fn default() -> Self {
        Self::new()
    }
}

impl CompileError {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Create a single error with a static string.
    pub fn static_str(s: &'static str) -> Self {
        Self {
            errors: vec![ErrorKind::StaticStr(s)],
        }
    }

    /// Create a single error with a string object.
    pub fn str(s: String) -> Self {
        Self {
            errors: vec![ErrorKind::Str(s)],
        }
    }
}

impl<'a> From<ParseError<'a>> for CompileError {
    fn from(_: ParseError<'a>) -> Self {
        CompileError::static_str("parser error")
    }
}

impl<'a> From<TokenizeError> for CompileError {
    fn from(_: TokenizeError) -> Self {
        CompileError::static_str("tokenize error")
    }
}

impl<'a> From<serde_json::error::Error> for CompileError {
    fn from(_: serde_json::error::Error) -> Self {
        CompileError::static_str("JSON serialization error")
    }
}

impl<'a> From<ethabi::Error> for CompileError {
    fn from(e: ethabi::Error) -> Self {
        CompileError::str(format!("ethabi error: {}", e))
    }
}
