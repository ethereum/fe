use vyper_parser::errors::ParseError;
use vyper_parser::tokenizer::TokenizeError;

#[derive(Debug)]
pub enum ErrorKind {
    StaticStr(&'static str),
    Str(String),
}

#[derive(Debug)]
pub struct CompileError {
    errors: Vec<ErrorKind>,
}

impl CompileError {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn static_str(s: &'static str) -> Self {
        Self {
            errors: vec![ErrorKind::StaticStr(s)],
        }
    }

    pub fn str(s: String) -> Self {
        Self {
            errors: vec![ErrorKind::Str(s)],
        }
    }
}

impl<'a> From<ParseError<'a>> for CompileError {
    fn from(_: ParseError<'a>) -> Self {
        CompileError::static_str("Parser error")
    }
}

impl<'a> From<TokenizeError> for CompileError {
    fn from(_: TokenizeError) -> Self {
        CompileError::static_str("Tokenize error")
    }
}

impl<'a> From<serde_json::error::Error> for CompileError {
    fn from(_: serde_json::error::Error) -> Self {
        CompileError::static_str("JSON serialization error")
    }
}
