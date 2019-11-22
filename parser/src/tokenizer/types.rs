use serde::{
    Deserialize,
    Serialize,
};

use crate::span::Span;

#[derive(Serialize, Deserialize, Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    NAME,
    NUMBER,
    STRING,
    OP,
    COMMENT,

    INDENT,
    DEDENT,

    NEWLINE, // Grammatically significant newlines
    NL,      // Whitespace newlines (useful in case source should be reconstructed from tokens)
    ENDMARKER,

    ERRORTOKEN,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Token<'a> {
    /// The type of a token
    pub typ: TokenType,

    /// The text content of a parsed token
    pub string: &'a str,

    /// The span of source text covered by a token.
    pub span: Span,

    /// The text content of the line from which a token was parsed
    pub line: &'a str,
}
