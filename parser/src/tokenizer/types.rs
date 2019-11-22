use serde::{
    Deserialize,
    Serialize,
};

use crate::span::{
    Offset,
    Position,
};

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
pub struct TokenInfo<'a> {
    /// The type of a token
    pub typ: TokenType,

    /// The text content of a parsed token
    pub string: &'a str,

    /// The beginning line/column position of a token in the original source
    /// string
    pub start_pos: Position,

    /// The global byte offset into the original source string of the beginning
    /// of a token
    pub start_off: Offset,

    /// The ending line/column position of a token in the original source
    /// string
    pub end_pos: Position,

    /// The global byte offset into the original source string of the end of a
    /// token
    pub end_off: Offset,

    /// The text content of the line from which a token was parsed
    pub line: &'a str,
}
