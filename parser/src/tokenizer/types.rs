use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum TokenType {
    NAME,
    NUMBER,
    STRING,
    OP,
    COMMENT,

    INDENT,
    DEDENT,

    NEWLINE, // Newlines *between* statements and strings
    NL,      // Newlines *within* statements and strings
    ENDMARKER,

    ERRORTOKEN,
}

pub type Position = (
    usize, // Line number (1-indexed)
    usize, // Column offset (0-indexed)
);

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TokenInfo<'a> {
    pub typ: TokenType,
    pub string: &'a str,
    pub start: Position,
    pub end: Position,
    pub line: &'a str,
}
