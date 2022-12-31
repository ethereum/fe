pub mod lexer;
pub mod parser;
pub mod syntax_kind;
pub mod syntax_node;

pub use syntax_kind::SyntaxKind;

pub type TextRange = rowan::TextRange;

/// An parse error which is accumulated in the [`parser::Parser`] while parsing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError {
    /// An error message.
    pub msg: String,

    /// A range of the error.
    pub range: TextRange,
}
