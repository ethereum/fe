pub mod lexer;
pub mod parser;
pub mod syntax_kind;
pub mod syntax_node;

pub use syntax_kind::SyntaxKind;

use parser::RootScope;
use syntax_node::SyntaxNode;

pub type TextRange = rowan::TextRange;

pub fn parse_source_file(text: &str) -> (SyntaxNode, Vec<ParseError>) {
    let lexer = lexer::Lexer::new(text);
    let mut parser = parser::Parser::new(lexer);
    let checkpoint = parser.enter(RootScope::default(), None);

    parser.parse(parser::ItemListScope::default(), None);

    parser.leave(checkpoint);
    parser.finish()
}

/// An parse error which is accumulated in the [`parser::Parser`] while parsing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError {
    /// An error message.
    pub msg: String,

    /// A range of the error.
    pub range: TextRange,
}
