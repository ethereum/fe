pub mod ast;
pub mod lexer;
pub mod parser;
pub mod syntax_kind;
pub mod syntax_node;

use rowan::TextSize;
use smallvec::SmallVec;
pub use syntax_kind::SyntaxKind;
pub use syntax_node::{FeLang, GreenNode, NodeOrToken, SyntaxNode, SyntaxToken, TextRange};

use parser::RootScope;

pub fn parse_source_file(text: &str) -> (GreenNode, Vec<ParseError>) {
    let lexer = lexer::Lexer::new(text);
    let mut parser = parser::Parser::new(lexer);
    let checkpoint = parser.enter(RootScope::default(), None);

    let _ = parser.parse(parser::ItemListScope::default());

    parser.leave(checkpoint);
    let (node, errs) = parser.finish();
    (node, errs)
}

/// An parse error which is accumulated in the [`parser::Parser`] while parsing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    Expected(SmallVec<[SyntaxKind; 2]>, Option<String>, TextSize),
    Unexpected(TextRange),

    Msg(String, TextRange),
}

impl ParseError {
    pub fn expected(tokens: &[SyntaxKind], msg: Option<&str>, pos: TextSize) -> Self {
        ParseError::Expected(
            SmallVec::from_slice(tokens),
            msg.map(|s| s.to_string()),
            pos,
        )
    }

    pub fn msg(&self) -> String {
        match self {
            ParseError::Expected(_, Some(msg), _) => msg.clone(),
            ParseError::Expected(tokens, None, _) => {
                if tokens.len() == 1 {
                    return format!("expected {}", tokens[0].describe());
                }

                let mut s = "expected ".to_string();
                let mut delim = "";
                for (i, t) in tokens.iter().enumerate() {
                    s.push_str(delim);
                    s.push_str(t.describe());

                    delim = if i + 2 == tokens.len() { " or " } else { ", " };
                }
                s
            }
            ParseError::Unexpected(_) => "unexpected syntax".into(),
            ParseError::Msg(m, _) => m.clone(),
        }
    }

    pub fn range(&self) -> TextRange {
        match self {
            ParseError::Expected(_, _, pos) => TextRange::empty(*pos),
            ParseError::Unexpected(r) | ParseError::Msg(_, r) => *r,
        }
    }
}
