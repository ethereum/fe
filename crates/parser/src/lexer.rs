mod token;
use crate::node::Span;
use fe_common::files::SourceFileId;
use logos::Logos;
pub use token::{Token, TokenKind};

#[derive(Clone)]
pub struct Lexer<'a> {
    file_id: SourceFileId,
    inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer with the given source code string.
    pub fn new(file_id: SourceFileId, src: &'a str) -> Lexer {
        Lexer {
            file_id,
            inner: TokenKind::lexer(src),
        }
    }

    /// Return the full source code string that's being tokenized.
    pub fn source(&self) -> &'a str {
        self.inner.source()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();
        let span = self.inner.span();
        Some(Token {
            kind,
            text,
            span: Span {
                file_id: self.file_id,
                start: span.start,
                end: span.end,
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, TokenKind};
    use fe_common::files::SourceFileId;
    use TokenKind::*;

    fn check(input: &str, expected: &[TokenKind]) {
        let lex = Lexer::new(SourceFileId::dummy_file(), input);

        let actual = lex.map(|t| t.kind).collect::<Vec<_>>();

        assert!(
            actual.iter().eq(expected.iter()),
            "\nexpected: {expected:?}\n  actual: {actual:?}"
        );
    }

    #[test]
    fn basic() {
        check(
            "contract Foo:\n  x: u32\n  fn f() -> u32: not x",
            &[
                Contract, Name, Colon, Newline, Name, Colon, Name, Newline, Fn, Name, ParenOpen,
                ParenClose, Arrow, Name, Colon, Not, Name,
            ],
        );
    }

    #[test]
    fn strings() {
        let rawstr = r#""string \t with \n escapes \" \"""#;
        let mut lex = Lexer::new(SourceFileId::dummy_file(), rawstr);
        let lexedstr = lex.next().unwrap();
        assert!(lexedstr.kind == Text);
        assert!(lexedstr.text == rawstr);
        assert!(lex.next().is_none());
    }

    #[test]
    fn errors() {
        check(
            "contract Foo@ 5u8 \n  self.bar",
            &[
                Contract, Name, Error, Int, Name, Newline, SelfValue, Dot, Name,
            ],
        );
    }

    #[test]
    fn tabs_and_comment() {
        check(
            "\n\t \tcontract\n\tFoo // hi mom!\n ",
            &[Newline, Contract, Newline, Name, Newline],
        );
    }
}
