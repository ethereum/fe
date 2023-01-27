pub use fe_common::diagnostics::Label;
use fe_common::diagnostics::{Diagnostic, Severity};
use fe_common::files::SourceFileId;

use crate::lexer::{Lexer, Token, TokenKind};
use crate::node::Span;
use std::{error, fmt};

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct ParseFailed;
impl fmt::Display for ParseFailed {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(fmt, "ParseFailed")
    }
}
impl error::Error for ParseFailed {}

pub type ParseResult<T> = Result<T, ParseFailed>;

/// `Parser` maintains the parsing state, such as the token stream,
/// "enclosure" (paren, brace, ..) stack, diagnostics, etc.
/// Syntax parsing logic is in the [`crate::grammar`] module.
///
/// See [`BTParser`] if you need backtrackable parser.
pub struct Parser<'a> {
    pub file_id: SourceFileId,
    lexer: Lexer<'a>,

    /// Tokens that have been "peeked", or split from a larger token.
    /// Eg. `>>` may be split into two `>` tokens when parsing the end of a
    /// generic type parameter list (eg. `Map<u256, Map<u256, address>>`).
    buffered: Vec<Token<'a>>,

    enclosure_stack: Vec<Enclosure>,

    /// The diagnostics (errors and warnings) emitted during parsing.
    pub diagnostics: Vec<Diagnostic>,
}

impl<'a> Parser<'a> {
    /// Create a new parser for a source code string and associated file id.
    pub fn new(file_id: SourceFileId, content: &'a str) -> Self {
        Parser {
            file_id,
            lexer: Lexer::new(file_id, content),
            buffered: vec![],
            enclosure_stack: vec![],
            diagnostics: vec![],
        }
    }

    /// Returns back tracking parser.
    pub fn as_bt_parser<'b>(&'b mut self) -> BTParser<'a, 'b> {
        BTParser::new(self)
    }

    /// Return the next token, or an error if we've reached the end of the file.
    #[allow(clippy::should_implement_trait)] // next() is a nice short name for a common task
    pub fn next(&mut self) -> ParseResult<Token<'a>> {
        self.eat_newlines_if_in_nonblock_enclosure();
        if let Some(tok) = self.next_raw() {
            if is_enclosure_open(tok.kind) {
                self.enclosure_stack
                    .push(Enclosure::non_block(tok.kind, tok.span));
            } else if is_enclosure_close(tok.kind) {
                if let Some(open) = self.enclosure_stack.pop() {
                    if !enclosure_tokens_match(open.token_kind, tok.kind) {
                        // TODO: we should search the enclosure_stack
                        //  for the last matching enclosure open token.
                        //  If any enclosures are unclosed, we should emit an
                        //  error, and somehow close their respective ast nodes.
                        //  We could synthesize the correct close token, or emit
                        //  a special TokenKind::UnclosedEnclosure or whatever.
                        todo!()
                    }
                } else {
                    self.error(tok.span, format!("Unmatched `{}`", tok.text));
                }
            }
            Ok(tok)
        } else {
            self.error(
                Span::new(
                    self.file_id,
                    self.lexer.source().len(),
                    self.lexer.source().len(),
                ),
                "unexpected end of file",
            );
            Err(ParseFailed)
        }
    }

    fn next_raw(&mut self) -> Option<Token<'a>> {
        self.buffered.pop().or_else(|| self.lexer.next())
    }

    /// Take a peek at the next token kind without consuming it, or return an
    /// error if we've reached the end of the file.
    pub fn peek_or_err(&mut self) -> ParseResult<TokenKind> {
        self.eat_newlines_if_in_nonblock_enclosure();
        if let Some(tk) = self.peek_raw() {
            Ok(tk)
        } else {
            let index = self.lexer.source().len();
            self.error(
                Span::new(self.file_id, index, index),
                "unexpected end of file",
            );
            Err(ParseFailed)
        }
    }

    /// Take a peek at the next token kind. Returns `None` if we've reached the
    /// end of the file.
    pub fn peek(&mut self) -> Option<TokenKind> {
        self.eat_newlines_if_in_nonblock_enclosure();
        self.peek_raw()
    }

    fn peek_raw(&mut self) -> Option<TokenKind> {
        if self.buffered.is_empty() {
            if let Some(tok) = self.lexer.next() {
                self.buffered.push(tok);
            } else {
                return None;
            }
        }
        Some(self.buffered.last().unwrap().kind)
    }

    fn eat_newlines_if_in_nonblock_enclosure(&mut self) {
        // TODO: allow newlines inside angle brackets?
        //   eg `fn f(x: map\n <\n u8\n, ...`
        if let Some(enc) = self.enclosure_stack.last() {
            if !enc.is_block {
                self.eat_newlines();
            }
        }
    }

    /// Split the next token into two tokens, returning the first. Only supports
    /// splitting the `>>` token into two `>` tokens, specifically for
    /// parsing the closing angle bracket of a generic type argument list
    /// (`Map<x, Map<y, z>>`).
    ///
    /// # Panics
    /// Panics if the next token isn't `>>`
    pub fn split_next(&mut self) -> ParseResult<Token<'a>> {
        let gtgt = self.next()?;
        assert_eq!(gtgt.kind, TokenKind::GtGt);

        let (gt1, gt2) = gtgt.text.split_at(1);
        self.buffered.push(Token {
            kind: TokenKind::Gt,
            text: gt2,
            span: Span::new(self.file_id, gtgt.span.start + 1, gtgt.span.end),
        });

        Ok(Token {
            kind: TokenKind::Gt,
            text: gt1,
            span: Span::new(self.file_id, gtgt.span.start, gtgt.span.end - 1),
        })
    }

    /// Returns `true` if the parser has reached the end of the file.
    pub fn done(&mut self) -> bool {
        self.peek_raw().is_none()
    }

    pub fn eat_newlines(&mut self) {
        while self.peek_raw() == Some(TokenKind::Newline) {
            self.next_raw();
        }
    }

    /// Assert that the next token kind it matches the expected token
    /// kind, and return it. This should be used in cases where the next token
    /// kind is expected to have been checked already.
    ///
    /// # Panics
    /// Panics if the next token kind isn't `tk`.
    pub fn assert(&mut self, tk: TokenKind) -> Token<'a> {
        let tok = self.next().unwrap();
        assert_eq!(tok.kind, tk, "internal parser error");
        tok
    }

    /// If the next token matches the expected kind, return it. Otherwise emit
    /// an error diagnostic with the given message and return an error.
    pub fn expect<S: Into<String>>(
        &mut self,
        expected: TokenKind,
        message: S,
    ) -> ParseResult<Token<'a>> {
        self.expect_with_notes(expected, message, |_| Vec::new())
    }

    /// Like [`Parser::expect`], but with additional notes to be appended to the
    /// bottom of the diagnostic message. The notes are provided by a
    /// function that returns a `Vec<String>`, to avoid allocations in the
    /// case where the token is as expected.
    pub fn expect_with_notes<Str, NotesFn>(
        &mut self,
        expected: TokenKind,
        message: Str,
        notes_fn: NotesFn,
    ) -> ParseResult<Token<'a>>
    where
        Str: Into<String>,
        NotesFn: FnOnce(&Token) -> Vec<String>,
    {
        let tok = self.next()?;
        if tok.kind == expected {
            Ok(tok)
        } else {
            self.fancy_error(
                message.into(),
                vec![Label::primary(
                    tok.span,
                    format!(
                        "expected {}, found {}",
                        expected.describe(),
                        tok.kind.describe()
                    ),
                )],
                notes_fn(&tok),
            );
            Err(ParseFailed)
        }
    }

    /// If the next token matches the expected kind, return it. Otherwise return
    /// None.
    pub fn optional(&mut self, kind: TokenKind) -> Option<Token<'a>> {
        if self.peek() == Some(kind) {
            Some(self.next().unwrap())
        } else {
            None
        }
    }

    /// Emit an "unexpected token" error diagnostic with the given message.
    pub fn unexpected_token_error<S: Into<String>>(
        &mut self,
        tok: &Token,
        message: S,
        notes: Vec<String>,
    ) {
        self.fancy_error(
            message,
            vec![Label::primary(tok.span, "unexpected token")],
            notes,
        );
    }

    /// Enter a "block", which is a brace-enclosed list of statements,
    /// separated by newlines and/or semicolons.
    /// This checks for and consumes the `{` that precedes the block.
    pub fn enter_block(&mut self, context_span: Span, context_name: &str) -> ParseResult<()> {
        if self.peek_raw() == Some(TokenKind::BraceOpen) {
            let tok = self.next_raw().unwrap();
            self.enclosure_stack.push(Enclosure::block(tok.span));
            self.eat_newlines();
            Ok(())
        } else {
            self.fancy_error(
                format!("{context_name} must start with `{{`"),
                vec![Label::primary(
                    Span::new(self.file_id, context_span.end, context_span.end),
                    "expected `{` here",
                )],
                vec![],
            );
            Err(ParseFailed)
        }
    }

    /// Consumes newlines and semicolons. Returns Ok if one or more newlines or
    /// semicolons are consumed, or if the next token is a `}`.
    pub fn expect_stmt_end(&mut self, context_name: &str) -> ParseResult<()> {
        let mut newline = false;
        while matches!(self.peek_raw(), Some(TokenKind::Newline | TokenKind::Semi)) {
            newline = true;
            self.next_raw().unwrap();
        }
        if newline {
            return Ok(());
        }
        match self.peek_raw() {
            Some(TokenKind::BraceClose) => Ok(()),
            Some(_) => {
                let tok = self.next()?;
                self.unexpected_token_error(
                    &tok,
                    format!("unexpected token while parsing {context_name}"),
                    vec![format!(
                        "expected a newline; found {} instead",
                        tok.kind.describe()
                    )],
                );
                Err(ParseFailed)
            }
            None => Ok(()), // unexpect eof error will be generated be parent block
        }
    }

    /// Emit an error diagnostic, but don't stop parsing
    pub fn error<S: Into<String>>(&mut self, span: Span, message: S) {
        self.diagnostics.push(Diagnostic {
            severity: Severity::Error,
            message: message.into(),
            labels: vec![Label::primary(span, "")],
            notes: vec![],
        })
    }

    /// Emit a "fancy" error diagnostic with any number of labels and notes,
    /// but don't stop parsing.
    pub fn fancy_error<S: Into<String>>(
        &mut self,
        message: S,
        labels: Vec<Label>,
        notes: Vec<String>,
    ) {
        self.diagnostics.push(Diagnostic {
            severity: Severity::Error,
            message: message.into(),
            labels,
            notes,
        })
    }
}

/// A thin wrapper that makes [`Parser`] backtrackable.
pub struct BTParser<'a, 'b> {
    snapshot: &'b mut Parser<'a>,
    parser: Parser<'a>,
}

impl<'a, 'b> BTParser<'a, 'b> {
    pub fn new(snapshot: &'b mut Parser<'a>) -> Self {
        let parser = Parser {
            file_id: snapshot.file_id,
            lexer: snapshot.lexer.clone(),
            buffered: snapshot.buffered.clone(),
            enclosure_stack: snapshot.enclosure_stack.clone(),
            diagnostics: Vec::new(),
        };
        Self { snapshot, parser }
    }

    pub fn accept(self) {
        self.snapshot.lexer = self.parser.lexer;
        self.snapshot.buffered = self.parser.buffered;
        self.snapshot.enclosure_stack = self.parser.enclosure_stack;
        self.snapshot.diagnostics.extend(self.parser.diagnostics);
    }
}

impl<'a, 'b> std::ops::Deref for BTParser<'a, 'b> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a, 'b> std::ops::DerefMut for BTParser<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parser
    }
}

/// The start position of a chunk of code enclosed by (), [], or {}.
/// (This has nothing to do with "closures".)
///
/// Note that <> isn't currently considered an enclosure, as `<` might be
/// a less-than operator, while the rest are unambiguous.
///
/// A `{}` enclosure may or may not be a block. A block contains a list of
/// statements, separated by newlines or semicolons (or both).
/// Semicolons and newlines are consumed by `par.expect_stmt_end()`.
///
/// Non-block enclosures contains zero or more expressions, maybe separated by
/// commas. When the top-most enclosure is a non-block, newlines are ignored
/// (by `par.next()`), and semicolons are just normal tokens that'll be
/// rejected by the parsing fns in grammar/.
#[derive(Clone, Debug)]
struct Enclosure {
    token_kind: TokenKind,
    _token_span: Span, // TODO: mark mismatched tokens
    is_block: bool,
}
impl Enclosure {
    pub fn block(token_span: Span) -> Self {
        Self {
            token_kind: TokenKind::BraceOpen,
            _token_span: token_span,
            is_block: true,
        }
    }
    pub fn non_block(token_kind: TokenKind, token_span: Span) -> Self {
        Self {
            token_kind,
            _token_span: token_span,
            is_block: false,
        }
    }
}

fn is_enclosure_open(tk: TokenKind) -> bool {
    use TokenKind::*;
    matches!(tk, ParenOpen | BraceOpen | BracketOpen)
}

fn is_enclosure_close(tk: TokenKind) -> bool {
    use TokenKind::*;
    matches!(tk, ParenClose | BraceClose | BracketClose)
}

fn enclosure_tokens_match(open: TokenKind, close: TokenKind) -> bool {
    use TokenKind::*;
    matches!(
        (open, close),
        (ParenOpen, ParenClose) | (BraceOpen, BraceClose) | (BracketOpen, BracketClose)
    )
}
