use crate::node::{Node, Span};
use logos::Logos;
use std::ops::Add;

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
    pub span: Span,
}

impl<'a> From<Token<'a>> for Node<String> {
    fn from(tok: Token<'a>) -> Node<String> {
        Node::new(tok.text.into(), tok.span)
    }
}

impl<'a> Add<&Token<'a>> for Span {
    type Output = Self;

    fn add(self, other: &Token<'a>) -> Self {
        self + other.span
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Logos)]
pub enum TokenKind {
    // Ignoring comments and spaces/tabs for now.
    // If we implement an auto-formatting tool, we'll probably want to change this.
    #[regex(r"#[^\n]*", logos::skip)]
    #[regex("[ \t]+", logos::skip)]
    #[error]
    Error,

    #[regex(r"\n[ \t]*")]
    Newline,

    /// Virtual tokens emitted by the parser
    Indent,
    Dedent,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Name,
    #[regex("[0-9]+")]
    Int,
    #[regex("0[xX][0-9a-fA-F]+")]
    Hex,
    #[regex("0[oO][0-7]+")]
    Octal,
    #[regex("0[bB][0-1]+")]
    Binary,
    // Float,
    #[regex(r#""([^"\\]|\\.)*""#)]
    #[regex(r#"'([^'\\]|\\.)*'"#)]
    Text,
    #[token("true")]
    True,
    #[token("false")]
    False,
    // #[token("None")] // ?
    // None,
    #[token("assert")]
    Assert,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("contract")]
    Contract,
    #[token("fn")]
    Fn,
    #[token("const")]
    Const,
    #[token("elif")]
    Elif,
    #[token("else")]
    Else,
    #[token("emit")]
    Emit,
    #[token("event")]
    Event,
    #[token("idx")]
    Idx,
    #[token("if")]
    If,
    #[token("import")]
    Import,
    #[token("pragma")]
    Pragma,
    #[token("pass")]
    Pass,
    #[token("for")]
    For,
    #[token("pub")]
    Pub,
    #[token("return")]
    Return,
    #[token("revert")]
    Revert,
    #[token("struct")]
    Struct,
    #[token("type")]
    Type,
    #[token("while")]
    While,

    #[token("and")]
    And,
    #[token("as")]
    As,
    #[token("in")]
    In,
    #[token("not")]
    Not,
    #[token("or")]
    Or,
    #[token("let")]
    Let,
    #[token("use")]
    Use,
    // Symbols
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("|")]
    Pipe,
    #[token("&")]
    Amper,
    #[token("<")]
    Lt,
    #[token("<<")]
    LtLt,
    #[token(">")]
    Gt,
    #[token(">>")]
    GtGt,
    #[token("=")]
    Eq,
    #[token(".")]
    Dot,
    #[token("%")]
    Percent,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("~")]
    Tilde,
    #[token("^")]
    Hat,
    #[token("**")]
    StarStar,
    #[token("**=")]
    StarStarEq,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    StarEq,
    #[token("/=")]
    SlashEq,
    #[token("%=")]
    PercentEq,
    #[token("&=")]
    AmperEq,
    #[token("|=")]
    PipeEq,
    #[token("^=")]
    HatEq,
    #[token("<<=")]
    LtLtEq,
    #[token(">>=")]
    GtGtEq,
    #[token("->")]
    Arrow,
}

impl TokenKind {
    /// Return a user-friendly description of the token kind. E.g.
    /// `TokenKind::Newline => "a newline"`
    /// Returns [`TokenKind::symbol_str`] for symbol tokens.
    pub fn friendly_str(&self) -> Option<&'static str> {
        use TokenKind::*;
        let val = match self {
            Newline => "a newline",
            Dedent => "a dedent",
            Name => "a name",
            Int => "a number",
            Hex => "a hexadecimal number",
            Octal => "an octal number",
            Binary => "a binary number",
            Text => "a string",
            _ => return self.symbol_str(),
        };
        Some(val)
    }

    /// If the token is a symbol or keyword, return the string representation.
    /// E.g. `TokenKind::EqEq => "=="`
    pub fn symbol_str(&self) -> Option<&'static str> {
        use TokenKind::*;
        let val = match self {
            True => "true",
            False => "false",
            Assert => "assert",
            Break => "break",
            Continue => "continue",
            Contract => "contract",
            Fn => "fn",
            Const => "const",
            Let => "let",
            Elif => "elif",
            Else => "else",
            Emit => "emit",
            Event => "event",
            Idx => "idx",
            If => "if",
            Import => "import",
            Pragma => "pragma",
            Pass => "pass",
            For => "for",
            Pub => "pub",
            Return => "return",
            Revert => "revert",
            Struct => "struct",
            Type => "type",
            While => "while",
            And => "and",
            As => "as",
            In => "in",
            Not => "not",
            Or => "or",
            Use => "use",
            ParenOpen => "(",
            ParenClose => ")",
            BracketOpen => "[",
            BracketClose => "]",
            BraceOpen => "{",
            BraceClose => "}",
            Colon => ":",
            ColonColon => "::",
            Comma => ",",
            Semi => "",
            Plus => "+",
            Minus => "-",
            Star => "*",
            Slash => "/",
            Pipe => "|",
            Amper => "&",
            Lt => "<",
            LtLt => "<<",
            Gt => ">",
            GtGt => ">>",
            Eq => "=",
            Dot => ".",
            Percent => "%",
            EqEq => "==",
            NotEq => "!=",
            LtEq => "<=",
            GtEq => ">=",
            Tilde => "~",
            Hat => "^",
            StarStar => "**",
            StarStarEq => "**=",
            PlusEq => "+=",
            MinusEq => "-=",
            StarEq => "*=",
            SlashEq => "/=",
            PercentEq => "%=",
            AmperEq => "&=",
            PipeEq => "|=",
            HatEq => "^=",
            LtLtEq => "<<=",
            GtGtEq => ">>=",
            Arrow => "->",
            Error | Newline | Indent | Dedent | Name | Int | Hex | Octal | Binary | Text => {
                return None
            }
        };
        Some(val)
    }
}
