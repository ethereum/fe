use crate::node::Node;
use crate::node::Span;
use logos::Logos;
use smol_str::SmolStr;
use std::ops::Add;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
    pub span: Span,
}

impl<'a> From<Token<'a>> for Node<SmolStr> {
    fn from(tok: Token<'a>) -> Node<SmolStr> {
        Node::new(tok.text.into(), tok.span)
    }
}

impl<'a> Add<&Token<'a>> for Span {
    type Output = Self;

    fn add(self, other: &Token<'a>) -> Self {
        self + other.span
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Logos)]
pub enum TokenKind {
    // Ignoring comments and spaces/tabs for now.
    // If we implement an auto-formatting tool, we'll probably want to change this.
    #[regex(r"//[^\n]*", logos::skip)]
    #[regex("[ \t]+", logos::skip)]
    #[error]
    Error,

    #[regex(r"\n[ \t]*")]
    Newline,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Name,
    #[regex("[0-9]+(?:_[0-9]+)*")]
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
    #[token("else")]
    Else,
    #[token("idx")]
    Idx,
    #[token("if")]
    If,
    #[token("match")]
    Match,
    #[token("impl")]
    Impl,
    #[token("pragma")]
    Pragma,
    #[token("for")]
    For,
    #[token("pub")]
    Pub,
    #[token("return")]
    Return,
    #[token("revert")]
    Revert,
    #[token("Self")]
    SelfType,
    #[token("self")]
    SelfValue,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("trait")]
    Trait,
    #[token("type")]
    Type,
    #[token("unsafe")]
    Unsafe,
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
    #[token("mut")]
    Mut,
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
    #[token("#")]
    Hash,
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
    #[token("..")]
    DotDot,
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
    #[token("=>")]
    FatArrow,
}

impl TokenKind {
    /// Return a user-friendly description of the token kind. E.g.
    /// TokenKind::Newline => "a newline"
    /// TokenKind::Colon => "`:`"
    pub fn describe(&self) -> &str {
        use TokenKind::*;
        match self {
            Newline => "a newline",
            Name => "a name",
            Int => "a number",
            Hex => "a hexadecimal number",
            Octal => "an octal number",
            Binary => "a binary number",
            Text => "a string",

            True => "keyword `true`",
            False => "keyword `false`",
            Assert => "keyword `assert`",
            Break => "keyword `break`",
            Continue => "keyword `continue`",
            Contract => "keyword `contract`",
            Fn => "keyword `fn`",
            Const => "keyword `const`",
            Let => "keyword `let`",
            Mut => "keyword `mut`",
            Else => "keyword `else`",
            Idx => "keyword `idx`",
            If => "keyword `if`",
            Match => "keyword `match`",
            Impl => "keyword `impl`",
            Pragma => "keyword `pragma`",
            For => "keyword `for`",
            Pub => "keyword `pub`",
            Return => "keyword `return`",
            Revert => "keyword `revert`",
            SelfType => "keyword `Self`",
            SelfValue => "keyword `self`",
            Struct => "keyword `struct`",
            Enum => "keyword `enum`",
            Trait => "keyword `trait`",
            Type => "keyword `type`",
            Unsafe => "keyword `unsafe`",
            While => "keyword `while`",
            And => "keyword `and`",
            As => "keyword `as`",
            In => "keyword `in`",
            Not => "keyword `not`",
            Or => "keyword `or`",
            Use => "keyword `use`",
            ParenOpen => "symbol `(`",
            ParenClose => "symbol `)`",
            BracketOpen => "symbol `[`",
            BracketClose => "symbol `]`",
            BraceOpen => "symbol `{`",
            BraceClose => "symbol `}`",
            Colon => "symbol `:`",
            ColonColon => "symbol `::`",
            Comma => "symbol `,`",
            Hash => "symbol `#`",
            Semi => "symbol `;`",
            Plus => "symbol `+`",
            Minus => "symbol `-`",
            Star => "symbol `*`",
            Slash => "symbol `/`",
            Pipe => "symbol `|`",
            Amper => "symbol `&`",
            Lt => "symbol `<`",
            LtLt => "symbol `<<`",
            Gt => "symbol `>`",
            GtGt => "symbol `>>`",
            Eq => "symbol `=`",
            Dot => "symbol `.`",
            DotDot => "symbol `..`",
            Percent => "symbol `%`",
            EqEq => "symbol `==`",
            NotEq => "symbol `!=`",
            LtEq => "symbol `<=`",
            GtEq => "symbol `>=`",
            Tilde => "symbol `~`",
            Hat => "symbol `^`",
            StarStar => "symbol `**`",
            StarStarEq => "symbol `**=`",
            PlusEq => "symbol `+=`",
            MinusEq => "symbol `-=`",
            StarEq => "symbol `*=`",
            SlashEq => "symbol `/=`",
            PercentEq => "symbol `%=`",
            AmperEq => "symbol `&=`",
            PipeEq => "symbol `|=`",
            HatEq => "symbol `^=`",
            LtLtEq => "symbol `<<=`",
            GtGtEq => "symbol `>>=`",
            Arrow => "symbol `->`",
            FatArrow => "symbol `=>`",

            Error => unreachable!(), // TODO this is reachable
        }
    }
}
