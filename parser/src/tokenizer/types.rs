use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum TokenType {
    EndMarker = 0,
    Name = 1,
    Number = 2,
    String = 3,
    Newline = 4,
    Indent = 5,
    Dedent = 6,
    LPar = 7,
    RPar = 8,
    LSqb = 9,
    RSqb = 10,
    Colon = 11,
    Comma = 12,
    Semi = 13,
    Plus = 14,
    Minus = 15,
    Star = 16,
    Slash = 17,
    VBar = 18,
    Amper = 19,
    Less = 20,
    Greater = 21,
    Equal = 22,
    Dot = 23,
    Percent = 24,
    LBrace = 25,
    RBrace = 26,
    EqEqual = 27,
    NotEqual = 28,
    LessEqual = 29,
    GreaterEqual = 30,
    Tilde = 31,
    Circumflex = 32,
    LeftShift = 33,
    RightShift = 34,
    DoubleStar = 35,
    PlusEqual = 36,
    MinEqual = 37,
    StarEqual = 38,
    SlashEqual = 39,
    PercentEqual = 40,
    AmperEqual = 41,
    VBarEqual = 42,
    CircumflexEqual = 43,
    LeftShiftEqual = 44,
    RightShiftEqual = 45,
    DoubleStarEqual = 46,
    DoubleSlash = 47,
    DoubleSlashEqual = 48,
    At = 49,
    AtEqual = 50,
    RArrow = 51,
    Ellipsis = 52,
    ColonEqual = 53,
    Op = 54,
    Await = 55,
    Async = 56,
    TypeIgnore = 57,
    TypeComment = 58,
    ErrorToken = 59,
    // These aren't used by the C tokenizer but are needed for tokenize.py
    Comment = 60,
    NL = 61,
    Encoding = 62,
    NTokens = 63,
    // Special definitions for cooperation with parser
    NTOffset = 256,
}

impl TokenType {
    pub fn from_one_char(c: char) -> Self {
        match c {
            '%' => Self::Percent,
            '&' => Self::Amper,
            '(' => Self::LPar,
            ')' => Self::RPar,
            '*' => Self::Star,
            '+' => Self::Plus,
            ',' => Self::Comma,
            '-' => Self::Minus,
            '.' => Self::Dot,
            '/' => Self::Slash,
            ':' => Self::Colon,
            ';' => Self::Semi,
            '<' => Self::Less,
            '=' => Self::Equal,
            '>' => Self::Greater,
            '@' => Self::At,
            '[' => Self::LSqb,
            ']' => Self::RSqb,
            '^' => Self::Circumflex,
            '{' => Self::LBrace,
            '|' => Self::VBar,
            '}' => Self::RBrace,
            '~' => Self::Tilde,
            _ => Self::Op,
        }
    }

    pub fn from_two_chars(c1: char, c2: char) -> Self {
        match c1 {
            '!' => match c2 {
                '=' => Self::NotEqual,
                _ => Self::Op,
            },
            '%' => match c2 {
                '=' => Self::PercentEqual,
                _ => Self::Op,
            },
            '&' => match c2 {
                '=' => Self::AmperEqual,
                _ => Self::Op,
            },
            '*' => match c2 {
                '*' => Self::DoubleStar,
                '=' => Self::StarEqual,
                _ => Self::Op,
            },
            '+' => match c2 {
                '=' => Self::PlusEqual,
                _ => Self::Op,
            },
            '-' => match c2 {
                '=' => Self::MinEqual,
                '>' => Self::RArrow,
                _ => Self::Op,
            },
            '/' => match c2 {
                '/' => Self::DoubleSlash,
                '=' => Self::SlashEqual,
                _ => Self::Op,
            },
            ':' => match c2 {
                '=' => Self::ColonEqual,
                _ => Self::Op,
            },
            '<' => match c2 {
                '<' => Self::LeftShift,
                '=' => Self::LessEqual,
                '>' => Self::NotEqual,
                _ => Self::Op,
            },
            '=' => match c2 {
                '=' => Self::EqEqual,
                _ => Self::Op,
            },
            '>' => match c2 {
                '=' => Self::GreaterEqual,
                '>' => Self::RightShift,
                _ => Self::Op,
            },
            '@' => match c2 {
                '=' => Self::AtEqual,
                _ => Self::Op,
            },
            '^' => match c2 {
                '=' => Self::CircumflexEqual,
                _ => Self::Op,
            },
            '|' => match c2 {
                '=' => Self::VBarEqual,
                _ => Self::Op,
            },
            _ => Self::Op,
        }
    }

    pub fn from_three_chars(c1: char, c2: char, c3: char) -> Self {
        match c1 {
            '*' => match c2 {
                '*' => match c3 {
                    '=' => Self::DoubleStarEqual,
                    _ => Self::Op,
                },
                _ => Self::Op,
            },
            '.' => match c2 {
                '.' => match c3 {
                    '.' => Self::Ellipsis,
                    _ => Self::Op,
                },
                _ => Self::Op,
            },
            '/' => match c2 {
                '/' => match c3 {
                    '=' => Self::DoubleSlashEqual,
                    _ => Self::Op,
                },
                _ => Self::Op,
            },
            '<' => match c2 {
                '<' => match c3 {
                    '=' => Self::LeftShiftEqual,
                    _ => Self::Op,
                },
                _ => Self::Op,
            },
            '>' => match c2 {
                '>' => match c3 {
                    '=' => Self::RightShiftEqual,
                    _ => Self::Op,
                },
                _ => Self::Op,
            },
            _ => Self::Op,
        }
    }
}

pub type Position = (
    // Line number (1-indexed)
    usize,
    // Column offset (0-indexed)
    usize,
);

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TokenInfo<'a> {
    pub typ: TokenType,
    pub string: &'a str,
    pub start: Position,
    pub end: Position,
    pub line: &'a str,
}
