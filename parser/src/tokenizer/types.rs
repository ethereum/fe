use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum TokenType {
    ENDMARKER = 0,
    NAME = 1,
    NUMBER = 2,
    STRING = 3,
    NEWLINE = 4,
    INDENT = 5,
    DEDENT = 6,
    LPAR = 7,
    RPAR = 8,
    LSQB = 9,
    RSQB = 10,
    COLON = 11,
    COMMA = 12,
    SEMI = 13,
    PLUS = 14,
    MINUS = 15,
    STAR = 16,
    SLASH = 17,
    VBAR = 18,
    AMPER = 19,
    LESS = 20,
    GREATER = 21,
    EQUAL = 22,
    DOT = 23,
    PERCENT = 24,
    LBRACE = 25,
    RBRACE = 26,
    EQEQUAL = 27,
    NOTEQUAL = 28,
    LESSEQUAL = 29,
    GREATEREQUAL = 30,
    TILDE = 31,
    CIRCUMFLEX = 32,
    LEFTSHIFT = 33,
    RIGHTSHIFT = 34,
    DOUBLESTAR = 35,
    PLUSEQUAL = 36,
    MINEQUAL = 37,
    STAREQUAL = 38,
    SLASHEQUAL = 39,
    PERCENTEQUAL = 40,
    AMPEREQUAL = 41,
    VBAREQUAL = 42,
    CIRCUMFLEXEQUAL = 43,
    LEFTSHIFTEQUAL = 44,
    RIGHTSHIFTEQUAL = 45,
    DOUBLESTAREQUAL = 46,
    DOUBLESLASH = 47,
    DOUBLESLASHEQUAL = 48,
    AT = 49,
    ATEQUAL = 50,
    RARROW = 51,
    ELLIPSIS = 52,
    COLONEQUAL = 53,
    OP = 54,
    AWAIT = 55,
    ASYNC = 56,
    TYPEIGNORE = 57,
    TYPECOMMENT = 58,
    ERRORTOKEN = 59,
    // These aren't used by the C tokenizer but are needed for tokenize.py
    COMMENT = 60,
    NL = 61,
    ENCODING = 62,
    NTOKENS = 63,
    // Special definitions for cooperation with parser
    NTOFFSET = 256,
}

impl TokenType {
    pub fn from_one_char(c: char) -> Self {
        match c {
            '%' => Self::PERCENT,
            '&' => Self::AMPER,
            '(' => Self::LPAR,
            ')' => Self::RPAR,
            '*' => Self::STAR,
            '+' => Self::PLUS,
            ',' => Self::COMMA,
            '-' => Self::MINUS,
            '.' => Self::DOT,
            '/' => Self::SLASH,
            ':' => Self::COLON,
            ';' => Self::SEMI,
            '<' => Self::LESS,
            '=' => Self::EQUAL,
            '>' => Self::GREATER,
            '@' => Self::AT,
            '[' => Self::LSQB,
            ']' => Self::RSQB,
            '^' => Self::CIRCUMFLEX,
            '{' => Self::LBRACE,
            '|' => Self::VBAR,
            '}' => Self::RBRACE,
            '~' => Self::TILDE,
            _ => Self::OP,
        }
    }

    pub fn from_two_chars(c1: char, c2: char) -> Self {
        match c1 {
            '!' => match c2 {
                '=' => Self::NOTEQUAL,
                _ => Self::OP,
            },
            '%' => match c2 {
                '=' => Self::PERCENTEQUAL,
                _ => Self::OP,
            },
            '&' => match c2 {
                '=' => Self::AMPEREQUAL,
                _ => Self::OP,
            },
            '*' => match c2 {
                '*' => Self::DOUBLESTAR,
                '=' => Self::STAREQUAL,
                _ => Self::OP,
            },
            '+' => match c2 {
                '=' => Self::PLUSEQUAL,
                _ => Self::OP,
            },
            '-' => match c2 {
                '=' => Self::MINEQUAL,
                '>' => Self::RARROW,
                _ => Self::OP,
            },
            '/' => match c2 {
                '/' => Self::DOUBLESLASH,
                '=' => Self::SLASHEQUAL,
                _ => Self::OP,
            },
            ':' => match c2 {
                '=' => Self::COLONEQUAL,
                _ => Self::OP,
            },
            '<' => match c2 {
                '<' => Self::LEFTSHIFT,
                '=' => Self::LESSEQUAL,
                '>' => Self::NOTEQUAL,
                _ => Self::OP,
            },
            '=' => match c2 {
                '=' => Self::EQEQUAL,
                _ => Self::OP,
            },
            '>' => match c2 {
                '=' => Self::GREATEREQUAL,
                '>' => Self::RIGHTSHIFT,
                _ => Self::OP,
            },
            '@' => match c2 {
                '=' => Self::ATEQUAL,
                _ => Self::OP,
            },
            '^' => match c2 {
                '=' => Self::CIRCUMFLEXEQUAL,
                _ => Self::OP,
            },
            '|' => match c2 {
                '=' => Self::VBAREQUAL,
                _ => Self::OP,
            },
            _ => Self::OP,
        }
    }

    pub fn from_three_chars(c1: char, c2: char, c3: char) -> Self {
        match c1 {
            '*' => match c2 {
                '*' => match c3 {
                    '=' => Self::DOUBLESTAREQUAL,
                    _ => Self::OP,
                },
                _ => Self::OP,
            },
            '.' => match c2 {
                '.' => match c3 {
                    '.' => Self::ELLIPSIS,
                    _ => Self::OP,
                },
                _ => Self::OP,
            },
            '/' => match c2 {
                '/' => match c3 {
                    '=' => Self::DOUBLESLASHEQUAL,
                    _ => Self::OP,
                },
                _ => Self::OP,
            },
            '<' => match c2 {
                '<' => match c3 {
                    '=' => Self::LEFTSHIFTEQUAL,
                    _ => Self::OP,
                },
                _ => Self::OP,
            },
            '>' => match c2 {
                '>' => match c3 {
                    '=' => Self::RIGHTSHIFTEQUAL,
                    _ => Self::OP,
                },
                _ => Self::OP,
            },
            _ => Self::OP,
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
