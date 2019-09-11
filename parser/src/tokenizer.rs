use regex::Regex;
use serde::{Deserialize, Serialize};
use str_concat::concat;

use crate::string_utils::{lines_with_endings, rstrip_slice};

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
    fn from_one_char(c: char) -> Self {
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

    fn from_two_chars(c1: char, c2: char) -> Self {
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

    fn from_three_chars(c1: char, c2: char, c3: char) -> Self {
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

type Position = (
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

fn group(choices: &[&str]) -> String {
    ["(".to_string(), choices.join("|"), ")".to_string()].concat()
}

fn any(choices: &[&str]) -> String {
    [group(choices), "*".to_string()].concat()
}

fn maybe(choices: &[&str]) -> String {
    [group(choices), "?".to_string()].concat()
}

const WHITESPACE: &str = r"[ \f\t]*";
const COMMENT: &str = r"#[^\r\n]*";

/// Whitespace + any(r"\\\r?\n" + Whitespace) + maybe(Comment)
fn get_ignore_pattern() -> String {
    let any_part = &[r"\\\r?\n", WHITESPACE].concat()[..];

    [WHITESPACE, &any(&[any_part])[..], &maybe(&[COMMENT])[..]].concat()
}

const NAME: &str = r"\w+";

const HEXNUMBER: &str = r"0[xX](?:_?[0-9a-fA-F])+";
const BINNUMBER: &str = r"0[bB](?:_?[01])+";
const OCTNUMBER: &str = r"0[oO](?:_?[0-7])+";
const DECNUMBER: &str = r"(?:0(?:_?0)*|[1-9](?:_?[0-9])*)";

/// INTNUMBER = group(Hexnumber, Binnumber, Octnumber, Decnumber)
fn get_intnumber_pattern() -> String {
    group(&[HEXNUMBER, BINNUMBER, OCTNUMBER, DECNUMBER])
}

const EXPONENT: &str = r"[eE][-+]?[0-9](?:_?[0-9])*";

/// POINTFLOAT = group(
///     r"[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?",
///     r"\.[0-9](?:_?[0-9])*",
/// ) + maybe(EXPONENT)
fn get_pointfloat_pattern() -> String {
    [
        &group(&[
            r"[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?",
            r"\.[0-9](?:_?[0-9])*",
        ])[..],
        &maybe(&[EXPONENT])[..],
    ]
    .concat()
}

/// EXPFLOAT = r"[0-9](?:_?[0-9])*" + EXPONENT
fn get_expfloat_pattern() -> String {
    [r"[0-9](?:_?[0-9])*", EXPONENT].concat()
}

/// FLOATNUMBER = group(POINTFLOAT, EXPFLOAT)
fn get_floatnumber_pattern() -> String {
    group(&[&get_pointfloat_pattern()[..], &get_expfloat_pattern()[..]])
}

/// IMAGNUMBER = group(r"[0-9](?:_?[0-9])*[jJ]", FLOATNUMBER + r"[jJ]")
fn get_imagnumber_pattern() -> String {
    group(&[
        r"[0-9](?:_?[0-9])*[jJ]",
        &[&get_floatnumber_pattern()[..], r"[jJ]"].concat()[..],
    ])
}

/// NUMBER = group(IMAGNUMBER, FLOATNUMBER, INTNUMBER)
fn get_number_pattern() -> String {
    group(&[
        &get_imagnumber_pattern()[..],
        &get_floatnumber_pattern()[..],
        &get_intnumber_pattern()[..],
    ])
}

// Tail end of ' string
const SINGLE: &str = r###"
[^'\\]*(?:\\.[^'\\]*)*'
"###;

// Tail end of " string
const DOUBLE: &str = r###"
[^"\\]*(?:\\.[^"\\]*)*"
"###;

// Tail end of ''' string
const SINGLE3: &str = r###"
[^'\\]*(?:(?:\\.|'(?!''))[^'\\]*)*'''
"###;

// Tail end of """ string
const DOUBLE3: &str = r###"
[^"\\]*(?:(?:\\.|"(?!""))[^"\\]*)*"""
"###;

fn get_string_end_regex(token: &str) -> Regex {
    if token.ends_with("\"\"\"") {
        Regex::new(DOUBLE3).unwrap()
    } else if token.ends_with("'''") {
        Regex::new(SINGLE3).unwrap()
    } else if token.ends_with("\"") {
        Regex::new(DOUBLE).unwrap()
    } else if token.ends_with("'") {
        Regex::new(SINGLE).unwrap()
    } else {
        panic!("Unrecognized quote style");
    }
}

const TABSIZE: usize = 8;

fn tokenize<'a>(input: &'a str) -> Result<Vec<TokenInfo<'a>>, String> {
    let mut result: Vec<TokenInfo<'a>> = Vec::new();

    // State vars
    let mut parenlev: usize = 0;
    let mut continued: bool = false;
    let mut needcont: bool = false;
    let mut indents: Vec<usize> = vec![0];

    let mut contstr: Option<&'a str> = None;
    let mut contline: Option<&'a str> = None;
    let mut endprog: Option<&Regex> = None;

    let mut strstart: Option<Position> = None;

    let mut last_line: Option<&'a str> = None;
    for (line, lnum) in lines_with_endings(input).zip(1..) {
        let mut pos: usize = 0;
        let line_len: usize = line.len();

        if let Some(contstr_val) = contstr {
            // Continued string
            if let Some(engprog_val) = endprog {
                if let Some(mtch) = engprog_val.find(line) {
                    let end = mtch.end();
                    pos = end;
                    result.push(TokenInfo {
                        typ: TokenType::String,
                        string: concat(contstr.unwrap(), &line[..end]).unwrap(),
                        start: strstart.unwrap(),
                        end: (lnum, end),
                        line: concat(contline.unwrap(), line).unwrap(),
                    });
                    contstr = None;
                    contline = None;
                    needcont = false;
                }
            } else if needcont && !line.ends_with("\\\n") && !line.ends_with("\\\r\n") {
                result.push(TokenInfo {
                    typ: TokenType::ErrorToken,
                    string: concat(contstr_val, line).unwrap(),
                    start: strstart.unwrap(),
                    end: (lnum, line_len),
                    line: contline.unwrap(),
                });
                contstr = None;
                contline = None;
                continue;
            } else {
                contstr = Some(concat(contstr_val, line).unwrap());
                contline = Some(concat(contline.unwrap(), line).unwrap());
                continue;
            }
        } else if parenlev == 0 && !continued {
            // New statement
            let mut column: usize = 0;

            // Measure leading whitespace
            for c in line[pos..].chars() {
                match c {
                    ' ' => {
                        column += 1;
                    }
                    '\t' => {
                        column = (column / TABSIZE + 1) * TABSIZE;
                    }
                    '\x0c' => {
                        // Form feed ("\f" in python)
                        column = 0;
                    }
                    _ => {
                        // Break if we encounter anything that's not part of indentation
                        break;
                    }
                }
                pos += c.len_utf8();
            }

            if pos == line_len {
                // If no more chars in line (not even newline, carriage return, etc.), we're at
                // EOF.  Break out of the token loop.
                break;
            }

            if let Some(c) = line[pos..].chars().next() {
                if c == '#' || c == '\r' || c == '\n' {
                    if c == '#' {
                        let comment_token = rstrip_slice(&line[pos..], "\r\n");
                        result.push(TokenInfo {
                            typ: TokenType::Comment,
                            string: comment_token,
                            start: (lnum, pos),
                            end: (lnum, pos + comment_token.len()),
                            line: line,
                        });
                        pos += comment_token.len();
                    }

                    result.push(TokenInfo {
                        typ: TokenType::NL,
                        string: &line[pos..],
                        start: (lnum, pos),
                        end: (lnum, line_len),
                        line: line,
                    });

                    continue;
                }
            }

            if column > *indents.last().unwrap() {
                indents.push(column);
                result.push(TokenInfo {
                    typ: TokenType::Indent,
                    string: &line[..pos],
                    start: (lnum, 0),
                    end: (lnum, pos),
                    line: line,
                });
            }

            while column < *indents.last().unwrap() {
                if !indents.contains(&column) {
                    return Err("Unindent does not match any outer indentation level".to_string());
                }
                indents.pop();
                result.push(TokenInfo {
                    typ: TokenType::Dedent,
                    string: &line[line_len..],
                    start: (lnum, pos),
                    end: (lnum, pos),
                    line: line,
                });
            }
        } else {
            continued = false;
        }

        last_line = Some(line);
    }

    if let Some(_) = contstr {
        return Err("EOF in multi-line string".to_string());
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regex_patterns() {
        assert_eq!(
            get_ignore_pattern(),
            "[ \\f\\t]*(\\\\\\r?\\n[ \\f\\t]*)*(#[^\\r\\n]*)?",
        );
        assert_eq!(
            get_intnumber_pattern(),
            "(0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|(?:0(?:_?0)*|[1-9](?:_?[0-9])*))",
        );
        assert_eq!(
            get_pointfloat_pattern(),
            "([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?",
        );
        assert_eq!(
            get_expfloat_pattern(),
            "[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*",
        );
        assert_eq!(
            get_floatnumber_pattern(),
            "(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)",
        );
        assert_eq!(
            get_imagnumber_pattern(),
            "([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])",
        );
        assert_eq!(
            get_number_pattern(),
            "(([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)|(0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|(?:0(?:_?0)*|[1-9](?:_?[0-9])*)))",
        );
    }

    #[test]
    fn test_tokenize() {
        let tokens = tokenize(
            r"
# Test comment
class TestClassDefinition(WithParent1, WithParent2):
    foo = 'bar'

    def foo():
        return 'bar'

test = 'foo'
",
        );

        let serialized = serde_json::to_string_pretty(&tokens).unwrap();

        //println!("{}", serialized);

        //assert_eq!(
        //    vec![TokenInfo {
        //        typ: TokenType::Comment,
        //        string: "Test",
        //        start: 0,
        //        end: 4,
        //        line: 0,
        //    }]
        //);
    }
}
