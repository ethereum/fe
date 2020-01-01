use nom::error::{
    ErrorKind as NomErrorKind,
    ParseError as NomParseError,
};
use nom::Err as NomErr;

use crate::parsers::{
    Cursor,
    ParseResult,
};
use crate::string_utils::StringPositions;

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorKind {
    StaticStr(&'static str),
    Str(String),
    Eof,
}

impl ErrorKind {
    pub fn description(&self) -> &str {
        use ErrorKind::*;

        match self {
            StaticStr(s) => s,
            Str(s) => s.as_str(),
            Eof => "end of file",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParseError<'a> {
    errors: Vec<(Cursor<'a>, ErrorKind)>,
}

impl<'a> ParseError<'a> {
    pub fn new(input: Cursor<'a>, kind: ErrorKind) -> Self {
        Self {
            errors: vec![(input, kind)],
        }
    }
}

impl<'a> NomParseError<Cursor<'a>> for ParseError<'a> {
    fn from_error_kind(input: Cursor<'a>, kind: NomErrorKind) -> Self {
        Self::new(input, ErrorKind::Str(kind.description().to_string()))
    }

    fn append(input: Cursor<'a>, kind: NomErrorKind, mut other: Self) -> Self {
        other
            .errors
            .push((input, ErrorKind::Str(kind.description().to_string())));
        other
    }

    fn from_char(input: Cursor<'a>, c: char) -> Self {
        Self::new(input, ErrorKind::Str(format!("expected '{}'", c)))
    }

    fn add_context(input: Cursor<'a>, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push((input, ErrorKind::StaticStr(ctx)));
        other
    }
}

pub fn make_error<O>(input: Cursor, kind: NomErrorKind) -> ParseResult<O> {
    Err(NomErr::Error(NomParseError::from_error_kind(input, kind)))
}

pub fn make_parse_error<O>(input: Cursor, kind: ErrorKind) -> ParseResult<O> {
    Err(NomErr::Error(ParseError::new(input, kind)))
}

/// Format an error into a debug trace message.
///
/// Inspired by nom internals:
/// https://github.com/Geal/nom/blob/c326e077b83c62f81b717c80a281cb453cb914e7/src/error.rs#L141
pub fn format_debug_error(input: &str, e: ParseError, show_err_no: bool) -> String {
    use std::iter::repeat;

    let mut string_positions = StringPositions::new(input);
    let lines: Vec<_> = input.lines().map(String::from).collect();

    let mut result = String::new();

    for (err_no, (parser_input, err_kind)) in e.errors.iter().rev().enumerate() {
        let first_token = parser_input.iter().next();

        if show_err_no {
            result += &format!("{}: ", err_no);
        }

        let offset = match first_token {
            Some(tok) => tok.span.start,
            None => input.len(),
        };
        let pos = match string_positions.get_pos(offset) {
            Some(pos) => pos,
            None => string_positions.get_last().unwrap(),
        };

        result += &format!(
            "at line {} col {}, {}:\n",
            pos.line,
            pos.col,
            err_kind.description()
        );

        result += &lines[pos.line - 1];
        result += "\n";
        if pos.col > 0 {
            result += &repeat(' ').take(pos.col).collect::<String>();
        }
        result += "^\n\n";
    }

    result
}

/// Format an error into a user-facing error message.
///
/// Uses the innermost error to build a user-facing error message and position.
pub fn format_user_error(input: &str, err: ParseError) -> String {
    let deepest_error = err.errors.first().unwrap();
    let new_err = ParseError {
        errors: vec![deepest_error.clone()],
    };

    format_debug_error(input, new_err, false)
}
