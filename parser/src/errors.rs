use nom::error::{
    ErrorKind,
    ParseError,
    VerboseError,
    VerboseErrorKind,
};
use nom::{
    Err as NomErr,
    IResult,
};

use crate::parsers::TokenSlice;
use crate::string_utils::StringPositions;

pub fn make_error<I, O, E>(input: I, kind: ErrorKind) -> IResult<I, O, E>
where
    E: ParseError<I>,
{
    Err(NomErr::Error(ParseError::from_error_kind(input, kind)))
}

/// Format a verbose error into a debug trace message.
///
/// Inspired by nom internals:
/// https://github.com/Geal/nom/blob/c326e077b83c62f81b717c80a281cb453cb914e7/src/error.rs#L141
pub fn format_debug_error(input: &str, e: VerboseError<TokenSlice>) -> String {
    use std::iter::repeat;

    let mut string_positions = StringPositions::new(input);
    let lines: Vec<_> = input.lines().map(String::from).collect();

    let mut result = String::new();

    for (err_no, (parser_input, err_kind)) in e.errors.iter().rev().enumerate() {
        let first_token = parser_input.iter().next();

        if let Some(tok) = first_token {
            let pos = string_positions.get_pos(tok.span.start).unwrap();
            let line = pos.line;
            let col = pos.col;

            match err_kind {
                VerboseErrorKind::Char(c) => {
                    result += &format!("{}: at line {}:\n", err_no, line);
                    result += &lines[line];
                    result += "\n";

                    if col > 0 {
                        result += &repeat(' ').take(col).collect::<String>();
                    }
                    result += "^\n";
                    result += &format!(
                        "expected '{}', found {}\n\n",
                        c,
                        tok.string.chars().next().unwrap()
                    );
                }
                VerboseErrorKind::Context(s) => {
                    result += &format!("{}: at line {}, in {}:\n", err_no, line, s);
                    result += &lines[line];
                    result += "\n";
                    if col > 0 {
                        result += &repeat(' ').take(col).collect::<String>();
                    }
                    result += "^\n\n";
                }
                VerboseErrorKind::Nom(e) => {
                    result += &format!("{}: at line {}, in {:?}:\n", err_no, line, e);
                    result += &lines[line];
                    result += "\n";
                    if col > 0 {
                        result += &repeat(' ').take(col).collect::<String>();
                    }
                    result += "^\n\n";
                }
            }
        } else {
            match err_kind {
                VerboseErrorKind::Char(c) => {
                    result += &format!("{}: expected '{}', got empty input\n\n", err_no, c);
                }
                VerboseErrorKind::Context(s) => {
                    result += &format!("{}: in {}, got empty input\n\n", err_no, s);
                }
                VerboseErrorKind::Nom(e) => {
                    result += &format!("{}: in {:?}, got empty input\n\n", err_no, e);
                }
            }
        }
    }

    result
}

#[derive(Debug, PartialEq)]
pub enum FormatError {
    NoContextFound(String),
}

/// Format a verbose error into a user-facing syntax error message.
pub fn format_user_error(
    input: &str,
    err: VerboseError<TokenSlice>,
) -> Result<String, FormatError> {
    use std::iter::repeat;

    let last_context_error = err
        .errors
        .iter()
        .rev()
        .skip_while(|e| match e {
            (_, VerboseErrorKind::Context(_)) => false,
            _ => true,
        })
        .next();

    if let Some((parser_input, VerboseErrorKind::Context(context_string))) = last_context_error {
        let mut string_positions = StringPositions::new(input);
        let lines: Vec<_> = input.lines().map(String::from).collect();

        let mut result = String::new();
        let first_token = parser_input.iter().next();

        if let Some(tok) = first_token {
            let pos = string_positions.get_pos(tok.span.start).unwrap();
            let line = pos.line;
            let col = pos.col;

            result += &format!("at line {}, expected {}:\n", line, context_string);
            result += &lines[line];
            result += "\n";
            if col > 0 {
                result += &repeat(' ').take(col).collect::<String>();
            }
            result += "^\n";
        } else {
            result += &format!("expected {}, got empty input\n", context_string);
        }
        Ok(result)
    } else {
        Err(FormatError::NoContextFound(
            "no context error found in verbose error".into(),
        ))
    }
}
