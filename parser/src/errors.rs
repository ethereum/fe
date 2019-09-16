use nom::error::{ErrorKind, ParseError, VerboseError, VerboseErrorKind};
use nom::{Err as NomErr, IResult};

use crate::parsers::TokenSlice;

pub fn make_error<I, O, E>(input: I, kind: ErrorKind) -> IResult<I, O, E>
where
    E: ParseError<I>,
{
    Err(NomErr::Error(ParseError::from_error_kind(input, kind)))
}

pub fn make_failure<I, O, E>(input: I, kind: ErrorKind) -> IResult<I, O, E>
where
    E: ParseError<I>,
{
    Err(NomErr::Failure(ParseError::from_error_kind(input, kind)))
}

/// Borrowed from nom:
/// https://github.com/Geal/nom/blob/c326e077b83c62f81b717c80a281cb453cb914e7/src/error.rs#L141
pub fn format_error<'a>(input: &str, e: VerboseError<TokenSlice<'a>>) -> String {
    use nom::Offset;
    use std::iter::repeat;

    let lines: Vec<_> = input.lines().map(String::from).collect();

    let mut result = String::new();

    for (i, (parser_input, kind)) in e.errors.iter().enumerate() {
        let first_token = parser_input.into_iter().next();

        if let Some(tok) = first_token {
            let tok_ptr = tok.string.as_ptr();
            let tok_offset = tok_ptr as usize - input.as_ptr() as usize;
            let substring = &input[tok_offset..];

            let mut offset = input.offset(substring);

            let mut line = 0;
            let mut column = 0;

            for (j, l) in lines.iter().enumerate() {
                if offset <= l.len() {
                    line = j;
                    column = offset;
                    break;
                } else {
                    offset = offset - l.len() - 1;
                }
            }

            match kind {
                VerboseErrorKind::Char(c) => {
                    result += &format!("{}: at line {}:\n", i, line);
                    result += &lines[line];
                    result += "\n";

                    if column > 0 {
                        result += &repeat(' ').take(column).collect::<String>();
                    }
                    result += "^\n";
                    result += &format!(
                        "expected '{}', found {}\n\n",
                        c,
                        substring.chars().next().unwrap()
                    );
                }
                VerboseErrorKind::Context(s) => {
                    result += &format!("{}: at line {}, in {}:\n", i, line, s);
                    result += &lines[line];
                    result += "\n";
                    if column > 0 {
                        result += &repeat(' ').take(column).collect::<String>();
                    }
                    result += "^\n\n";
                }
                VerboseErrorKind::Nom(e) => {
                    result += &format!("{}: at line {}, in {:?}:\n", i, line, e);
                    result += &lines[line];
                    result += "\n";
                    if column > 0 {
                        result += &repeat(' ').take(column).collect::<String>();
                    }
                    result += "^\n\n";
                }
            }
        } else {
            match kind {
                VerboseErrorKind::Char(c) => {
                    result += &format!("{}: expected '{}', got empty input\n\n", i, c);
                }
                VerboseErrorKind::Context(s) => {
                    result += &format!("{}: in {}, got empty input\n\n", i, s);
                }
                VerboseErrorKind::Nom(e) => {
                    result += &format!("{}: in {:?}, got empty input\n\n", i, e);
                }
            }
        }
    }

    result
}
