use nom::error::{ErrorKind, ParseError};
use nom::{Err as NomErr, IResult};

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
