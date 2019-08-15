use nom::error::{ErrorKind, ParseError};
use nom::{Err as NomErr, IResult};

#[derive(Clone, Debug, PartialEq)]
pub struct MultiError<I> {
    pub errors: Vec<SingleError<I>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SingleError<I> {
    pub input: I,
    pub context: Option<String>,
    pub kind: Option<ErrorKind>,
}

pub fn make_error<I, O>(input: I, kind: ErrorKind) -> IResult<I, O> {
    Err(NomErr::Error((input, kind)))
}

pub fn make_failure<I, O>(input: I, kind: ErrorKind) -> IResult<I, O> {
    Err(NomErr::Failure((input, kind)))
}

impl<I> MultiError<I> {
    pub fn make_error<O>(input: I, context: String) -> IResult<I, O, Self> {
        Err(NomErr::Error(Self {
            errors: vec![SingleError::from_context(input, context)],
        }))
    }

    pub fn make_failure<O>(input: I, context: String) -> IResult<I, O, Self> {
        Err(NomErr::Failure(Self {
            errors: vec![SingleError::from_context(input, context)],
        }))
    }
}

impl<I> ParseError<I> for MultiError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Self {
            errors: vec![SingleError::from_error_kind(input, kind)],
        }
    }

    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        other.errors.push(SingleError::from_error_kind(input, kind));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        Self {
            errors: vec![SingleError::from_char(input, c)],
        }
    }

    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push(SingleError::from_context(input, ctx.to_string()));
        other
    }
}

impl<I> SingleError<I> {
    pub fn from_context(input: I, context: String) -> Self {
        Self {
            input: input,
            context: Some(context),
            kind: None,
        }
    }
}

impl<I> ParseError<I> for SingleError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Self {
            input: input,
            context: None,
            kind: Some(kind),
        }
    }

    fn from_char(input: I, c: char) -> Self {
        Self {
            input: input,
            context: Some(c.to_string()),
            kind: Some(ErrorKind::Char),
        }
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}
