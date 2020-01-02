use crate::errors::{
    ErrorKind,
    ParseError,
};
use crate::{
    Cursor,
    ParseResult,
};

macro_rules! succ {
  (0, $macro:ident ! ($($args:tt)*)) => ($macro!(1, $($args)*));
  (1, $macro:ident ! ($($args:tt)*)) => ($macro!(2, $($args)*));
  (2, $macro:ident ! ($($args:tt)*)) => ($macro!(3, $($args)*));
  (3, $macro:ident ! ($($args:tt)*)) => ($macro!(4, $($args)*));
  (4, $macro:ident ! ($($args:tt)*)) => ($macro!(5, $($args)*));
  (5, $macro:ident ! ($($args:tt)*)) => ($macro!(6, $($args)*));
  (6, $macro:ident ! ($($args:tt)*)) => ($macro!(7, $($args)*));
}

pub trait Alt<'a, O> {
    fn parse(&self, input: Cursor<'a>) -> ParseResult<'a, O>;
}

macro_rules! alt_trait_impl {
    ($($type_var:ident)+) => {
        impl<'a, O, $($type_var),+> Alt<'a, O> for ($($type_var),+)
        where
            $($type_var: Fn(Cursor<'a>) -> ParseResult<O>),+
        {
            fn parse(&self, input: Cursor<'a>) -> ParseResult<'a, O> {
                alt_parse_body!(0, self, input, pos, $($type_var)+)
            }
        }
    };
}

macro_rules! alt_parse_body {
    ($id:tt, $self:expr, $input:expr, $pos:expr, $head:ident $($tail:ident)+) => {{
        let result = $self.$id($input);
        if result.is_ok() {
            return result;
        }
        succ!($id, alt_parse_body!($self, $input, $pos, $($tail)+))
    }};
    ($id:tt, $self:expr, $input:expr, $pos:expr, $head:ident) => {{
        $self.$id($input)
    }};
}

alt_trait_impl!(A B);
alt_trait_impl!(A B C);
alt_trait_impl!(A B C D);
alt_trait_impl!(A B C D E);
alt_trait_impl!(A B C D E F);
alt_trait_impl!(A B C D E F G);
alt_trait_impl!(A B C D E F G H);

pub fn alt<'a, O, A: Alt<'a, O>>(alts: A) -> impl Fn(Cursor<'a>) -> ParseResult<O> {
    move |input| alts.parse(input)
}

pub fn many1<'a, O, P>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<Vec<O>>
where
    P: Fn(Cursor<'a>) -> ParseResult<O>,
{
    move |input| match parser(input) {
        Ok((input, first)) => {
            let mut input = input;
            let mut results = vec![first];

            loop {
                match parser(input) {
                    Ok((input_, next)) => {
                        input = input_;
                        results.push(next);
                    }
                    Err(_) => break,
                }
            }

            Ok((input, results))
        }
        Err(err) => Err(err.push(
            input,
            ErrorKind::StaticStr("many1: expected at least one occurrence"),
        )),
    }
}

pub fn many0<'a, O, P>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<Vec<O>>
where
    P: Fn(Cursor<'a>) -> ParseResult<O>,
{
    move |input| {
        if input.is_empty() {
            return Err(ParseError::eof(input));
        }

        let mut input = input;
        let mut results = vec![];

        loop {
            match parser(input) {
                Ok((next_input, next)) => {
                    input = next_input;
                    results.push(next);
                }
                Err(_) => break,
            }
        }

        Ok((input, results))
    }
}

pub fn preceded<'a, O1, O2, F, G>(f: F, g: G) -> impl Fn(Cursor<'a>) -> ParseResult<O2>
where
    F: Fn(Cursor<'a>) -> ParseResult<O1>,
    G: Fn(Cursor<'a>) -> ParseResult<O2>,
{
    move |input| {
        let (input, _) = f(input)?;
        g(input)
    }
}

pub fn terminated<'a, O1, O2, F, G>(f: F, g: G) -> impl Fn(Cursor<'a>) -> ParseResult<O1>
where
    F: Fn(Cursor<'a>) -> ParseResult<O1>,
    G: Fn(Cursor<'a>) -> ParseResult<O2>,
{
    move |input| {
        let (input, result) = f(input)?;
        let (input, _) = g(input)?;
        Ok((input, result))
    }
}

pub fn pair<'a, O1, O2, F, G>(f: F, g: G) -> impl Fn(Cursor<'a>) -> ParseResult<(O1, O2)>
where
    F: Fn(Cursor<'a>) -> ParseResult<O1>,
    G: Fn(Cursor<'a>) -> ParseResult<O2>,
{
    move |input| {
        let (input, result_1) = f(input)?;
        let (input, result_2) = g(input)?;
        Ok((input, (result_1, result_2)))
    }
}

pub fn separated_pair<'a, O1, O2, O3, F, G, H>(
    f: F,
    g: G,
    h: H,
) -> impl Fn(Cursor<'a>) -> ParseResult<(O1, O3)>
where
    F: Fn(Cursor<'a>) -> ParseResult<O1>,
    G: Fn(Cursor<'a>) -> ParseResult<O2>,
    H: Fn(Cursor<'a>) -> ParseResult<O3>,
{
    move |input| {
        let (input, result_1) = f(input)?;
        let (input, _) = g(input)?;
        let (input, result_2) = h(input)?;
        Ok((input, (result_1, result_2)))
    }
}

pub fn opt<'a, O, P>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<Option<O>>
where
    P: Fn(Cursor<'a>) -> ParseResult<O>,
{
    move |input| match parser(input) {
        Ok((input_ok, result)) => Ok((input_ok, Some(result))),
        Err(_) => Ok((input, None)),
    }
}

pub fn map<'a, O1, O2, P, M>(parser: P, mapper: M) -> impl Fn(Cursor<'a>) -> ParseResult<O2>
where
    P: Fn(Cursor<'a>) -> ParseResult<O1>,
    M: Fn(O1) -> O2,
{
    move |input| match parser(input) {
        Ok((input_ok, result)) => Ok((input_ok, mapper(result))),
        Err(err) => Err(err),
    }
}

pub fn verify<'a, O, P, V, D>(
    parser: P,
    verifier: V,
    describer: D,
) -> impl Fn(Cursor<'a>) -> ParseResult<O>
where
    P: Fn(Cursor<'a>) -> ParseResult<O>,
    V: Fn(&O) -> bool,
    D: Fn(Cursor<'a>, &O) -> ParseError<'a>,
{
    move |input| match parser(input) {
        Ok((input_ok, result)) => {
            if verifier(&result) {
                Ok((input_ok, result))
            } else {
                Err(describer(input_ok, &result))
            }
        }
        Err(err) => Err(err),
    }
}
