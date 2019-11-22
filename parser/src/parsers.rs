use std::convert::TryFrom;

use nom::branch::alt;
use nom::combinator::{
    map,
    verify,
};
use nom::error::{
    context,
    ErrorKind,
    ParseError,
};
use nom::multi::{
    many0,
    many1,
};
use nom::sequence::{
    delimited,
    pair,
    preceded,
    separated_pair,
};
use nom::IResult;

use crate::ast::ModuleStmt::*;
use crate::ast::*;
use crate::errors::make_error;
use crate::span::GetSourceSpan;
use crate::tokenizer::tokenize::tokenize;
use crate::tokenizer::types::{
    TokenInfo,
    TokenType,
};

pub type TokenRef<'a> = &'a TokenInfo<'a>;
pub type TokenSlice<'a> = &'a [TokenInfo<'a>];
pub type TokenResult<'a, O, E> = IResult<TokenSlice<'a>, O, E>;

/// Tokenize the given source code in `source` and filter out tokens not
/// relevant to parsing.
pub fn get_parse_tokens<'a>(source: &'a str) -> Result<Vec<TokenInfo<'a>>, String> {
    let tokens = tokenize(source)?;

    Ok(tokens
        .into_iter()
        .filter(|t| t.typ != TokenType::NL && t.typ != TokenType::COMMENT)
        .collect())
}

/// Parse a single token from a token slice.
pub fn one_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    match input.iter().next() {
        None => make_error(input, ErrorKind::Eof),
        Some(token) => Ok((&input[1..], token)),
    }
}

/// Parse a token of a specific type from a token slice.
pub fn token<'a, E>(typ: TokenType) -> impl Fn(TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    verify(one_token, move |t: &TokenInfo| t.typ == typ)
}

/// Parse a name token from a token slice.
pub fn name_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::NAME)(input)
}

/// Parse a name token containing a specific string from a token slice.
pub fn name_string<'a, E>(
    string: &'a str,
) -> impl Fn(TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    verify(name_token, move |t: &TokenInfo| t.string == string)
}

/// Parse an op token from a token slice.
pub fn op_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::OP)(input)
}

/// Parse an op token containing a specific string from a token slice.
pub fn op_string<'a, E>(
    string: &'a str,
) -> impl Fn(TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    verify(op_token, move |t: &TokenInfo| t.string == string)
}

/// Parse a number token from a token slice.
pub fn number_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::NUMBER)(input)
}

/// Parse a string token from a token slice.
pub fn string_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::STRING)(input)
}

/// Parse an indent token from a token slice.
pub fn indent_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::INDENT)(input)
}

/// Parse a dedent token from a token slice.
pub fn dedent_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::DEDENT)(input)
}

/// Parse a grammatically significant newline token from a token slice.
pub fn newline_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::NEWLINE)(input)
}

/// Parse an endmarker token from a token slice.
pub fn endmarker_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, TokenRef<'a>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::ENDMARKER)(input)
}

/// Parse a module definition.
pub fn file_input<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, Module, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    // (NEWLINE* module_stmt)*
    let (input, body) = many0(preceded(many0(newline_token), module_stmt))(input)?;

    // NEWLINE*
    let (input, _) = many0(newline_token)(input)?;

    // ENDMARKER
    let (input, end_tok) = endmarker_token(input)?;

    let source_span = match body.first() {
        Some(first_stmt) => {
            let first_span = first_stmt.get_source_span();
            let last_span = body.last().unwrap().get_source_span();

            (first_span, last_span).into()
        }
        None => end_tok.into(),
    };

    Ok((input, Module { body, source_span }))
}

/// Parse a module statement, such as a contract definition.
pub fn module_stmt<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ModuleStmt, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let (input, module_stmt) = context("expected event definition", parse_event_def)(input)?;

    Ok((input, module_stmt))
}

/// Parse an event definition statement.
pub fn parse_event_def<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ModuleStmt, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    // "event" name ":" NEWLINE
    let (input, event_kw) = name_string("event")(input)?;
    let (input, name) = name_token(input)?;
    let (input, _) = op_string(":")(input)?;
    let (input, _) = newline_token(input)?;

    // INDENT event_field+ DEDENT
    let (input, _) = indent_token(input)?;
    let (input, fields) = many1(parse_event_field)(input)?;
    let (input, _) = dedent_token(input)?;

    let last_field = fields.last().unwrap();
    let source_span = (event_kw, last_field.get_source_span()).into();

    Ok((
        input,
        EventDef {
            name: name.string,
            fields: fields,
            source_span: source_span,
        },
    ))
}

/// Parse an event field definition.
pub fn parse_event_field<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, EventField, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let (input, name) = name_token(input)?;
    let (input, _) = op_string(":")(input)?;
    let (input, typ) = name_token(input)?;
    let (input, _) = newline_token(input)?;

    Ok((
        input,
        EventField {
            name: name.string,
            typ: typ.into(),
            source_span: (name, typ).into(),
        },
    ))
}

/// Parse a constant expression that can be evaluated at compile-time.
pub fn const_expr<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let (input, head) = const_term(input)?;
    let (input, tail) = many0(alt((
        pair(op_string("+"), const_term),
        pair(op_string("-"), const_term),
    )))(input)?;

    let mut left_expr = head;
    for (op_tok, right_expr) in tail {
        let source_span = (left_expr.get_source_span(), right_expr.get_source_span()).into();

        left_expr = ConstExpr::BinOp {
            left: Box::new(left_expr),
            op: Operator::try_from(op_tok.string).unwrap(),
            right: Box::new(right_expr),
            source_span: source_span,
        };
    }

    Ok((input, left_expr))
}

/// Parse a constant term that may appear as the operand of an addition or
/// subtraction.
pub fn const_term<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let (input, head) = const_factor(input)?;
    let (input, tail) = many0(alt((
        pair(op_string("*"), const_factor),
        pair(op_string("/"), const_factor),
        pair(op_string("%"), const_factor),
    )))(input)?;

    let mut left_expr = head;
    for (op_tok, right_expr) in tail {
        let source_span = (left_expr.get_source_span(), right_expr.get_source_span()).into();

        left_expr = ConstExpr::BinOp {
            left: Box::new(left_expr),
            op: Operator::try_from(op_tok.string).unwrap(),
            right: Box::new(right_expr),
            source_span: source_span,
        };
    }

    Ok((input, left_expr))
}

/// Parse a constant factor that may appear as the operand of a multiplication,
/// division, modulus, or unary op or as the exponent of a power expression.
pub fn const_factor<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let unary_op = map(
        pair(
            alt((op_string("+"), op_string("-"), op_string("~"))),
            const_factor,
        ),
        |res| {
            let (op_tok, operand) = res;
            let source_span = (op_tok, operand.get_source_span()).into();

            ConstExpr::UnaryOp {
                op: UnaryOp::try_from(op_tok.string).unwrap(),
                operand: Box::new(operand),
                source_span: source_span,
            }
        },
    );

    alt((unary_op, const_power))(input)
}

/// Parse a constant power expression that may appear in the position of a
/// constant factor.
pub fn const_power<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let bin_op = map(
        separated_pair(const_atom, op_string("**"), const_factor),
        |res| {
            let (left, right) = res;
            let source_span = (left.get_source_span(), right.get_source_span()).into();

            ConstExpr::BinOp {
                left: Box::new(left),
                op: Operator::Pow,
                right: Box::new(right),
                source_span: source_span,
            }
        },
    );

    alt((bin_op, const_atom))(input)
}

/// Parse a constant atom expression that may appear in the position of a
/// constant power or as the base of a constant power expression.
pub fn const_atom<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    alt((
        const_group,
        map(name_token, |t| ConstExpr::Name {
            name: t.string,
            source_span: t.into(),
        }),
        map(number_token, |t| ConstExpr::Num {
            num: t.string,
            source_span: t.into(),
        }),
    ))(input)
}

/// Parse a parenthesized constant group that may appear in the position of a
/// constant atom.
pub fn const_group<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    delimited(op_string("("), const_expr, op_string(")"))(input)
}
