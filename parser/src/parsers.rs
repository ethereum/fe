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
    pair,
    preceded,
    separated_pair,
};
use nom::IResult;

use crate::ast::ModuleStmt::*;
use crate::ast::*;
use crate::errors::make_error;
use crate::span::Spanned;
use crate::tokenizer::tokenize::tokenize;
use crate::tokenizer::types::{
    Token,
    TokenType,
};

pub type TokenSlice<'a> = &'a [Token<'a>];
pub type TokenResult<'a, O, E> = IResult<TokenSlice<'a>, O, E>;

/// Tokenize the given source code in `source` and filter out tokens not
/// relevant to parsing.
pub fn get_parse_tokens<'a>(source: &'a str) -> Result<Vec<Token<'a>>, String> {
    let tokens = tokenize(source)?;

    Ok(tokens
        .into_iter()
        .filter(|t| t.typ != TokenType::NL && t.typ != TokenType::COMMENT)
        .collect())
}

/// Parse a single token from a token slice.
pub fn one_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    match input.iter().next() {
        None => make_error(input, ErrorKind::Eof),
        Some(token) => Ok((&input[1..], token)),
    }
}

/// Parse a token of a specific type from a token slice.
pub fn token<'a, E>(typ: TokenType) -> impl Fn(TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    verify(one_token, move |t: &Token| t.typ == typ)
}

/// Parse a name token from a token slice.
pub fn name_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::NAME)(input)
}

/// Parse a name token containing a specific string from a token slice.
pub fn name_string<'a, E>(string: &'a str) -> impl Fn(TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    verify(name_token, move |t: &Token| t.string == string)
}

/// Parse an op token from a token slice.
pub fn op_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::OP)(input)
}

/// Parse an op token containing a specific string from a token slice.
pub fn op_string<'a, E>(string: &'a str) -> impl Fn(TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    verify(op_token, move |t: &Token| t.string == string)
}

/// Parse a number token from a token slice.
pub fn number_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::NUMBER)(input)
}

/// Parse a string token from a token slice.
pub fn string_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::STRING)(input)
}

/// Parse an indent token from a token slice.
pub fn indent_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::INDENT)(input)
}

/// Parse a dedent token from a token slice.
pub fn dedent_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::DEDENT)(input)
}

/// Parse a grammatically significant newline token from a token slice.
pub fn newline_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::NEWLINE)(input)
}

/// Parse an endmarker token from a token slice.
pub fn endmarker_token<'a, E>(input: TokenSlice<'a>) -> TokenResult<&Token, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    token(TokenType::ENDMARKER)(input)
}

/// Parse a module definition.
pub fn file_input<'a, E>(input: TokenSlice<'a>) -> TokenResult<Spanned<Module>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    // (NEWLINE* module_stmt)*
    let (input, body) = many0(preceded(many0(newline_token), module_stmt))(input)?;

    // NEWLINE*
    let (input, _) = many0(newline_token)(input)?;

    // ENDMARKER
    let (input, end_tok) = endmarker_token(input)?;

    let span = match body.first() {
        Some(first_stmt) => {
            let first_span = first_stmt.span;
            let last_span = body.last().unwrap().span;

            (&first_span, &last_span).into()
        }
        None => end_tok.span,
    };

    Ok((
        input,
        Spanned {
            node: Module { body },
            span,
        },
    ))
}

/// Parse a module statement, such as a contract definition.
pub fn module_stmt<'a, E>(input: TokenSlice<'a>) -> TokenResult<Spanned<ModuleStmt>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let (input, module_stmt) = context("event definition", event_def)(input)?;

    Ok((input, module_stmt))
}

/// Parse an event definition statement.
pub fn event_def<'a, E>(input: TokenSlice<'a>) -> TokenResult<Spanned<ModuleStmt>, E>
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
    let (input, fields) = many1(event_field)(input)?;
    let (input, _) = dedent_token(input)?;

    let last_field = fields.last().unwrap();
    let span = (&event_kw.span, &last_field.span).into();

    Ok((
        input,
        Spanned {
            node: EventDef {
                name: name.string,
                fields,
            },
            span,
        },
    ))
}

/// Parse an event field definition.
pub fn event_field<'a, E>(input: TokenSlice<'a>) -> TokenResult<Spanned<EventField>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let (input, name) = name_token(input)?;
    let (input, _) = op_string(":")(input)?;
    let (input, typ) = name_token(input)?;
    let (input, _) = newline_token(input)?;

    Ok((
        input,
        Spanned {
            node: EventField {
                name: name.string,
                typ: typ.into(),
            },
            span: (&name.span, &typ.span).into(),
        },
    ))
}

/// Parse a constant expression that can be evaluated at compile-time.
pub fn const_expr<'a, E>(input: TokenSlice<'a>) -> TokenResult<Spanned<ConstExpr>, E>
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
        let span = (&left_expr.span, &right_expr.span).into();

        left_expr = Spanned {
            node: ConstExpr::BinOp {
                left: Box::new(left_expr),
                op: Operator::try_from(op_tok.string).unwrap(),
                right: Box::new(right_expr),
            },
            span,
        };
    }

    Ok((input, left_expr))
}

/// Parse a constant term that may appear as the operand of an addition or
/// subtraction.
pub fn const_term<'a, E>(input: TokenSlice<'a>) -> TokenResult<Spanned<ConstExpr>, E>
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
        let span = (&left_expr.span, &right_expr.span).into();

        left_expr = Spanned {
            node: ConstExpr::BinOp {
                left: Box::new(left_expr),
                op: Operator::try_from(op_tok.string).unwrap(),
                right: Box::new(right_expr),
            },
            span,
        };
    }

    Ok((input, left_expr))
}

/// Parse a constant factor that may appear as the operand of a multiplication,
/// division, modulus, or unary op or as the exponent of a power expression.
pub fn const_factor<'a, E>(input: TokenSlice<'a>) -> TokenResult<Spanned<ConstExpr>, E>
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
            let span = (&op_tok.span, &operand.span).into();

            Spanned {
                node: ConstExpr::UnaryOp {
                    op: UnaryOp::try_from(op_tok.string).unwrap(),
                    operand: Box::new(operand),
                },
                span,
            }
        },
    );

    alt((unary_op, const_power))(input)
}

/// Parse a constant power expression that may appear in the position of a
/// constant factor.
pub fn const_power<'a, E>(input: TokenSlice<'a>) -> TokenResult<Spanned<ConstExpr>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let bin_op = map(
        separated_pair(const_atom, op_string("**"), const_factor),
        |res| {
            let (left, right) = res;
            let span = (&left.span, &right.span).into();

            Spanned {
                node: ConstExpr::BinOp {
                    left: Box::new(left),
                    op: Operator::Pow,
                    right: Box::new(right),
                },
                span,
            }
        },
    );

    alt((bin_op, const_atom))(input)
}

/// Parse a constant atom expression that may appear in the position of a
/// constant power or as the base of a constant power expression.
pub fn const_atom<'a, E>(input: TokenSlice<'a>) -> TokenResult<Spanned<ConstExpr>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    alt((
        const_group,
        map(name_token, |t| Spanned {
            node: ConstExpr::Name { name: t.string },
            span: t.span,
        }),
        map(number_token, |t| Spanned {
            node: ConstExpr::Num { num: t.string },
            span: t.span,
        }),
    ))(input)
}

/// Parse a parenthesized constant group that may appear in the position of a
/// constant atom.
pub fn const_group<'a, E>(input: TokenSlice<'a>) -> TokenResult<Spanned<ConstExpr>, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let (input, l_paren) = op_string("(")(input)?;
    let (input, spanned_expr) = const_expr(input)?;
    let (input, r_paren) = op_string(")")(input)?;

    Ok((
        input,
        Spanned {
            node: spanned_expr.node,
            span: (&l_paren.span, &r_paren.span).into(),
        },
    ))
}
