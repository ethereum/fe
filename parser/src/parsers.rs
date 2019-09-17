use std::convert::TryFrom;

use nom::branch::alt;
use nom::combinator::{map, verify};
use nom::error::{context, ErrorKind, ParseError};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, preceded, separated_pair};
use nom::IResult;

use crate::ast::ModuleStmt::*;
use crate::ast::*;
use crate::errors::make_error;
use crate::tokenizer::tokenize::tokenize;
use crate::tokenizer::types::{TokenInfo, TokenType};

pub type TokenRef<'a> = &'a TokenInfo<'a>;
pub type TokenSlice<'a> = &'a [TokenInfo<'a>];
pub type TokenResult<'a, O, E> = IResult<TokenSlice<'a>, O, E>;

/// Tokenize the given source code in `source` and filter out tokens not relevant to parsing.
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

/// Parse a module definition from a list of tokens produced from a source file.
pub fn file_input<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, Module, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    // (NEWLINE* module_stmt)*
    let (input, body) = many0(preceded(many0(newline_token), module_stmt))(input)?;

    // NEWLINE*
    let (input, _) = many0(newline_token)(input)?;

    // ENDMARKER
    let (input, _) = endmarker_token(input)?;

    Ok((input, Module { body }))
}

/// Parse a module statement, such as a contract definition, into a `ModuleStmt` object.
pub fn module_stmt<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ModuleStmt, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let (input, module_stmt) = context("expected event definition", parse_event_def)(input)?;

    Ok((input, module_stmt))
}

/// Parse an event definition statement into a `ModuleStmt::EventDef` object.
pub fn parse_event_def<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ModuleStmt, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    // "event" name ":" <newline>
    let (input, _) = name_string("event")(input)?;
    let (input, name) = name_token(input)?;
    let (input, _) = op_string(":")(input)?;
    let (input, _) = newline_token(input)?;

    // <indent> event_field+ <dedent>
    let (input, _) = indent_token(input)?;
    let (input, fields) = many1(parse_event_field)(input)?;
    let (input, _) = dedent_token(input)?;

    Ok((
        input,
        EventDef {
            name: name.string.into(),
            fields: fields,
        },
    ))
}

/// Parse an event field definition into an `EventField` object.
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
            name: name.string.into(),
            typ: typ.string.into(),
        },
    ))
}

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
        left_expr = ConstExpr::BinOp {
            left: Box::new(left_expr),
            op: Operator::try_from(op_tok.string).unwrap(),
            right: Box::new(right_expr),
        };
    }

    Ok((input, left_expr))
}

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
        left_expr = ConstExpr::BinOp {
            left: Box::new(left_expr),
            op: Operator::try_from(op_tok.string).unwrap(),
            right: Box::new(right_expr),
        };
    }

    Ok((input, left_expr))
}

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
            ConstExpr::UnaryOp {
                op: UnaryOp::try_from(op_tok.string).unwrap(),
                operand: Box::new(operand),
            }
        },
    );

    alt((unary_op, const_power))(input)
}

pub fn const_power<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let bin_op = map(
        separated_pair(const_atom, op_string("**"), const_factor),
        |res| {
            let (left, right) = res;
            ConstExpr::BinOp {
                left: Box::new(left),
                op: Operator::Pow,
                right: Box::new(right),
            }
        },
    );

    alt((bin_op, const_atom))(input)
}

pub fn const_atom<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    alt((
        const_group,
        map(name_token, |t| ConstExpr::Name(t.string.into())),
        map(number_token, |t| ConstExpr::Num(t.string.into())),
    ))(input)
}

pub fn const_group<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    delimited(op_string("("), const_expr, op_string(")"))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    use nom::error::{ErrorKind, VerboseError};
    use nom::Err as NomErr;

    use crate::errors::format_debug_error;

    type SimpleError<I> = (I, ErrorKind);

    /// Convert a parser into one that can function as a standalone file parser.  File
    /// tokenizations can be interspersed with arbitrary `NEWLINE` tokens and are also terminated
    /// with an `ENDMARKER` token.  Parsers defined lower in the grammar tree are not intended to
    /// handle that kind of tokenization which makes testing them difficult.  This combinator helps
    /// with that.
    fn standalone<'a, O, E, F>(parser: F) -> impl Fn(TokenSlice<'a>) -> TokenResult<'a, O, E>
    where
        E: ParseError<TokenSlice<'a>>,
        F: Fn(TokenSlice<'a>) -> TokenResult<'a, O, E>,
    {
        move |input: TokenSlice<'a>| {
            let (input, _) = many0(newline_token)(input)?;
            let (input, o) = parser(input)?;
            let (input, _) = many0(newline_token)(input)?;
            let (input, _) = endmarker_token(input)?;

            Ok((input, o))
        }
    }

    /// Assert `$parser` succeeds when applied to the given input in `$examples` with the expected
    /// output specified in `$examples` or `$expected`.
    macro_rules! assert_parser_success {
        ($parser:expr, $examples:expr,) => {{
            assert_parser_success!($parser, $examples);
        }};
        ($parser:expr, $examples:expr) => {{
            for (inp, expected) in $examples {
                let tokens = get_parse_tokens(inp).unwrap();
                let actual = $parser(&tokens[..]);

                assert_eq!(actual, expected);
            }
        }};
        ($parser:expr, $examples:expr, $expected:expr,) => {{
            assert_parser_success!($parser, $examples, $expected);
        }};
        ($parser:expr, $examples:expr, $expected:expr) => {{
            for inp in $examples {
                let tokens = get_parse_tokens(inp).unwrap();
                let actual = $parser(&tokens[..]);

                assert_eq!(actual, $expected);
            }
        }};
    }

    /// Assert that `$parser` succeeds when applied as a standalone parser to the given input in
    /// `$examples` with the expected output specified in `$examples` or `$expected`.  Print a
    /// debug trace if parsing fails.
    macro_rules! assert_standalone_parser_success {
        ($parser:ident, $examples:expr,) => {{
            assert_standalone_parser_success!($parser, $examples);
        }};
        ($parser:ident, $examples:expr) => {{
            for (inp, expected) in $examples {
                let tokens = get_parse_tokens(inp).unwrap();
                let actual = standalone($parser::<VerboseError<_>>)(&tokens[..]);

                if let Err(err) = &actual {
                    match err {
                        NomErr::Error(e) | NomErr::Failure(e) => {
                            println!("Parsing trace:\n{}", format_debug_error(inp, e.clone()));
                        }
                        _ => (),
                    }
                }

                assert_eq!(actual, expected);
            }
        }};
        ($parser:ident, $examples:expr, $expected:expr,) => {{
            assert_standalone_parser_success!($parser, $examples, $expected);
        }};
        ($parser:ident, $examples:expr, $expected:expr) => {{
            for inp in $examples {
                let tokens = get_parse_tokens(inp).unwrap();
                let actual = standalone($parser::<VerboseError<_>>)(&tokens[..]);

                assert_eq!(actual, $expected);
            }
        }};
    }

    /// Assert `$parser` returns an error when applied as a standalone parser to the given input in
    /// `$examples`.
    macro_rules! assert_standalone_parser_error {
        ($parser:ident, $examples:expr,) => {{
            assert_standalone_parser_error!($parser, $examples)
        }};
        ($parser:ident, $examples:expr) => {{
            for inp in $examples {
                let tokens = get_parse_tokens(inp).unwrap();
                let actual = standalone($parser::<SimpleError<_>>)(&tokens[..]);

                assert!(actual.is_err());
            }
        }};
    }

    macro_rules! empty_slice {
        () => {
            &[][..]
        };
    }

    #[test]
    fn test_const_expr_success() {
        use crate::ast::ConstExpr::*;
        use crate::ast::Operator::*;

        assert_standalone_parser_success!(
            const_expr,
            vec![
                (
                    "2 ** 8",
                    Ok((
                        empty_slice!(),
                        BinOp {
                            left: Box::new(Num("2".into())),
                            op: Pow,
                            right: Box::new(Num("8".into())),
                        }
                    ))
                ),
                (
                    "CONST_1 ** (CONST_2 + CONST_3)",
                    Ok((
                        empty_slice!(),
                        BinOp {
                            left: Box::new(Name("CONST_1".into())),
                            op: Pow,
                            right: Box::new(BinOp {
                                left: Box::new(Name("CONST_2".into())),
                                op: Add,
                                right: Box::new(Name("CONST_3".into())),
                            }),
                        }
                    ))
                ),
            ],
        );
    }

    #[test]
    fn test_file_input_empty_file() {
        // Empty file
        assert_parser_success!(
            file_input::<SimpleError<_>>,
            vec!["", "  \t ", " \n\n   \t \n \t "],
            Ok((&[][..], Module { body: vec![] })),
        );
    }

    #[test]
    fn test_file_input_one_stmt() {
        let examples = vec![
            // No leading or trailing whitespace
            r"event Greet:
    name: bytes32
    age: uint8",
            // Leading whitespace
            r"
event Greet:
    name: bytes32
    age: uint8",
            // Leading and trailing whitespace
            r"
event Greet:
    name: bytes32
    age: uint8
",
        ];
        let expected: TokenResult<_, SimpleError<_>> = Ok((
            &[][..],
            Module {
                body: vec![EventDef {
                    name: "Greet".into(),
                    fields: vec![
                        EventField {
                            name: "name".into(),
                            typ: "bytes32".into(),
                        },
                        EventField {
                            name: "age".into(),
                            typ: "uint8".into(),
                        },
                    ],
                }],
            },
        ));
        assert_parser_success!(file_input::<SimpleError<_>>, examples, expected);
    }

    #[test]
    fn test_file_input_many_stmt() {
        let examples = vec![
            // No leading, mid, or trailing whitespace
            r"event Greet:
    name: bytes32
    age: uint8
event Other:
    info1: uint256
    info2: bool",
            // Leading whitespace
            r"
event Greet:
    name: bytes32
    age: uint8
event Other:
    info1: uint256
    info2: bool",
            // Leading and trailing whitespace
            r"
event Greet:
    name: bytes32
    age: uint8
event Other:
    info1: uint256
    info2: bool
",
            // Leading, mid, and trailing whitespace
            r"
event Greet:
    name: bytes32
    age: uint8

event Other:
    info1: uint256
    info2: bool
",
        ];
        let expected: TokenResult<_, SimpleError<_>> = Ok((
            &[][..],
            Module {
                body: vec![
                    EventDef {
                        name: "Greet".into(),
                        fields: vec![
                            EventField {
                                name: "name".into(),
                                typ: "bytes32".into(),
                            },
                            EventField {
                                name: "age".into(),
                                typ: "uint8".into(),
                            },
                        ],
                    },
                    EventDef {
                        name: "Other".into(),
                        fields: vec![
                            EventField {
                                name: "info1".into(),
                                typ: "uint256".into(),
                            },
                            EventField {
                                name: "info2".into(),
                                typ: "bool".into(),
                            },
                        ],
                    },
                ],
            },
        ));
        assert_parser_success!(file_input::<SimpleError<_>>, examples, expected);
    }
}
