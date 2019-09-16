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
    let (i, body) = many0(preceded(many0(newline_token), module_stmt))(input)?;

    // NEWLINE*
    let (i, _) = many0(newline_token)(i)?;

    // ENDMARKER
    let (i, _) = endmarker_token(i)?;

    Ok((i, Module { body }))
}

/// Parse a module statement, such as a contract definition, into a `ModuleStmt` object.
pub fn module_stmt<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ModuleStmt, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let (i, module_stmt) = context("expected event definition", parse_event_def)(input)?;

    Ok((i, module_stmt))
}

/// Parse an event definition statement into a `ModuleStmt::EventDef` object.
pub fn parse_event_def<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ModuleStmt, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    // "event" name ":" <newline>
    let (i, _) = name_string("event")(input)?;
    let (i, name) = name_token(i)?;
    let (i, _) = op_string(":")(i)?;
    let (i, _) = newline_token(i)?;

    // <indent> event_field+ <dedent>
    let (i, _) = indent_token(i)?;
    let (i, fields) = many1(parse_event_field)(i)?;
    let (i, _) = dedent_token(i)?;

    Ok((
        i,
        EventDef {
            name: name.string.to_string(),
            fields: fields,
        },
    ))
}

/// Parse an event field definition into an `EventField` object.
pub fn parse_event_field<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, EventField, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let (i, name) = name_token(input)?;
    let (i, _) = op_string(":")(i)?;
    let (i, typ) = name_token(i)?;
    let (i, _) = newline_token(i)?;

    Ok((
        i,
        EventField {
            name: name.string.to_string(),
            typ: typ.string.into(),
        },
    ))
}

pub fn const_expr<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    const_factor(input)
}

pub fn const_factor<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let const_factor_left = map(
        pair(
            alt((op_string("+"), op_string("-"), op_string("~"))),
            const_factor,
        ),
        |res| ConstExpr::UnaryOp {
            op: match res.0.string {
                "+" => UnaryOp::UAdd,
                "-" => UnaryOp::USub,
                "~" => UnaryOp::Invert,
                _ => panic!("unreachable"),
            },
            operand: Box::new(res.1),
        },
    );

    alt((const_factor_left, const_power))(input)
}

pub fn const_power<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let const_power_left = map(
        separated_pair(const_paren, op_string("**"), const_factor),
        |res| ConstExpr::BinOp {
            left: Box::new(res.0),
            op: Operator::Pow,
            right: Box::new(res.1),
        },
    );

    alt((const_power_left, const_paren))(input)
}

pub fn const_paren<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    alt((
        delimited(op_string("("), const_expr, op_string(")")),
        const_atom,
    ))(input)
}

pub fn const_atom<'a, E>(input: TokenSlice<'a>) -> TokenResult<'a, ConstExpr, E>
where
    E: ParseError<TokenSlice<'a>>,
{
    let number_or_name = context(
        "number or name token",
        verify(one_token, |tok: &TokenInfo| {
            tok.typ == TokenType::NUMBER || tok.typ == TokenType::NAME
        }),
    );

    map(number_or_name, |tok| match tok.typ {
        TokenType::NUMBER => ConstExpr::Num(tok.string.into()),
        TokenType::NAME => ConstExpr::Name(tok.string.into()),
        _ => panic!("unreachable"),
    })(input)
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
            let (i, _) = many0(newline_token)(input)?;
            let (i, o) = parser(i)?;
            let (i, _) = many0(newline_token)(i)?;
            let (i, _) = endmarker_token(i)?;

            Ok((i, o))
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

    /// Assert `$parser` returns an error when applied to the given input in `$examples`.
    macro_rules! assert_parser_error {
        ($parser:expr, $examples:expr,) => {{
            assert_parser_error!($parser, $examples)
        }};
        ($parser:expr, $examples:expr) => {{
            for inp in $examples {
                let tokens = get_parse_tokens(inp).unwrap();
                let actual = $parser(&tokens[..]);

                assert!(actual.is_err());
            }
        }};
    }

    #[test]
    fn test_const_paren_success() {
        let empty_slice = &[][..];

        use crate::ast::ConstExpr::*;

        assert_standalone_parser_success!(
            const_paren,
            vec![
                ("(1)", Ok((empty_slice, Num("1".into())))),
                ("(CONST)", Ok((empty_slice, Name("CONST".into())))),
                ("1", Ok((empty_slice, Num("1".into())))),
            ],
        );
    }

    #[test]
    fn test_const_atom_success() {
        let empty_slice = &[][..];

        assert_standalone_parser_success!(
            const_atom,
            vec![
                ("1", Ok((empty_slice, ConstExpr::Num("1".into())))),
                ("asdf", Ok((empty_slice, ConstExpr::Name("asdf".into())))),
            ],
        );
    }

    #[test]
    fn test_const_atom_failure() {
        assert_parser_error!(
            standalone(const_atom::<SimpleError<_>>),
            vec!["(1)", "{ asdf }"],
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
                    name: "Greet".to_string(),
                    fields: vec![
                        EventField {
                            name: "name".to_string(),
                            typ: "bytes32".into(),
                        },
                        EventField {
                            name: "age".to_string(),
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
                        name: "Greet".to_string(),
                        fields: vec![
                            EventField {
                                name: "name".to_string(),
                                typ: "bytes32".into(),
                            },
                            EventField {
                                name: "age".to_string(),
                                typ: "uint8".into(),
                            },
                        ],
                    },
                    EventDef {
                        name: "Other".to_string(),
                        fields: vec![
                            EventField {
                                name: "info1".to_string(),
                                typ: "uint256".into(),
                            },
                            EventField {
                                name: "info2".to_string(),
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
