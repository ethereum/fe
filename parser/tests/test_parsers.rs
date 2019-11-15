use nom::error::{
    ErrorKind,
    ParseError,
    VerboseError,
};
use nom::multi::many0;
use nom::Err as NomErr;

use vyper_parser::ast::*;
use vyper_parser::errors::format_debug_error;
use vyper_parser::parsers::*;

type SimpleError<I> = (I, ErrorKind);

/// Convert a parser into one that can function as a standalone file parser.
/// File tokenizations can be interspersed with arbitrary `NEWLINE`
/// tokens and are also terminated with an `ENDMARKER` token.  Parsers
/// defined lower in the grammar tree are not intended to handle that
/// kind of tokenization which makes testing them difficult.  This
/// combinator helps with that.
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

/// Assert `$parser` succeeds when applied to the given input in `$examples`
/// with the expected output specified in `$examples` or `$expected`.
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

/// Assert that `$parser` succeeds when applied as a standalone parser to
/// the given input in `$examples` with the expected output specified in
/// `$examples` or `$expected`.  Print a debug trace if parsing fails.
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

/// Assert `$parser` returns an error when applied as a standalone parser to
/// the given input in `$examples`.
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
    use vyper_parser::ast::ConstExpr::*;
    use vyper_parser::ast::Operator::*;
    use vyper_parser::ast::UnaryOp::*;

    assert_standalone_parser_success!(
        const_expr,
        vec![
            ("1", Ok((empty_slice!(), Num("1".into()),))),
            (
                "-1",
                Ok((
                    empty_slice!(),
                    UnaryOp {
                        op: USub,
                        operand: Box::new(Num("1".into())),
                    }
                ))
            ),
            ("name", Ok((empty_slice!(), Name("name".into()),))),
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
        Ok((empty_slice!(), Module { body: vec![] })),
    );
}

#[test]
fn test_file_input_one_stmt() {
    use vyper_parser::ast::ModuleStmt::*;

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
        empty_slice!(),
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
    use vyper_parser::ast::ModuleStmt::*;

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
        empty_slice!(),
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
