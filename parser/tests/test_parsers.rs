#[macro_use]
mod utils;

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

/// Assert that `$parser` succeeds and parses all tokens when applied as a
/// standalone parser to the given input.  Expected results are defined as
/// serializations.  Print a debug trace if parsing fails.
macro_rules! assert_all_parsed_with_serialization {
    ($parser:ident, $examples:expr,) => {{
        assert_all_parsed_with_serialization!($parser, $examples);
    }};
    ($parser:ident, $examples:expr) => {{
        for (inp, expected_serialization) in $examples {
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

            let (actual_remaining, actual_ast) = actual.unwrap();

            let mut serializer_config = ron::ser::PrettyConfig::default();
            serializer_config.indentor = "  ".to_string();

            let actual_serialization =
                ron::ser::to_string_pretty(&actual_ast, serializer_config).unwrap();

            assert_eq!(actual_remaining, empty_slice!());
            assert_strings_eq!(actual_serialization, expected_serialization);
        }
    }};
}

macro_rules! empty_slice {
    () => {
        &[][..]
    };
}

macro_rules! include_test_example {
    ($path:expr) => {{
        split_test_example(include_str!($path)).unwrap()
    }};
}

fn split_test_example<'a>(input: &'a str) -> Result<(&'a str, &'a str), &'static str> {
    let parts: Vec<_> = input.split("\n---\n").collect();

    if parts.len() != 2 {
        Err("Parsing example has wrong format")
    } else {
        let input = parts[0];
        let parsed = parts[1];

        // If single trailing newline is present, clip off
        match parsed.chars().last() {
            Some(c) if c == '\n' => Ok((input, &parsed[..parsed.len() - 1])),
            _ => Ok((input, parsed)),
        }
    }
}

#[test]
fn test_const_expr_success() {
    assert_all_parsed_with_serialization!(
        const_expr,
        vec![
            include_test_example!("fixtures/parsers/const_expr/number_1.ron"),
            include_test_example!("fixtures/parsers/const_expr/number_2.ron"),
            include_test_example!("fixtures/parsers/const_expr/name_1.ron"),
            include_test_example!("fixtures/parsers/const_expr/power_1.ron"),
            include_test_example!("fixtures/parsers/const_expr/power_2.ron"),
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
