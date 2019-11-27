#[macro_use]
mod utils;
use utils::{
    parse_test_example,
    to_ron_string_pretty,
};

use nom::error::{
    ErrorKind,
    ParseError,
    VerboseError,
};
use nom::multi::many0;
use nom::Err as NomErr;

use vyper_parser::ast::Module;
use vyper_parser::errors::format_debug_error;
use vyper_parser::parsers::*;
use vyper_parser::span::{
    Span,
    Spanned,
};

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

/// Assert that `$parser` succeeds and parses all tokens when applied to the
/// given input.  Expected results are defined as serializations.  Print a debug
/// trace if parsing fails.
macro_rules! assert_fixtures_parsed {
    ($parser:expr, $($path:expr),+,) => {{
        assert_fixtures_parsed!($parser, $($path),+)
    }};
    ($parser:expr, $($path:expr),+) => {{
        let test_files = vec![
            $(($path, include_test_example!($path))),+
        ];

        for (filename, (inp, expected_ser)) in test_files {
            let tokens = get_parse_tokens(inp).unwrap();
            let actual = $parser(&tokens[..]);

            if let Err(err) = &actual {
                match err {
                    NomErr::Error(e) | NomErr::Failure(e) => {
                        println!("Parsing trace:\n{}", format_debug_error(inp, e.clone()));
                    }
                    _ => (),
                }
            }

            let (actual_remaining, actual_ast) = actual.unwrap();
            let actual_ser = to_ron_string_pretty(&actual_ast).unwrap();

            assert_eq!(actual_remaining, empty_slice!());
            assert_strings_eq!(
                actual_ser,
                expected_ser,
                "\nParsing results did not match for {}",
                filename,
            );
        }
    }};
}

#[test]
fn test_const_expr_success() {
    assert_fixtures_parsed!(
        standalone(const_expr::<VerboseError<_>>),
        "fixtures/parsers/const_expr/number_1.ron",
        "fixtures/parsers/const_expr/number_2.ron",
        "fixtures/parsers/const_expr/name_1.ron",
        "fixtures/parsers/const_expr/power_1.ron",
        "fixtures/parsers/const_expr/power_2.ron",
    );
}

#[test]
fn test_file_input_empty_file() {
    // Empty file
    assert_parser_success!(
        file_input::<SimpleError<_>>,
        vec![
            (
                "",
                Ok((
                    empty_slice!(),
                    Spanned {
                        node: Module { body: vec![] },
                        span: Span::new(0, 0),
                    }
                ))
            ),
            (
                "  \t ",
                Ok((
                    empty_slice!(),
                    Spanned {
                        node: Module { body: vec![] },
                        span: Span::new(4, 4),
                    }
                ))
            ),
            (
                " \n\n   \t \n \t ",
                Ok((
                    empty_slice!(),
                    Spanned {
                        node: Module { body: vec![] },
                        span: Span::new(12, 12),
                    }
                ))
            ),
        ],
    );
}

#[test]
fn test_file_input_one_stmt() {
    assert_fixtures_parsed!(
        file_input::<VerboseError<_>>,
        "fixtures/parsers/file_input/one_stmt_no_whitespace.ron",
        "fixtures/parsers/file_input/one_stmt_leading_whitespace.ron",
        "fixtures/parsers/file_input/one_stmt_leading_trailing.ron",
    );
}

#[test]
fn test_file_input_many_stmt() {
    assert_fixtures_parsed!(
        file_input::<VerboseError<_>>,
        "fixtures/parsers/file_input/many_stmt_no_whitespace.ron",
        "fixtures/parsers/file_input/many_stmt_leading_whitespace.ron",
        "fixtures/parsers/file_input/many_stmt_leading_trailing.ron",
        "fixtures/parsers/file_input/many_stmt_lots_of_whitespace.ron",
    );
}
