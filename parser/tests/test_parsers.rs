extern crate wasm_bindgen_test;

#[macro_use]
mod utils;

use wasm_bindgen_test::wasm_bindgen_test;

use utils::{
    parse_test_example,
    to_ron_string_pretty,
};
use vyper_parser::ast::Module;
use vyper_parser::builders::{
    many0,
    many1,
    terminated,
};
use vyper_parser::errors::ParseError;
use vyper_parser::parsers::*;
use vyper_parser::span::{
    Span,
    Spanned,
};
use vyper_parser::{
    get_parse_tokens,
    Cursor,
    ParseResult,
};

/// Convert a parser into one that can function as a standalone file parser.
/// File tokenizations always include a trailing `NEWLINE` and `ENDMARKER`
/// token.  Parsers defined lower in the grammar tree are not intended to handle
/// that kind of tokenization.  This combinator modifies lower-level parsers to
/// handle such tokenizations to facilitate unit testing.
fn standalone<'a, O, P>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<O>
where
    P: Fn(Cursor<'a>) -> ParseResult<O>,
{
    move |input| {
        let (input, o) = parser(input)?;
        let (input, _) = newline_token(input)?;
        let (input, _) = endmarker_token(input)?;

        Ok((input, o))
    }
}

/// Convert a parser into one that can function as a standalone parser that
/// applies itself one or more times to an input and returns all outputs in a
/// vector.
fn standalone_vec<'a, O, P>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<Vec<O>>
where
    P: Fn(Cursor<'a>) -> ParseResult<O> + Copy,
{
    move |input| {
        let (input, o) = many1(terminated(parser, newline_token))(input)?;
        let (input, _) = endmarker_token(input)?;

        Ok((input, o))
    }
}

/// Assert `$parser` succeeds when applied to the given input in `$examples`
/// with the expected output specified in `$examples` or `$expected`.
macro_rules! assert_parser_ok {
    ($parser:expr, $examples:expr,) => {{
        assert_parser_ok!($parser, $examples);
    }};
    ($parser:expr, $examples:expr) => {{
        for (inp, expected) in $examples {
            let tokens = get_parse_tokens(inp).unwrap();
            let actual = $parser(&tokens[..]);

            assert_eq!(actual, expected);
        }
    }};
}

/// Create an assertion callback to assert that `$parser` succeeds and parses
/// all tokens when applied to the input from a test fixture.  Print a debug
/// trace if parsing fails.
macro_rules! assert_fixture_parsed_with {
    ($parser:expr) => {{
        move |filename: &str, inp: &str, expected_ser: &str| {
            let tokens = get_parse_tokens(inp).unwrap();
            let actual = $parser(&tokens[..]);

            if let Err(err) = &actual {
                println!("Parsing trace:\n{}", err.format_debug(inp, true));
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
#[wasm_bindgen_test]
fn test_many0_err() {
    assert_eq!(
        many0(next)(empty_slice!()),
        Err(ParseError::eof(empty_slice!()))
    );
}

#[test]
#[wasm_bindgen_test]
fn test_next_err() {
    assert_eq!(next(empty_slice!()), Err(ParseError::eof(empty_slice!())));
}

#[test]
#[wasm_bindgen_test]
fn test_const_expr_ok() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone(const_expr)),
        "fixtures/parsers/const_expr/number_1.ron",
        "fixtures/parsers/const_expr/number_2.ron",
        "fixtures/parsers/const_expr/name_1.ron",
        "fixtures/parsers/const_expr/power_1.ron",
        "fixtures/parsers/const_expr/power_2.ron",
        "fixtures/parsers/const_expr/power_3.ron",
        "fixtures/parsers/const_expr/lots_of_operators.ron",
        "fixtures/parsers/const_expr/unary_ops.ron",
        "fixtures/parsers/const_expr/chain_of_adds.ron",
        "fixtures/parsers/const_expr/chain_of_muls.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_file_input_empty_file() {
    // Empty file
    assert_parser_ok!(
        file_input,
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
#[wasm_bindgen_test]
fn test_file_input() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(file_input),
        "fixtures/parsers/file_input/one_stmt_no_whitespace.ron",
        "fixtures/parsers/file_input/one_stmt_leading_whitespace.ron",
        "fixtures/parsers/file_input/one_stmt_leading_trailing.ron",
        "fixtures/parsers/file_input/one_stmt_form_feed.ron",
        "fixtures/parsers/file_input/many_stmt_no_whitespace.ron",
        "fixtures/parsers/file_input/many_stmt_leading_whitespace.ron",
        "fixtures/parsers/file_input/many_stmt_leading_trailing.ron",
        "fixtures/parsers/file_input/many_stmt_lots_of_whitespace.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_import_stmt() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(terminated(many1(import_stmt), endmarker_token)),
        "fixtures/parsers/import_stmt.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_simple_import() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(simple_import)),
        "fixtures/parsers/simple_import.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_simple_import_name() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(simple_import_name)),
        "fixtures/parsers/simple_import_name.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_from_import() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(from_import)),
        "fixtures/parsers/from_import.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_from_import_sub_path() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(from_import_sub_path)),
        "fixtures/parsers/from_import_sub_path.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_from_import_names() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(from_import_names)),
        "fixtures/parsers/from_import_names.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_from_import_names_list() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(from_import_names_list)),
        "fixtures/parsers/from_import_names_list.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_from_import_name() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(from_import_name)),
        "fixtures/parsers/from_import_name.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_dotted_name() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(dotted_name)),
        "fixtures/parsers/dotted_name.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_dots_to_int() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(dots_to_int)),
        "fixtures/parsers/dots_to_int.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_type_def() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(type_def)),
        "fixtures/parsers/type_def.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_type_desc() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(type_desc)),
        "fixtures/parsers/type_desc.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_map_type() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(map_type)),
        "fixtures/parsers/map_type.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_base_type() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(base_type)),
        "fixtures/parsers/base_type.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_arr_list() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone(arr_list)),
        "fixtures/parsers/arr_list.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_arr_dim() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(arr_dim)),
        "fixtures/parsers/arr_dim.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_arr_dim_err() {
    let src = "[1.0]";
    let toks = get_parse_tokens(src).unwrap();

    assert_eq!(
        standalone(arr_dim)(&toks),
        Err(ParseError::str(
            &toks[1..],
            "invalid integer literal \"1.0\"".to_string(),
        )),
    );
}

#[test]
#[wasm_bindgen_test]
fn test_contract_field_qual() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(contract_field_qual)),
        "fixtures/parsers/contract_field_qual.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_event_field_qual() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(event_field_qual)),
        "fixtures/parsers/event_field_qual.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_func_qual() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(standalone_vec(func_qual)),
        "fixtures/parsers/func_qual.ron",
    );
}
