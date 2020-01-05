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
    pair,
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

/// Convert a parser into one that can parse all tokens in a source file
/// (including the default `ENDMARKER` token).
fn standalone<'a, O, P>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<O>
where
    P: Fn(Cursor<'a>) -> ParseResult<O>,
{
    terminated(parser, pair(newline_token, endmarker_token))
}

/// Convert a parser into one that repeatedly applies itself to consume all
/// tokens in a source file.  Repetitions are assumed to be terminated by
/// `NEWLINE` tokens.
fn repeat_newline<'a, O, P>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<Vec<O>>
where
    P: Fn(Cursor<'a>) -> ParseResult<O> + Copy,
{
    terminated(many1(terminated(parser, newline_token)), endmarker_token)
}

/// Convert a parser into one that repeatedly applies itself to consume all
/// tokens in a source file.
fn repeat<'a, O, P>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<Vec<O>>
where
    P: Fn(Cursor<'a>) -> ParseResult<O> + Copy,
{
    terminated(many1(parser), endmarker_token)
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

            assert_eq!(
                actual,
                expected,
                "\n===== Input =====\n{:#?}\n===== Actual =====\n{:#?}\n===== Expected =====\n{:#?}",
                inp,
                actual,
                expected,
            );
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
                eprintln!("===== Error result for =====");
                eprintln!("* filename: {}", filename);
                eprintln!("* parsed with: {}", stringify!($parser));
                eprint!(
                    "\n===== Parsing trace =====\n{}",
                    err.format_debug(inp, true)
                );
                //eprintln!("===== Error =====\n{:#?}", err);
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
fn test_file_input() {
    assert_parser_ok!(
        file_input,
        vec![(
            "",
            Ok((
                empty_slice!(),
                Spanned {
                    node: Module { body: vec![] },
                    span: Span::new(0, 0),
                }
            ))
        )],
    );

    do_with_fixtures!(
        assert_fixture_parsed_with!(file_input),
        "fixtures/parsers/non_empty_file_input.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_empty_file_input() {
    assert_parser_ok!(
        empty_file_input,
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
fn test_non_empty_file_input() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(non_empty_file_input),
        "fixtures/parsers/non_empty_file_input.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_module_stmt() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat(module_stmt)),
        "fixtures/parsers/module_stmt.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_import_stmt() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat(import_stmt)),
        "fixtures/parsers/import_stmt.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_simple_import() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(simple_import)),
        "fixtures/parsers/simple_import.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_simple_import_name() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(simple_import_name)),
        "fixtures/parsers/simple_import_name.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_from_import() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(from_import)),
        "fixtures/parsers/from_import.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_from_import_sub_path() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(from_import_sub_path)),
        "fixtures/parsers/from_import_sub_path.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_from_import_names() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(from_import_names)),
        "fixtures/parsers/from_import_names.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_from_import_names_list() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(from_import_names_list)),
        "fixtures/parsers/from_import_names_list.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_from_import_name() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(from_import_name)),
        "fixtures/parsers/from_import_name.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_dotted_name() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(dotted_name)),
        "fixtures/parsers/dotted_name.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_dots_to_int() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(dots_to_int)),
        "fixtures/parsers/dots_to_int.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_contract_def() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat(contract_def)),
        "fixtures/parsers/contract_def.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_contract_stmt() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat(contract_stmt)),
        "fixtures/parsers/contract_stmt.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_contract_field() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat(contract_field)),
        "fixtures/parsers/contract_field.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_event_def() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat(event_def)),
        "fixtures/parsers/event_def.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_event_field() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat(event_field)),
        "fixtures/parsers/event_field.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_type_def() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(type_def)),
        "fixtures/parsers/type_def.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_type_desc() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(type_desc)),
        "fixtures/parsers/type_desc.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_map_type() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(map_type)),
        "fixtures/parsers/map_type.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_base_type() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(base_type)),
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
        assert_fixture_parsed_with!(repeat_newline(arr_dim)),
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
        assert_fixture_parsed_with!(repeat_newline(contract_field_qual)),
        "fixtures/parsers/contract_field_qual.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_event_field_qual() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(event_field_qual)),
        "fixtures/parsers/event_field_qual.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_func_qual() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(func_qual)),
        "fixtures/parsers/func_qual.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_const_expr() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(const_expr)),
        "fixtures/parsers/const_expr.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_const_term() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(const_term)),
        "fixtures/parsers/const_term.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_const_factor() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(const_factor)),
        "fixtures/parsers/const_factor.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_const_power() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(const_power)),
        "fixtures/parsers/const_power.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_const_atom() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(const_atom)),
        "fixtures/parsers/const_atom.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_const_group() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(const_group)),
        "fixtures/parsers/const_group.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_exprs() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(exprs)),
        "fixtures/parsers/exprs.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_expr() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(expr)),
        "fixtures/parsers/expr.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_disjunct() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(disjunct)),
        "fixtures/parsers/disjunct.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_conjunct() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(conjunct)),
        "fixtures/parsers/conjunct.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_comparison() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(comparison)),
        "fixtures/parsers/comparison.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_comp_op() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(comp_op)),
        "fixtures/parsers/comp_op.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_bitwise_or() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(bitwise_or)),
        "fixtures/parsers/bitwise_or.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_bitwise_xor() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(bitwise_xor)),
        "fixtures/parsers/bitwise_xor.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_bitwise_and() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(bitwise_and)),
        "fixtures/parsers/bitwise_and.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_shift_expr() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(shift_expr)),
        "fixtures/parsers/shift_expr.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_sum() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(sum)),
        "fixtures/parsers/sum.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_term() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(term)),
        "fixtures/parsers/term.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_factor() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(factor)),
        "fixtures/parsers/factor.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_power() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(power)),
        "fixtures/parsers/power.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_primary() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(primary)),
        "fixtures/parsers/primary.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_slices() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(slices)),
        "fixtures/parsers/slices.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_slice() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(slice)),
        "fixtures/parsers/slice.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_atom() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(atom)),
        "fixtures/parsers/atom.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_list() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(list)),
        "fixtures/parsers/list.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_tuple() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(tuple)),
        "fixtures/parsers/tuple.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_group() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(group)),
        "fixtures/parsers/group.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_args() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(args)),
        "fixtures/parsers/args.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_kwargs() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(kwargs)),
        "fixtures/parsers/kwargs.ron",
    );
}

#[test]
#[wasm_bindgen_test]
fn test_kwarg() {
    do_with_fixtures!(
        assert_fixture_parsed_with!(repeat_newline(kwarg)),
        "fixtures/parsers/kwarg.ron",
    );
}
