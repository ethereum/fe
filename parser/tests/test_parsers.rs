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
    P: Fn(Cursor<'a>) -> ParseResult<O>,
{
    move |input| terminated(many1(terminated(&parser, newline_token)), endmarker_token)(input)
}

/// Convert a parser into one that repeatedly applies itself to consume all
/// tokens in a source file.
fn repeat<'a, O, P>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<Vec<O>>
where
    P: Fn(Cursor<'a>) -> ParseResult<O>,
{
    move |input| terminated(many1(&parser), endmarker_token)(input)
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

macro_rules! parser_fixture_tests {
    ($(($parser:expr, $tester_name:ident, $fixture_path:expr,)),+,) => {
        $(
        #[test]
        #[wasm_bindgen_test]
        fn $tester_name() {
            do_with_fixtures!(
                assert_fixture_parsed_with!($parser),
                $fixture_path,
            );
        }
        )+
    };
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

parser_fixture_tests! {
    (
        non_empty_file_input,
        test_non_empty_file_input,
        "fixtures/parsers/non_empty_file_input.ron",
    ),
    (
        repeat(module_stmt),
        test_module_stmt,
        "fixtures/parsers/module_stmt.ron",
    ),
    (
        repeat(import_stmt),
        test_import_stmt,
        "fixtures/parsers/import_stmt.ron",
    ),
    (
        repeat_newline(simple_import),
        test_simple_import,
        "fixtures/parsers/simple_import.ron",
    ),
    (
        repeat_newline(simple_import_name),
        test_simple_import_name,
        "fixtures/parsers/simple_import_name.ron",
    ),
    (
        repeat_newline(from_import),
        test_from_import,
        "fixtures/parsers/from_import.ron",
    ),
    (
        repeat_newline(from_import_sub_path),
        test_from_import_sub_path,
        "fixtures/parsers/from_import_sub_path.ron",
    ),
    (
        repeat_newline(from_import_names),
        test_from_import_names,
        "fixtures/parsers/from_import_names.ron",
    ),
    (
        repeat_newline(from_import_names_list),
        test_from_import_names_list,
        "fixtures/parsers/from_import_names_list.ron",
    ),
    (
        repeat_newline(from_import_name),
        test_from_import_name,
        "fixtures/parsers/from_import_name.ron",
    ),
    (
        repeat_newline(dotted_name),
        test_dotted_name,
        "fixtures/parsers/dotted_name.ron",
    ),
    (
        repeat_newline(dots_to_int),
        test_dots_to_int,
        "fixtures/parsers/dots_to_int.ron",
    ),
    (
        repeat(contract_def),
        test_contract_def,
        "fixtures/parsers/contract_def.ron",
    ),
    (
        repeat(contract_stmt),
        test_contract_stmt,
        "fixtures/parsers/contract_stmt.ron",
    ),
    (
        repeat(contract_field),
        test_contract_field,
        "fixtures/parsers/contract_field.ron",
    ),
    (
        repeat(event_def),
        test_event_def,
        "fixtures/parsers/event_def.ron",
    ),
    (
        repeat(event_field),
        test_event_field,
        "fixtures/parsers/event_field.ron",
    ),
    (
        repeat_newline(type_def),
        test_type_def,
        "fixtures/parsers/type_def.ron",
    ),
    (
        repeat_newline(type_desc),
        test_type_desc,
        "fixtures/parsers/type_desc.ron",
    ),
    (
        repeat_newline(map_type),
        test_map_type,
        "fixtures/parsers/map_type.ron",
    ),
    (
        repeat_newline(base_type),
        test_base_type,
        "fixtures/parsers/base_type.ron",
    ),
    (
        standalone(arr_list),
        test_arr_list,
        "fixtures/parsers/arr_list.ron",
    ),
    (
        repeat_newline(arr_dim),
        test_arr_dim,
        "fixtures/parsers/arr_dim.ron",
    ),
    (
        repeat_newline(contract_field_qual),
        test_contract_field_qual,
        "fixtures/parsers/contract_field_qual.ron",
    ),
    (
        repeat_newline(event_field_qual),
        test_event_field_qual,
        "fixtures/parsers/event_field_qual.ron",
    ),
    (
        repeat_newline(func_qual),
        test_func_qual,
        "fixtures/parsers/func_qual.ron",
    ),
    (
        repeat_newline(exprs),
        test_exprs,
        "fixtures/parsers/exprs.ron",
    ),
    (
        repeat_newline(expr),
        test_expr,
        "fixtures/parsers/expr.ron",
    ),
    (
        repeat_newline(disjunct),
        test_disjunct,
        "fixtures/parsers/disjunct.ron",
    ),
    (
        repeat_newline(conjunct),
        test_conjunct,
        "fixtures/parsers/conjunct.ron",
    ),
    (
        repeat_newline(comparison),
        test_comparison,
        "fixtures/parsers/comparison.ron",
    ),
    (
        repeat_newline(comp_op),
        test_comp_op,
        "fixtures/parsers/comp_op.ron",
    ),
    (
        repeat_newline(bitwise_or),
        test_bitwise_or,
        "fixtures/parsers/bitwise_or.ron",
    ),
    (
        repeat_newline(bitwise_xor),
        test_bitwise_xor,
        "fixtures/parsers/bitwise_xor.ron",
    ),
    (
        repeat_newline(bitwise_and),
        test_bitwise_and,
        "fixtures/parsers/bitwise_and.ron",
    ),
    (
        repeat_newline(shift_expr),
        test_shift_expr,
        "fixtures/parsers/shift_expr.ron",
    ),
    (
        repeat_newline(sum),
        test_sum,
        "fixtures/parsers/sum.ron",
    ),
    (
        repeat_newline(term),
        test_term,
        "fixtures/parsers/term.ron",
    ),
    (
        repeat_newline(factor),
        test_factor,
        "fixtures/parsers/factor.ron",
    ),
    (
        repeat_newline(power),
        test_power,
        "fixtures/parsers/power.ron",
    ),
    (
        repeat_newline(primary),
        test_primary,
        "fixtures/parsers/primary.ron",
    ),
    (
        repeat_newline(slices),
        test_slices,
        "fixtures/parsers/slices.ron",
    ),
    (
        repeat_newline(slice),
        test_slice,
        "fixtures/parsers/slice.ron",
    ),
    (
        repeat_newline(atom),
        test_atom,
        "fixtures/parsers/atom.ron",
    ),
    (
        repeat_newline(list),
        test_list,
        "fixtures/parsers/list.ron",
    ),
    (
        repeat_newline(tuple),
        test_tuple,
        "fixtures/parsers/tuple.ron",
    ),
    (
        repeat_newline(group),
        test_group,
        "fixtures/parsers/group.ron",
    ),
    (
        repeat_newline(args),
        test_args,
        "fixtures/parsers/args.ron",
    ),
    (
        repeat_newline(kwargs),
        test_kwargs,
        "fixtures/parsers/kwargs.ron",
    ),
    (
        repeat_newline(kwarg),
        test_kwarg,
        "fixtures/parsers/kwarg.ron",
    ),
}
