extern crate wasm_bindgen_test;

#[macro_use]
mod utils;

use wasm_bindgen_test::wasm_bindgen_test;

use fe_parser::ast::Module;
use fe_parser::builders::{
    many0,
    many1,
    pair,
    terminated,
};
use fe_parser::errors::ParseError;
use fe_parser::parsers::*;
use fe_parser::span::{
    Span,
    Spanned,
};
use fe_parser::{
    get_parse_tokens,
    Cursor,
    ParseResult,
};
use utils::to_ron_string_pretty;

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
    ($(($parser:expr, $tester_name:ident, $writer_name:ident, $fixture_path:expr,)),+,) => {
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

        mod fixture_writers {
            use std::path::{
                Path,
                PathBuf,
            };
            use std::fs;
            use std::env;

            use super::*;

            fn get_fixture_content(fixture_name: &str) -> (String, PathBuf) {
                let fe_fixtures_path = env::var("FE_FIXTURES_PATH")
                    .expect("must set FE_FIXTURES_PATH env var to write fixtures");

                let mut path = PathBuf::from(fe_fixtures_path);
                path.push(fixture_name);

                let content = fs::read_to_string(&path)
                    .expect(&format!(
                        "could not read content from fixture path {}",
                        path.to_str().unwrap(),
                    ));

                (content, path)
            }

            mod writers {
                use super::*;

                $(
                pub fn $writer_name() {
                    let fixture_name = Path::new($fixture_path)
                        .file_name()
                        .expect("fixture path invalid");

                    let (content, path) = get_fixture_content(fixture_name.to_str().unwrap());

                    let (inp, _old_ser) = $crate::utils::parse_fixture(&content).unwrap();

                    let tokens = get_parse_tokens(inp).unwrap();
                    let (_, result) = $parser(&tokens[..]).unwrap();

                    let new_ser = to_ron_string_pretty(&result).unwrap();

                    let new_content = format!("{}\n---\n{}\n", inp, new_ser);

                    fs::write(path, new_content).unwrap();
                }
                )+
            }

            #[allow(dead_code)]
            pub fn write_all() {
                $(writers::$writer_name();)+
            }
        }
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

// Uncomment this to update all fixtures.
//#[test]
//fn write_fixtures() {
//    fixture_writers::write_all();
//}

// TODO: uncomment and fix
// parser_fixture_tests! {
//     (
//         file_input,
//         test_guest_book,
//         write_guest_book,
//         "fixtures/parsers/guest_book.ron",
//     ),
//     (
//         non_empty_file_input,
//         test_non_empty_file_input,
//         write_non_empty_file_input,
//         "fixtures/parsers/non_empty_file_input.ron",
//     ),
//     (
//         repeat(module_stmt),
//         test_module_stmt,
//         write_module_stmt,
//         "fixtures/parsers/module_stmt.ron",
//     ),
//     (
//         repeat(import_stmt),
//         test_import_stmt,
//         write_import_stmt,
//         "fixtures/parsers/import_stmt.ron",
//     ),
//     (
//         repeat_newline(simple_import),
//         test_simple_import,
//         write_simple_import,
//         "fixtures/parsers/simple_import.ron",
//     ),
//     (
//         repeat_newline(simple_import_name),
//         test_simple_import_name,
//         write_simple_import_name,
//         "fixtures/parsers/simple_import_name.ron",
//     ),
//     (
//         repeat_newline(from_import),
//         test_from_import,
//         write_from_import,
//         "fixtures/parsers/from_import.ron",
//     ),
//     (
//         repeat_newline(from_import_sub_path),
//         test_from_import_sub_path,
//         write_from_import_sub_path,
//         "fixtures/parsers/from_import_sub_path.ron",
//     ),
//     (
//         repeat_newline(from_import_names),
//         test_from_import_names,
//         write_from_import_names,
//         "fixtures/parsers/from_import_names.ron",
//     ),
//     (
//         repeat_newline(from_import_names_list),
//         test_from_import_names_list,
//         write_from_import_names_list,
//         "fixtures/parsers/from_import_names_list.ron",
//     ),
//     (
//         repeat_newline(from_import_name),
//         test_from_import_name,
//         write_from_import_name,
//         "fixtures/parsers/from_import_name.ron",
//     ),
//     (
//         repeat_newline(dotted_name),
//         test_dotted_name,
//         write_dotted_name,
//         "fixtures/parsers/dotted_name.ron",
//     ),
//     (
//         repeat_newline(dots_to_int),
//         test_dots_to_int,
//         write_dots_to_int,
//         "fixtures/parsers/dots_to_int.ron",
//     ),
//     (
//         repeat(contract_def),
//         test_contract_def,
//         write_contract_def,
//         "fixtures/parsers/contract_def.ron",
//     ),
//     (
//         repeat(contract_stmt),
//         test_contract_stmt,
//         write_contract_stmt,
//         "fixtures/parsers/contract_stmt.ron",
//     ),
//     (
//         repeat(contract_field),
//         test_contract_field,
//         write_contract_field,
//         "fixtures/parsers/contract_field.ron",
//     ),
//     (
//         repeat(event_def),
//         test_event_def,
//         write_event_def,
//         "fixtures/parsers/event_def.ron",
//     ),
//     (
//         repeat(event_field),
//         test_event_field,
//         write_event_field,
//         "fixtures/parsers/event_field.ron",
//     ),
//     (
//         repeat(func_def),
//         test_func_def,
//         write_func_def,
//         "fixtures/parsers/func_def.ron",
//     ),
//     (
//         repeat_newline(arg_list),
//         test_arg_list,
//         write_arg_list,
//         "fixtures/parsers/arg_list.ron",
//     ),
//     (
//         repeat_newline(arg_def),
//         test_arg_def,
//         write_arg_def,
//         "fixtures/parsers/arg_def.ron",
//     ),
//     (
//         repeat(type_def),
//         test_type_def,
//         write_type_def,
//         "fixtures/parsers/type_def.ron",
//     ),
//     (
//         repeat_newline(type_desc),
//         test_type_desc,
//         write_type_desc,
//         "fixtures/parsers/type_desc.ron",
//     ),
//     (
//         repeat_newline(map_type),
//         test_map_type,
//         write_map_type,
//         "fixtures/parsers/map_type.ron",
//     ),
//     (
//         repeat_newline(base_type),
//         test_base_type,
//         write_base_type,
//         "fixtures/parsers/base_type.ron",
//     ),
//     (
//         standalone(arr_list),
//         test_arr_list,
//         write_arr_list,
//         "fixtures/parsers/arr_list.ron",
//     ),
//     (
//         repeat_newline(arr_dim),
//         test_arr_dim,
//         write_arr_dim,
//         "fixtures/parsers/arr_dim.ron",
//     ),
//     (
//         repeat_newline(contract_field_qual),
//         test_contract_field_qual,
//         write_contract_field_qual,
//         "fixtures/parsers/contract_field_qual.ron",
//     ),
//     (
//         repeat_newline(event_field_qual),
//         test_event_field_qual,
//         write_event_field_qual,
//         "fixtures/parsers/event_field_qual.ron",
//     ),
//     (
//         repeat_newline(func_qual),
//         test_func_qual,
//         write_func_qual,
//         "fixtures/parsers/func_qual.ron",
//     ),
//     (
//         repeat(func_stmt),
//         test_func_stmt,
//         write_func_stmt,
//         "fixtures/parsers/func_stmt.ron",
//     ),
//     (
//         repeat(simple_stmt),
//         test_simple_stmt,
//         write_simple_stmt,
//         "fixtures/parsers/simple_stmt.ron",
//     ),
//     (
//         repeat_newline(small_stmt),
//         test_small_stmt,
//         write_small_stmt,
//         "fixtures/parsers/small_stmt.ron",
//     ),
//     (
//         repeat_newline(return_stmt),
//         test_return_stmt,
//         write_return_stmt,
//         "fixtures/parsers/return_stmt.ron",
//     ),
//     (
//         repeat_newline(assert_stmt),
//         test_assert_stmt,
//         write_assert_stmt,
//         "fixtures/parsers/assert_stmt.ron",
//     ),
//     (
//         repeat_newline(emit_stmt),
//         test_emit_stmt,
//         write_emit_stmt,
//         "fixtures/parsers/emit_stmt.ron",
//     ),
//     (
//         repeat_newline(pass_stmt),
//         test_pass_stmt,
//         write_pass_stmt,
//         "fixtures/parsers/pass_stmt.ron",
//     ),
//     (
//         repeat_newline(break_stmt),
//         test_break_stmt,
//         write_break_stmt,
//         "fixtures/parsers/break_stmt.ron",
//     ),
//     (
//         repeat_newline(continue_stmt),
//         test_continue_stmt,
//         write_continue_stmt,
//         "fixtures/parsers/continue_stmt.ron",
//     ),
//     (
//         repeat_newline(revert_stmt),
//         test_revert_stmt,
//         write_revert_stmt,
//         "fixtures/parsers/revert_stmt.ron",
//     ),
//     (
//         repeat_newline(vardecl_stmt),
//         test_vardecl_stmt,
//         write_vardecl_stmt,
//         "fixtures/parsers/vardecl_stmt.ron",
//     ),
//     (
//         repeat_newline(assign_stmt),
//         test_assign_stmt,
//         write_assign_stmt,
//         "fixtures/parsers/assign_stmt.ron",
//     ),
//     (
//         repeat_newline(augassign_stmt),
//         test_augassign_stmt,
//         write_augassign_stmt,
//         "fixtures/parsers/augassign_stmt.ron",
//     ),
//     (
//         repeat(compound_stmt),
//         test_compound_stmt,
//         write_compound_stmt,
//         "fixtures/parsers/compound_stmt.ron",
//     ),
//     (
//         repeat(if_stmt),
//         test_if_stmt,
//         write_if_stmt,
//         "fixtures/parsers/if_stmt.ron",
//     ),
//     (
//         repeat(for_stmt),
//         test_for_stmt,
//         write_for_stmt,
//         "fixtures/parsers/for_stmt.ron",
//     ),
//     (
//         repeat(while_stmt),
//         test_while_stmt,
//         write_while_stmt,
//         "fixtures/parsers/while_stmt.ron",
//     ),
//     (
//         repeat_newline(exprs),
//         test_exprs,
//         write_exprs,
//         "fixtures/parsers/exprs.ron",
//     ),
//     (
//         repeat_newline(expr),
//         test_expr,
//         write_expr,
//         "fixtures/parsers/expr.ron",
//     ),
//     (
//         repeat_newline(disjunct),
//         test_disjunct,
//         write_disjunct,
//         "fixtures/parsers/disjunct.ron",
//     ),
//     (
//         repeat_newline(conjunct),
//         test_conjunct,
//         write_conjunct,
//         "fixtures/parsers/conjunct.ron",
//     ),
//     (
//         repeat_newline(comparison),
//         test_comparison,
//         write_comparison,
//         "fixtures/parsers/comparison.ron",
//     ),
//     (
//         repeat_newline(comp_op),
//         test_comp_op,
//         write_comp_op,
//         "fixtures/parsers/comp_op.ron",
//     ),
//     (
//         repeat_newline(bitwise_or),
//         test_bitwise_or,
//         write_bitwise_or,
//         "fixtures/parsers/bitwise_or.ron",
//     ),
//     (
//         repeat_newline(bitwise_xor),
//         test_bitwise_xor,
//         write_bitwise_xor,
//         "fixtures/parsers/bitwise_xor.ron",
//     ),
//     (
//         repeat_newline(bitwise_and),
//         test_bitwise_and,
//         write_bitwise_and,
//         "fixtures/parsers/bitwise_and.ron",
//     ),
//     (
//         repeat_newline(shift_expr),
//         test_shift_expr,
//         write_shift_expr,
//         "fixtures/parsers/shift_expr.ron",
//     ),
//     (
//         repeat_newline(sum),
//         test_sum,
//         write_sum,
//         "fixtures/parsers/sum.ron",
//     ),
//     (
//         repeat_newline(term),
//         test_term,
//         write_term,
//         "fixtures/parsers/term.ron",
//     ),
//     (
//         repeat_newline(factor),
//         test_factor,
//         write_factor,
//         "fixtures/parsers/factor.ron",
//     ),
//     (
//         repeat_newline(power),
//         test_power,
//         write_power,
//         "fixtures/parsers/power.ron",
//     ),
//     (
//         repeat_newline(primary),
//         test_primary,
//         write_primary,
//         "fixtures/parsers/primary.ron",
//     ),
//     (
//         repeat_newline(slices),
//         test_slices,
//         write_slices,
//         "fixtures/parsers/slices.ron",
//     ),
//     (
//         repeat_newline(slice),
//         test_slice,
//         write_slice,
//         "fixtures/parsers/slice.ron",
//     ),
//     (
//         repeat_newline(atom),
//         test_atom,
//         write_atom,
//         "fixtures/parsers/atom.ron",
//     ),
//     (
//         repeat_newline(list),
//         test_list,
//         write_list,
//         "fixtures/parsers/list.ron",
//     ),
//     (
//         repeat_newline(tuple),
//         test_tuple,
//         write_tuple,
//         "fixtures/parsers/tuple.ron",
//     ),
//     (
//         repeat_newline(group),
//         test_group,
//         write_group,
//         "fixtures/parsers/group.ron",
//     ),
//     (
//         repeat_newline(args),
//         test_args,
//         write_args,
//         "fixtures/parsers/args.ron",
//     ),
//     (
//         repeat_newline(kwargs),
//         test_kwargs,
//         write_kwargs,
//         "fixtures/parsers/kwargs.ron",
//     ),
//     (
//         repeat_newline(kwarg),
//         test_kwarg,
//         write_kwarg,
//         "fixtures/parsers/kwarg.ron",
//     ),
//     (
//         repeat_newline(targets),
//         test_targets,
//         write_targets,
//         "fixtures/parsers/targets.ron",
//     ),
//     (
//         repeat_newline(target),
//         test_target,
//         write_target,
//         "fixtures/parsers/target.ron",
//     ),
//     (
//         repeat_newline(t_atom),
//         test_t_atom,
//         write_t_atom,
//         "fixtures/parsers/t_atom.ron",
//     ),
// }
