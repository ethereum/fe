use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{
    BufferWriter,
    ColorChoice,
};
use fe_parser::ast;
use fe_parser::lexer::TokenKind;
use fe_parser::newparser::grammar::{
    contracts,
    expressions,
    functions,
    module,
    types,
};
use fe_parser::newparser::{
    ParseResult,
    Parser,
};
use serde::Serialize;

#[macro_use]
mod utils;

#[derive(PartialEq)]
enum Repeat {
    No,
    Yes,
}

fn run_parser_test<F, T>(fixture_name: &str, content: &str, mut parse_fn: F, repeat: Repeat)
where
    F: FnMut(&mut Parser) -> ParseResult<T>,
    T: Serialize,
{
    let (input, expected_ser) = utils::parse_fixture(content)
        .expect(&format!("Test example has wrong format {}", fixture_name));

    let mut parser = Parser::new(&input);

    let mut parser_err = false;
    let actual_ser = if repeat == Repeat::Yes {
        let mut results = vec![];
        while !parser.done() {
            if let Ok(res) = parse_fn(&mut parser) {
                results.push(res);
            } else {
                parser_err = true;
                break;
            }
            // consume newline that separates test cases
            if parser.peek() == Some(TokenKind::Newline) {
                parser.next().unwrap();
            }
        }
        utils::to_ron_string_pretty(&results).unwrap()
    } else {
        if let Ok(res) = parse_fn(&mut parser) {
            utils::to_ron_string_pretty(&res).unwrap()
        } else {
            parser_err = true;
            String::new()
        }
    };
    print_diags(fixture_name, &parser, &input);
    assert!(parser.diagnostics.is_empty());
    if parser_err {
        eprintln!("parsing failed, but no diagnostics were generated. this should be fixed.");
        let next = parser.next();
        if next.is_err() {
            eprintln!("parser is at end of file");
        } else {
            parser.error(
                next.unwrap().span,
                "this is the next token at time of parsing failure",
            );
            print_diags(fixture_name, &parser, &input);
        }
        assert!(!parser_err);
    }

    eprintln!("{}", &actual_ser);
    assert_strings_eq!(
        expected_ser,
        actual_ser,
        "\nParsing results did not match for {}",
        fixture_name,
    );
}

fn print_diags(fixture_name: &str, parser: &Parser, input: &str) {
    if parser.diagnostics.is_empty() {
        return;
    }
    let file = SimpleFile::new(fixture_name, input);
    let writer = BufferWriter::stderr(ColorChoice::Auto);
    let mut buffer = writer.buffer();
    let config = term::Config::default();

    for diag in &parser.diagnostics {
        term::emit(&mut buffer, &config, &file, &diag).unwrap();
    }
    // If we use `writer` here, the output won't be captured by rust's test system.
    eprintln!("{}", std::str::from_utf8(buffer.as_slice()).unwrap());
}

/// Include a test example file and parse it.
#[allow(unused_macros)]
macro_rules! test_fn {
    ($name:ident, $parse_fn:expr) => {
        test_fn! { $name, $parse_fn, Repeat::Yes }
    };
    ($name:ident, $parse_fn:expr, $repeat:expr) => {
        #[test]
        fn $name() {
            run_parser_test(
                stringify!($name),
                include_str!(concat!("fixtures/parsers/", stringify!($name), ".ron"),),
                $parse_fn,
                $repeat,
            )
        }
    };
}

test_fn! {arg_list, functions::parse_fn_param_list}
test_fn! {assert_stmt, functions::parse_stmt}
test_fn! {augassign_stmt, functions::parse_stmt}
test_fn! {base_type, types::parse_type_desc}
test_fn! {bitwise_and, expressions::parse_expr}
test_fn! {bitwise_or, expressions::parse_expr}
test_fn! {bitwise_xor, expressions::parse_expr}
test_fn! {break_stmt, functions::parse_stmt}
// test_fn! {comparison, expressions::parse_expr} // TODO: `x in y`
test_fn! {compound_stmt, functions::parse_stmt}
test_fn! {conjunct, expressions::parse_expr}
test_fn! {continue_stmt, functions::parse_stmt}
test_fn! {contract_def, contracts::parse_contract_def}
test_fn! {disjunct, expressions::parse_expr}
test_fn! {emit_stmt, functions::parse_stmt}
test_fn! {event_def, types::parse_event_def}
test_fn! {event_field, types::parse_event_field}
test_fn! {expr, expressions::parse_expr}
test_fn! {exprs, expressions::parse_expr}
test_fn! {factor, expressions::parse_expr}
test_fn! {for_stmt, functions::parse_for_stmt}
test_fn! {func_def, |par| {
    let pub_qual = types::parse_opt_qualifier(par, TokenKind::Pub,
                                           ast::PubQualifier {});
    functions::parse_fn_def(par, pub_qual)
}}

test_fn! {func_stmt, |par| {
    // to match old parser output
    Ok(vec![functions::parse_stmt(par)?])
}}
test_fn! {guest_book, module::parse_module, Repeat::No}
test_fn! {generic_type_desc, types::parse_type_desc}
test_fn! {if_stmt, functions::parse_if_stmt}
test_fn! {list, expressions::parse_expr}
test_fn! {map_type, types::parse_type_desc}
test_fn! {module_stmt, module::parse_module_stmt}
test_fn! {pass_stmt, functions::parse_stmt}
test_fn! {power, expressions::parse_expr}
test_fn! {primary, expressions::parse_expr}
test_fn! {return_stmt, functions::parse_stmt}
test_fn! {revert_stmt, functions::parse_stmt}
test_fn! {shift_expr, expressions::parse_expr}
test_fn! {simple_import, module::parse_simple_import}

// TODO: import a.b (as c)
// test_fn! {simple_import_name, module::parse_simple_import}
test_fn! {small_stmt, functions::parse_stmt}

test_fn! {struct_def, types::parse_struct_def}
test_fn! {sum, expressions::parse_expr}
test_fn! {term, expressions::parse_expr}
test_fn! {while_stmt, functions::parse_while_stmt}
