use fe_common::diagnostics::diagnostics_string;
use fe_parser::grammar::{contracts, expressions, functions, module, types};
use fe_parser::{ParseResult, Parser};
use insta::assert_snapshot;

pub fn err_string<F, T>(test_name: &str, mut parse_fn: F, should_fail: bool, src: &str) -> String
where
    F: FnMut(&mut Parser) -> ParseResult<T>,
    T: std::fmt::Debug,
{
    let mut files = fe_common::files::FileStore::new();
    let id = files.add_file(test_name, src);
    let mut parser = Parser::new(id, src);

    let parse_failed = parse_fn(&mut parser).is_err();
    let diag = diagnostics_string(&parser.diagnostics, &files);
    if parse_failed != should_fail {
        panic!(
            "expected parsing to {}fail. Diagnostics:\n{}",
            if should_fail { "" } else { "not " },
            diag
        );
    }
    diag
}

macro_rules! test_parse_err {
    ($name:ident, $parse_fn:expr, $should_fail:expr, $src:expr) => {
        #[cfg(not(target_arch = "wasm32"))]
        #[test]
        fn $name() {
            assert_snapshot!(err_string(stringify!($name), $parse_fn, $should_fail, $src));
        }

        #[cfg(target_arch = "wasm32")]
        #[wasm_bindgen_test::wasm_bindgen_test]
        fn $name() {
            let actual = err_string(stringify!($name), $parse_fn, $should_fail, $src);
            let (_, expected) = include_str!(concat!(
                "snapshots/cases__errors__",
                stringify!($name),
                ".snap"
            ))
            .rsplit_once("---\n")
            .unwrap();
            pretty_assertions::assert_eq!(actual.trim(), expected.trim());
        }
    };
}

// These tests use the insta crate. insta will automatically generate the
// snapshot file on the first run.

test_parse_err! { contract_invalid_version_requirement, module::parse_module, true, r#"
pragma 0.o
contract C:
  pass
"#
}

test_parse_err! { contract_missing_version_requirement, module::parse_module, true, r#"
pragma
contract C:
  pass
"#
}

test_parse_err! { contract_bad_name, contracts::parse_contract_def, true, "contract 1X:\n x: u8" }
test_parse_err! { contract_empty_body, module::parse_module, true, "contract X:\n \n \ncontract Y:\n x: u8" }
test_parse_err! { contract_field_after_def, module::parse_module, false, r#"
contract C:
  fn f():
    pass
  x: u8
"#
}

test_parse_err! { type_desc_path_number, module::parse_module, true, "type Foo = some::mod::Foo::5000" }
test_parse_err! { contract_pub_event, contracts::parse_contract_def, false, "contract C:\n pub event E:\n  x: u8" }
test_parse_err! { contract_const_pub, contracts::parse_contract_def, false, "contract C:\n const pub x: u8" }
test_parse_err! { contract_const_fn, contracts::parse_contract_def, false, "contract C:\n const fn f():\n  pass" }
test_parse_err! { emit_no_args, functions::parse_stmt, true, "emit x" }
test_parse_err! { emit_expr, functions::parse_stmt, true, "emit x + 1" }
test_parse_err! { emit_bad_call, functions::parse_stmt, true, "emit MyEvent(1)()" }
test_parse_err! { expr_bad_prefix, expressions::parse_expr, true, "*x + 1" }
test_parse_err! { expr_path_left, expressions::parse_expr, true, "(1 + 2)::foo::bar" }
test_parse_err! { expr_path_right, expressions::parse_expr, true, "foo::10::bar" }
test_parse_err! { expr_dotted_number, expressions::parse_expr, true, "3.14" }
test_parse_err! { for_no_in, functions::parse_stmt, true, "for x:\n pass" }
test_parse_err! { fn_no_args, module::parse_module, false, "fn f:\n  return 5" }
test_parse_err! { fn_unsafe_pub, module::parse_module, false, "unsafe pub fn f():\n  return 5" }
test_parse_err! { fn_def_kw, contracts::parse_contract_def, true, "contract C:\n pub def f(x: u8):\n  return x" }
test_parse_err! { if_no_body, functions::parse_stmt, true, "if x:\nelse:\n x" }
test_parse_err! { use_bad_name, module::parse_use, true, "use x as 123" }
test_parse_err! { module_bad_stmt, module::parse_module, true, "if x:\n y" }
test_parse_err! { module_nonsense, module::parse_module, true, "))" }
test_parse_err! { struct_bad_field_name, types::parse_struct_def, true, "struct f:\n pub event" }
test_parse_err! { stmt_vardecl_attr, functions::parse_stmt, true, "f.s : u" }
test_parse_err! { stmt_vardecl_tuple, functions::parse_stmt, true, "(a, x+1) : u256" }
test_parse_err! { stmt_vardecl_tuple_empty, functions::parse_stmt, true, "(a, ()) : u256" }
test_parse_err! { stmt_vardecl_subscript, functions::parse_stmt, true, "a[1] : u256" }
test_parse_err! { stmt_vardecl_missing_type_annotation, functions::parse_stmt, true, "let x = 1" }
test_parse_err! { stmt_vardecl_missing_type_annotation_2, functions::parse_stmt, true, "let x" }
test_parse_err! { stmt_vardecl_missing_type_annotation_3, functions::parse_stmt, true, "let x:" }
test_parse_err! { stmt_vardecl_invalid_type_annotation, functions::parse_stmt, true, "let x: y + z" }
test_parse_err! { stmt_vardecl_invalid_name, functions::parse_stmt, true, "let x + y: u8" }
test_parse_err! { array_old_syntax, functions::parse_stmt, false, "let x: u8[10]" }
test_parse_err! { array_old_syntax_invalid, functions::parse_stmt, true, "let x: u8[10" }

test_parse_err! { self_const, module::parse_module, true, "const self: u8 = 10" }
test_parse_err! { self_contract, module::parse_module, true, "contract self:\n pass" }
test_parse_err! { self_struct, module::parse_module, true, "struct self:\n pass" }
test_parse_err! { self_fn, module::parse_module, true, "pub fn self():\n pass" }
test_parse_err! { self_use1, module::parse_module, true, "use self as bar" }
test_parse_err! { self_use2, module::parse_module, true, "use bar as self" }

// assert_snapshot! doesn't like the invalid escape code
#[test]
fn string_invalid_escape() {
    let err = err_string(
        "string_invalid_escape",
        expressions::parse_expr,
        false,
        r#""a string \c ""#,
    );
    assert_snapshot!(err);
}
