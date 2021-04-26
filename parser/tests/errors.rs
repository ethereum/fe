use fe_common::diagnostics::diagnostics_string;
use fe_parser::grammar::{contracts, expressions, functions, module, types};

use fe_parser::{ParseResult, Parser};
use insta::assert_snapshot;

pub fn err_string<F, T>(mut parse_fn: F, should_fail: bool, src: &str) -> String
where
    F: FnMut(&mut Parser) -> ParseResult<T>,
    T: std::fmt::Debug,
{
    let mut files = fe_common::files::FileStore::new();
    let id = files.add_file("[test snippet]", src);
    let mut parser = Parser::new(src, id);

    if should_fail != parse_fn(&mut parser).is_err() {
        panic!(
            "expected parsing to {}fail",
            if should_fail { "" } else { "not " }
        );
    }
    diagnostics_string(&parser.diagnostics, &files)
}

macro_rules! test_parse_err {
    ($name:ident, $parse_fn:expr, $should_fail:expr, $string:expr) => {
        #[test]
        fn $name() {
            let err = err_string($parse_fn, $should_fail, $string);
            assert_snapshot!(err);
        }
    };
}

// These tests use the insta crate. insta will automatically generate the
// snapshot file on the first run.

test_parse_err! { contract_bad_name, contracts::parse_contract_def, true, "contract 1X:\n x: u8" }
test_parse_err! { contract_empty_body, module::parse_module, true, "contract X:\n \n \ncontract Y:\n x: u8" }
test_parse_err! { contract_field_after_def, module::parse_module, false, r#"
contract C:
  def f():
    pass
  x: u8
"#
}

test_parse_err! { contract_pub_event, contracts::parse_contract_def, false, "contract C:\n pub event E:\n  x: u8" }
test_parse_err! { contract_const_pub, contracts::parse_contract_def, false, "contract C:\n const pub x: u8" }
test_parse_err! { contract_const_fn, contracts::parse_contract_def, false, "contract C:\n const def f():\n  pass" }
test_parse_err! { emit_no_args, functions::parse_stmt, true, "emit x" }
test_parse_err! { emit_expr, functions::parse_stmt, true, "emit x + 1" }
test_parse_err! { emit_bad_call, functions::parse_stmt, true, "emit MyEvent(1)()" }
test_parse_err! { expr_bad_prefix, expressions::parse_expr, true, "*x + 1" }
test_parse_err! { for_no_in, functions::parse_stmt, true, "for x:\n pass" }
test_parse_err! { fn_no_args, |par| functions::parse_fn_def(par, None), false, "def f:\n  return 5" }
test_parse_err! { if_no_body, functions::parse_stmt, true, "if x:\nelse:\n x" }
test_parse_err! { import_bad_name, module::parse_simple_import, true, "import x as 123" }
test_parse_err! { module_bad_stmt, module::parse_module, true, "if x:\n y" }
test_parse_err! { module_nonsense, module::parse_module, true, "))" }
test_parse_err! { string_invalid_escape, expressions::parse_expr, false, r#""a string \c ""# }
test_parse_err! { struct_bad_field_name, types::parse_struct_def, true, "struct f:\n pub def" }
