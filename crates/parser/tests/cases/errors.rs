use fe_common::db::TestDb;
use fe_common::diagnostics::diagnostics_string;
use fe_common::SourceFileId;
use fe_parser::grammar::{expressions, functions, module};
use fe_parser::Parser;
use insta::assert_snapshot;

pub fn err_string<F, T>(test_name: &str, mut parse_fn: F, src: &str) -> String
where
    F: FnMut(&mut Parser) -> T,
    T: std::fmt::Debug,
{
    let mut db = TestDb::default();
    let id = SourceFileId::new_local(&mut db, test_name, src.into());
    let mut parser = Parser::new(id, src);
    let _ = parse_fn(&mut parser);

    diagnostics_string(&db, &parser.diagnostics)
}

macro_rules! test_parse_err {
    ($name:ident, $parse_fn:expr, $src:expr) => {
        #[cfg(not(target_arch = "wasm32"))]
        #[test]
        fn $name() {
            assert_snapshot!(err_string(stringify!($name), $parse_fn, $src));
        }

        #[cfg(target_arch = "wasm32")]
        #[wasm_bindgen_test::wasm_bindgen_test]
        fn $name() {
            let actual = err_string(stringify!($name), $parse_fn, $src);
            let expected = include_str!(concat!(
                "snapshots/cases__errors__",
                stringify!($name),
                ".snap"
            ))
            .splitn(3, "---\n")
            .last()
            .unwrap();
            pretty_assertions::assert_eq!(actual.trim(), expected.trim());
        }
    };
}

// These tests use the insta crate. insta will automatically generate the
// snapshot file on the first run.

test_parse_err! { contract_invalid_version_requirement, module::parse_module, r#"
pragma 0.o
contract C {}"#
}

test_parse_err! { contract_missing_version_requirement, module::parse_module, r#"
pragma
contract C {}
"#
}

test_parse_err! { contract_bad_name, module::parse_module, "contract 1X {\n x: u8 \n}" }
test_parse_err! { contract_field_after_def, module::parse_module, r#"
contract C {
  fn f() {}
  x: u8
}"#
}

test_parse_err! { type_desc_path_number, module::parse_module, "type Foo = some::mod::Foo::5000" }
test_parse_err! { contract_const_pub, module::parse_module, "contract C {\n const pub x: u8\n}" }
test_parse_err! { contract_const_fn, module::parse_module, "contract C {\n const fn f() {}\n}" }
test_parse_err! { expr_bad_prefix, expressions::parse_expr, "*x + 1" }
test_parse_err! { expr_path_left, expressions::parse_expr, "(1 + 2)::foo::bar" }
test_parse_err! { expr_path_right, expressions::parse_expr, "foo::10::bar" }
test_parse_err! { expr_dotted_number, expressions::parse_expr, "3.14" }
test_parse_err! { expr_call_eq_label, expressions::parse_expr, "foo(bar=1, baz = 2)" }
test_parse_err! { expr_assignment, expressions::parse_expr, "1 + (x = y)" }
test_parse_err! { for_no_in, functions::parse_stmt, "for x {}" }
test_parse_err! { fn_no_args, module::parse_module, "fn f {\n  return 5\n}" }
test_parse_err! { fn_unsafe_pub, module::parse_module, "unsafe pub fn f() {\n  return 5 }" }
test_parse_err! { fn_def_kw, module::parse_module, "contract C {\n pub def f(x: u8){\n  return x \n}\n}" }

test_parse_err! { fn_invalid_bound, module::parse_module, "pub fn f<T:(u8, u8)>() {}" }
test_parse_err! { use_bad_name, module::parse_use, "use x as 123" }
test_parse_err! { module_bad_stmt, module::parse_module, "if x { y }" }
test_parse_err! { module_nonsense, module::parse_module, "))" }
test_parse_err! { struct_bad_field_name, module::parse_module, "struct f {\n pub type }" }
test_parse_err! { stmt_vardecl_attr, functions::parse_stmt, "f.s : u" }
test_parse_err! { stmt_vardecl_tuple, functions::parse_stmt, "(a, x+1) : u256" }
test_parse_err! { stmt_vardecl_tuple_empty, functions::parse_stmt, "(a, ()) : u256" }
test_parse_err! { stmt_vardecl_subscript, functions::parse_stmt, "a[1] : u256" }
test_parse_err! { stmt_vardecl_missing_type_annotation, functions::parse_stmt, "let x = 1" }
test_parse_err! { stmt_vardecl_missing_type_annotation_2, functions::parse_stmt, "let x" }
test_parse_err! { stmt_vardecl_missing_type_annotation_3, functions::parse_stmt, "let x:" }
test_parse_err! { stmt_vardecl_invalid_type_annotation, functions::parse_stmt, "let x: y + z" }
test_parse_err! { stmt_vardecl_invalid_name, functions::parse_stmt, "let x + y: u8" }
test_parse_err! { number_end_with_underscore, functions::parse_stmt, "42_42_"}
test_parse_err! { array_old_syntax, functions::parse_stmt, "let x: u8[10]" }
test_parse_err! { array_old_syntax_invalid, functions::parse_stmt, "let x: u8[10" }
test_parse_err! { self_const, module::parse_module, "const self: u8 = 10" }
test_parse_err! { self_contract, module::parse_module, "contract self {}" }
test_parse_err! { self_struct, module::parse_module, "struct self {}" }
test_parse_err! { self_fn, module::parse_module, "pub fn self() {}" }
test_parse_err! { self_use1, module::parse_module, "use self as bar" }
test_parse_err! { self_use2, module::parse_module, "use bar as self" }
test_parse_err! { stmt_match1, functions::parse_stmt, r#"match my_enum {
    mymod::MyS {.., x: x, y: true}  => {
        return x
    }
}"# }

// assert_snapshot! doesn't like the invalid escape code
#[test]
fn string_invalid_escape() {
    let err = err_string(
        "string_invalid_escape",
        expressions::parse_expr,
        r#""a string \c ""#,
    );
    assert_snapshot!(err);
}
