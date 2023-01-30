use fe_common::db::TestDb;
use fe_common::diagnostics::print_diagnostics;
use fe_common::SourceFileId;
use fe_parser::parse_file;
use fe_test_files::fixture;
use insta::assert_snapshot;
use wasm_bindgen_test::wasm_bindgen_test;

fn parse_and_print(path: &str, src: &str) -> String {
    let mut db = TestDb::default();
    let id = SourceFileId::new_local(&mut db, path, src.into());

    let (module, diags) = parse_file(id, src);

    if !diags.is_empty() {
        print_diagnostics(&db, &diags);
        panic!("parse error");
    }
    format!("{module}")
}

macro_rules! test_print {
    ($name:ident, $path:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let src = fixture($path);
            if cfg!(target_arch = "wasm32") {
                fe_common::assert_snapshot_wasm!(
                    concat!("snapshots/cases__print_ast__", stringify!($name), ".snap"),
                    parse_and_print($path, src)
                );
            } else {
                assert_snapshot!(parse_and_print($path, src))
            }

            // These tests used to assert that the source text and the formatted
            // text are equal, which is probably more likely to catch errors,
            // but the diffs from `pretty_assertions` made it hard to debug.
            // Feel free to change it back or find a better diff printer.
        }
    };
}

test_print! { erc20, "demos/erc20_token.fe" }
test_print! { guest_book, "demos/guest_book.fe" }
test_print! { expr_parens, "printing/expr_parens.fe" }
test_print! { defs, "printing/defs.fe" }
