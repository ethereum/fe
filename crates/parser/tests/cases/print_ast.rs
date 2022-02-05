use fe_common::db::TestDb;
use fe_common::diagnostics::print_diagnostics;
use fe_common::SourceFileId;
use fe_parser::parse_file;
use fe_test_files::fixture;
use wasm_bindgen_test::wasm_bindgen_test;

fn parse_and_print(path: &str, src: &str) -> String {
    let mut db = TestDb::default();
    let id = SourceFileId::new_local(&mut db, path, src.into());

    let (module, diags) = parse_file(id, src);

    if !diags.is_empty() {
        print_diagnostics(&db, &diags);
        panic!("parse error");
    }
    format!("{}", module)
}

macro_rules! test_print {
    ($name:ident, $path:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let src = fixture($path);
            pretty_assertions::assert_eq!(src, parse_and_print($path, src))
        }
    };
}

test_print! { erc20, "demos/erc20_token.fe" }
test_print! { guest_book, "printing/guest_book_no_comments.fe" }
test_print! { expr_parens, "printing/expr_parens.fe" }
test_print! { defs, "printing/defs.fe" }
