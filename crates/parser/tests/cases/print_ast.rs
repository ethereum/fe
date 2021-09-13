use fe_parser::parse_file;
use fe_test_files::fixture;
use wasm_bindgen_test::wasm_bindgen_test;

fn parse_and_print(src: &str) -> String {
    let (module, _) = parse_file(src).expect("failed to parse source file");
    format!("{}", module)
}

macro_rules! test_print {
    ($name:ident, $path:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let src = fixture($path);
            pretty_assertions::assert_eq!(src, parse_and_print(src))
        }
    };
}

test_print! { erc20, "demos/erc20_token.fe" }
test_print! { guest_book, "printing/guest_book_no_comments.fe" }
test_print! { expr_parens, "printing/expr_parens.fe" }
test_print! { defs, "printing/defs.fe" }
