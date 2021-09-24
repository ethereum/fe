use wasm_bindgen_test::wasm_bindgen_test;

macro_rules! test_file {
    ($name:ident) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let path = concat!("crashes/", stringify!($name), ".fe");
            let src = test_files::fixture(path);
            fe_driver::compile(src, true, true).ok();
        }
    };
}

test_file! { agroce531 }
