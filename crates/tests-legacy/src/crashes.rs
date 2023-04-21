use wasm_bindgen_test::wasm_bindgen_test;

macro_rules! test_file {
    ($name:ident) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let mut db = fe_driver::Db::default();
            let path = concat!("crashes/", stringify!($name), ".fe");
            let src = test_files::fixture(path);
            fe_driver::compile_single_file(&mut db, path, src, true, true).ok();
        }
    };
}

test_file! { agroce531 }
test_file! { agroce550 }
test_file! { agroce551 }
test_file! { agroce623 }
test_file! { revert_const }
test_file! { mptr_field_abi }
