use fe_common::files::FileStore;
use wasm_bindgen_test::wasm_bindgen_test;

macro_rules! test_file {
    ($name:ident) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let path = concat!("crashes/", stringify!($name), ".fe");
            let src = test_files::fixture(path);
            let mut files = FileStore::new();
            let deps = files.add_included_libraries();
            let id = files.add_file(path, src);
            fe_driver::compile_module(&files, id, &deps, true, true).ok();
        }
    };
}

test_file! { agroce531 }
test_file! { agroce550 }
test_file! { revert_const }
