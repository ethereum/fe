use fe_analyzer::namespace::items::ModuleId;
use fe_lowering::TestDb;
use insta::assert_snapshot;
use wasm_bindgen_test::wasm_bindgen_test;

macro_rules! test_file {
    ($name:ident, $path:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let mut db = TestDb::default();
            let module =
                ModuleId::new_standalone(&mut db, $path.into(), test_files::fixture($path).into());
            let lowered = format!(
                "{}",
                fe_lowering::lower_main_module(&mut db, module).ast(&db)
            );

            if cfg!(target_arch = "wasm32") {
                fe_common::assert_snapshot_wasm!(
                    concat!("snapshots/lowering__", stringify!($name), ".snap"),
                    lowered
                );
            } else {
                assert_snapshot!(lowered);
            }
        }
    };
}

test_file! { aug_assign, "lowering/aug_assign.fe" }
test_file! { base_tuple, "lowering/base_tuple.fe" }
test_file! { list_expressions, "lowering/list_expressions.fe" }
test_file! { return_unit, "lowering/return_unit.fe" }
test_file! { unit_implicit, "lowering/unit_implicit.fe" }
test_file! { init, "lowering/init.fe" }
test_file! { custom_empty_type, "lowering/custom_empty_type.fe" }
test_file! { nested_tuple, "lowering/nested_tuple.fe" }
test_file! { map_tuple, "lowering/map_tuple.fe" }
test_file! { type_alias_tuple, "lowering/type_alias_tuple.fe" }
test_file! { tuple_destruct, "lowering/tuple_destruct.fe" }
test_file! { module_const, "lowering/module_const.fe" }
test_file! { module_fn, "lowering/module_fn.fe" }
test_file! { struct_fn, "lowering/struct_fn.fe" }
test_file! { ternary, "lowering/ternary.fe" }
test_file! { and_or, "lowering/and_or.fe" }
// TODO: the analyzer rejects lowered nested tuples.
// test_file!(array_tuple, "lowering/array_tuple.fe");
