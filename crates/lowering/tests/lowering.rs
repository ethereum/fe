use fe_analyzer::namespace::items::ModuleId;
use fe_analyzer::Db;
use fe_common::diagnostics::print_diagnostics;
use fe_common::files::{FileStore, SourceFileId};
use fe_parser::ast as fe;
use insta::assert_snapshot;
use wasm_bindgen_test::wasm_bindgen_test;

fn lower(src: &str, id: SourceFileId, files: &FileStore) -> fe::Module {
    let fe_module = parse_file(src, id, files);
    let (db, module_id) = analyze(fe_module, id, files);
    fe_lowering::lower(&db, module_id)
}

fn analyze(module: fe::Module, id: SourceFileId, files: &FileStore) -> (Db, ModuleId) {
    let db = Db::default();
    match fe_analyzer::analyze(&db, module) {
        Ok(id) => (db, id),
        Err(diagnostics) => {
            print_diagnostics(&diagnostics, id, files);
            panic!("analysis failed");
        }
    }
}

fn parse_file(src: &str, id: SourceFileId, files: &FileStore) -> fe::Module {
    match fe_parser::parse_file(src) {
        Ok((module, diags)) if diags.is_empty() => module,
        Ok((_, diags)) | Err(diags) => {
            print_diagnostics(&diags, id, files);
            panic!("failed to parse file");
        }
    }
}

macro_rules! test_file {
    ($name:ident, $path:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let mut files = FileStore::new();

            let src = test_files::fixture($path);
            let src_id = files.add_file(src, $path);
            let lowered_code = format!("{}", lower(src, src_id, &files));

            if cfg!(target_arch = "wasm32") {
                fe_common::assert_snapshot_wasm!(
                    concat!("snapshots/lowering__", stringify!($name), ".snap"),
                    lowered_code
                );
            } else {
                assert_snapshot!(lowered_code);
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
// TODO: the analyzer rejects lowered nested tuples.
// test_file!(array_tuple, "lowering/array_tuple.fe");
