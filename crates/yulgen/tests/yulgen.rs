use fe_analyzer::namespace::types::{AbiDecodeLocation, Base, FeString, FixedSize, Struct, U256};
use fe_yulgen::constructor;
use fe_yulgen::runtime::functions::{abi, structs};
use insta::assert_display_snapshot;
use wasm_bindgen_test::wasm_bindgen_test;

macro_rules! test_yulgen {
    ($name:ident, $func:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            if cfg!(target_arch = "wasm32") {
                fe_common::assert_snapshot_wasm!(
                    concat!("snapshots/yulgen__", stringify!($name), ".snap"),
                    $func.to_string()
                );
            } else {
                assert_display_snapshot!($func);
            }
        }
    };
}

test_yulgen! { constructor_no_init,  constructor::build() }

test_yulgen! { abi_decode_string_mem, abi::decode(FeString { max_size: 100 }, AbiDecodeLocation::Memory) }
test_yulgen! { abi_decode_string_calldata, abi::decode(FeString { max_size: 100 }, AbiDecodeLocation::Calldata) }
test_yulgen! { abi_decode_u256_mem,  abi::decode(U256, AbiDecodeLocation::Memory) }
test_yulgen! { abi_encode_u256_address,  abi::encode(vec![U256, Base::Address]) }

fn struct_bool_bool() -> Struct {
    let mut val = Struct::new("Foo");
    val.add_field("bar", &FixedSize::bool()).unwrap();
    val.add_field("bar2", &FixedSize::bool()).unwrap();
    val
}

test_yulgen! { struct_empty, structs::generate_new_fn(&Struct::new("Foo")) }
test_yulgen! { struct_new_gen, structs::generate_new_fn(&struct_bool_bool()) }
test_yulgen! { struct_getter_gen_bar, structs::generate_get_fn(&struct_bool_bool(), &struct_bool_bool().fields[0].0) }
test_yulgen! { struct_getter_gen_bar2, structs::generate_get_fn(&struct_bool_bool(), &struct_bool_bool().fields[1].0) }
