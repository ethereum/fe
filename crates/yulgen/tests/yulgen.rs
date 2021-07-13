use fe_analyzer::context::FunctionAttributes;
use fe_analyzer::namespace::events::EventDef;
use fe_analyzer::namespace::types::{
    AbiDecodeLocation, AbiType, Array, Base, FeString, FixedSize, Integer, Struct, U256,
};
use fe_yulgen::constructor;
use fe_yulgen::names::abi as abi_names;
use fe_yulgen::operations::{abi as abi_operations, data as data_operations};
use fe_yulgen::runtime::abi_dispatcher;
use fe_yulgen::runtime::functions::{
    abi as abi_functions, revert as revert_functions, structs as structs_functions,
};
use insta::assert_display_snapshot;
use wasm_bindgen_test::wasm_bindgen_test;
use yultsur::*;

macro_rules! test_yulgen {
    ($name:ident, $code:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            if cfg!(target_arch = "wasm32") {
                fe_common::assert_snapshot_wasm!(
                    concat!("snapshots/yulgen__", stringify!($name), ".snap"),
                    $code.to_string()
                );
            } else {
                assert_display_snapshot!($code);
            }
        }
    };
}

// constructor
test_yulgen! { constructor_no_init,  constructor::build() }

fn function_attributes() -> Vec<FunctionAttributes> {
    vec![
        FunctionAttributes {
            is_public: true,
            name: "hello_world".to_string(),
            params: vec![],
            return_type: FixedSize::String(FeString { max_size: 42 }),
        },
        FunctionAttributes {
            is_public: true,
            name: "add".to_string(),
            params: vec![
                (
                    "a".to_string(),
                    FixedSize::Base(Base::Numeric(Integer::U256)),
                ),
                (
                    "b".to_string(),
                    FixedSize::Base(Base::Numeric(Integer::U256)),
                ),
            ],
            return_type: FixedSize::Base(Base::Numeric(Integer::U256)),
        },
    ]
}

// ABI dispatcher
test_yulgen! { abi_dispatcher,  abi_dispatcher::dispatcher(&function_attributes()) }

// ABI encoding functions
test_yulgen! {
    abi_encode_u256_address_function,
    abi_functions::encode(vec![U256, Base::Address])
}

// ABI decoding functions
test_yulgen! {
    abi_decode_data_address_bool_mem_function,
    abi_functions::decode_data(&[Base::Bool, Base::Address], AbiDecodeLocation::Memory)
}
test_yulgen! {
    abi_decode_data_u256_bytes_string_bool_address_bytes_calldata_function,
    abi_functions::decode_data(&[
        FixedSize::u256(),
        FixedSize::Array(Array { inner: Base::Numeric(Integer::U8), size: 100 }),
        FixedSize::String(FeString { max_size: 42 }),
        FixedSize::bool(),
        FixedSize::address(),
        FixedSize::Array(Array { inner: Base::Numeric(Integer::U8), size: 100 }),
    ], AbiDecodeLocation::Calldata)
}
test_yulgen! {
    abi_decode_component_uint256_mem_function,
    abi_functions::decode_component_uint(32, AbiDecodeLocation::Memory)
}
test_yulgen! {
    abi_decode_component_int16_calldata_function,
    abi_functions::decode_component_int(2, AbiDecodeLocation::Calldata)
}
test_yulgen! {
    abi_decode_component_bool_calldata_function,
    abi_functions::decode_component_bool(AbiDecodeLocation::Calldata)
}
test_yulgen! {
    abi_decode_component_address_mem_function,
    abi_functions::decode_component_bool(AbiDecodeLocation::Memory)
}
test_yulgen! {
    abi_decode_component_static_array_address_calldata_function,
    abi_functions::decode_component_static_array(&AbiType::Address, 42, AbiDecodeLocation::Calldata)
}
test_yulgen! {
    abi_decode_component_tuple_u256_address_mem_function,
    abi_functions::decode_component_tuple(
        &[AbiType::Uint { size: 32 }, AbiType::Address],
        AbiDecodeLocation::Memory
    )
}
test_yulgen! {
    abi_decode_component_bytes_26_mem_function,
    abi_functions::decode_component_bytes(26, AbiDecodeLocation::Memory)
}
test_yulgen! {
    abi_decode_component_string_26_calldata_function,
    abi_functions::decode_component_bytes(26, AbiDecodeLocation::Calldata)
}

fn struct_bool_bool() -> Struct {
    let mut val = Struct::new("Foo");
    val.add_field("bar", &FixedSize::bool()).unwrap();
    val.add_field("bar2", &FixedSize::bool()).unwrap();
    val
}

// struct functions
test_yulgen! {
    struct_empty_function,
    structs_functions::generate_new_fn(&Struct::new("Foo"))
}
test_yulgen! {
    struct_new_gen_function,
    structs_functions::generate_new_fn(&struct_bool_bool())
}
test_yulgen! {
    struct_getter_gen_bar_function,
    structs_functions::generate_get_fn(&struct_bool_bool(), &struct_bool_bool().fields[0].0)
}
test_yulgen! {
    struct_getter_gen_bar2_function,
    structs_functions::generate_get_fn(&struct_bool_bool(), &struct_bool_bool().fields[1].0)
}

fn event_no_indexed() -> EventDef {
    EventDef::new(
        "MyEvent",
        vec![
            ("my_u256".to_string(), FixedSize::Base(U256)),
            ("my_addr".to_string(), FixedSize::Base(Base::Address)),
        ],
        vec![],
    )
}

fn event_one_indexed() -> EventDef {
    EventDef::new(
        "MyEvent",
        vec![
            ("my_u256".to_string(), FixedSize::Base(U256)),
            ("my_addr".to_string(), FixedSize::Base(Base::Address)),
        ],
        vec![0],
    )
}

// data operations
test_yulgen! {
    emit_event_no_indexed_operation,
    data_operations::emit_event(event_no_indexed(), vec![expression! { 26 }, expression! { 0x42 }])
}
test_yulgen! {
    emit_event_one_indexed_operation,
    data_operations::emit_event(event_one_indexed(), vec![expression! { 26 }, expression! { 0x42 }])
}
test_yulgen! {
    sum_operation,
    data_operations::sum(expressions! { 42 26 99 })
}

// ABI operations
test_yulgen! {
    encode_u256_operation,
    abi_operations::encode(&[U256], vec![expression! { 42 }])
}
test_yulgen! {
    encode_size_u256_operation,
    abi_operations::encoding_size(&[U256], vec![expression! { 42 }])
}
test_yulgen! {
    decode_string_calldata_operation,
    abi_operations::decode_data(
        &[FeString { max_size: 26 }],
        expression! { 42 },
        expression! { 10 },
        AbiDecodeLocation::Calldata
    )
}

// ABI names
test_yulgen! {
    encode_name,
    abi_names::encode(&[
        FixedSize::Base(U256),
        FixedSize::Array(Array {
            inner: Base::Numeric(Integer::U8),
            size: 100
        })
    ])
}
test_yulgen! {
    decode_data_name_u256_calldata_name,
    abi_names::decode_data(&[U256], AbiDecodeLocation::Calldata)
}
test_yulgen! {
    decode_data_name_string_mem_name,
    abi_names::decode_data(&[FeString { max_size: 42 }], AbiDecodeLocation::Memory)
}

// revert functions
test_yulgen! { revert_string_error, revert_functions::generate_revert_fn_for_assert(&[FeString { max_size: 3}.into()]) }
