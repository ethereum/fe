//! Stress tests that test broad behavior

#![cfg(feature = "solc-backend")]
use fe_compiler_test_utils::*;
use insta::assert_snapshot;

pub fn deploy_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    fe_compiler_test_utils::deploy_contract(
        executor,
        &format!("stress/{fixture}"),
        contract_name,
        init_params,
    )
}

#[test]
fn data_copying_stress() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "data_copying_stress.fe", "Foo", &[]);

        harness.test_function(
            &mut executor,
            "set_my_vals",
            &[
                string_token("my string"),
                string_token("my other string"),
                uint_token(26),
                uint_token(42),
            ],
            None,
        );

        harness.test_function(&mut executor, "emit_my_event", &[], None);

        harness.test_function(&mut executor, "set_to_my_other_vals", &[], None);

        harness.test_function(&mut executor, "emit_my_event", &[], None);

        let my_array = uint_array_token(&[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
        let my_mutated_array = uint_array_token(&[1, 2, 3, 5, 5, 6, 7, 8, 9, 10]);

        let my_addrs = address_array_token(&["0", "1", "2"]);
        let my_second_addr = address_token("1");

        harness.test_function(
            &mut executor,
            "mutate_and_return",
            &[my_array.clone()],
            Some(&my_mutated_array),
        );

        harness.test_function(
            &mut executor,
            "multiple_references_shared_memory",
            &[my_array.clone()],
            None,
        );

        harness.test_function(
            &mut executor,
            "clone_and_return",
            &[my_array.clone()],
            Some(&my_array),
        );

        harness.test_function(
            &mut executor,
            "clone_mutate_and_return",
            &[my_array.clone()],
            Some(&my_array),
        );

        harness.test_function(
            &mut executor,
            "assign_my_nums_and_return",
            &[],
            Some(&uint_array_token(&[42, 26, 0, 1, 255])),
        );

        harness.test_function(&mut executor, "set_my_addrs", &[my_addrs], None);
        harness.test_function(
            &mut executor,
            "get_my_second_addr",
            &[],
            Some(&my_second_addr),
        );

        harness.events_emitted(
            executor,
            &[
                ("MyEvent", &[string_token("my string"), uint_token(26)]),
                (
                    "MyEvent",
                    &[string_token("my other string"), uint_token(42)],
                ),
            ],
        );

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn abi_encoding_stress() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "abi_encoding_stress.fe", "Foo", &[]);

        let my_addrs = address_array_token(&["a", "b", "c", "d", "e"]);
        let my_u128 = uint_token(42);
        let my_string = string_token("my string");
        let my_u16s = uint_array_token(&(0..255).collect::<Vec<_>>());
        let my_bool = bool_token(true);
        let my_bytes = bytes_token(&"ten bytes.".repeat(10));

        harness.test_function(&mut executor, "set_my_addrs", &[my_addrs.clone()], None);
        harness.test_function(&mut executor, "get_my_addrs", &[], Some(&my_addrs));

        harness.test_function(&mut executor, "set_my_u128", &[my_u128.clone()], None);
        harness.test_function(&mut executor, "get_my_u128", &[], Some(&my_u128));

        harness.test_function(&mut executor, "set_my_string", &[my_string.clone()], None);
        harness.test_function(&mut executor, "get_my_string", &[], Some(&my_string));

        harness.test_function(&mut executor, "set_my_u16s", &[my_u16s.clone()], None);
        harness.test_function(&mut executor, "get_my_u16s", &[], Some(&my_u16s));

        harness.test_function(&mut executor, "set_my_bool", &[my_bool.clone()], None);
        harness.test_function(&mut executor, "get_my_bool", &[], Some(&my_bool));

        harness.test_function(&mut executor, "set_my_bytes", &[my_bytes.clone()], None);
        harness.test_function(&mut executor, "get_my_bytes", &[], Some(&my_bytes));

        let my_tuple1 = tuple_token(&[
            uint_token(42),
            uint_token(26),
            bool_token(true),
            address_token("000000000000000000000000000000000001e240"),
        ]);
        let my_tuple2 = tuple_token(&[
            uint_token(12341234),
            uint_token(42),
            bool_token(false),
            address_token("00000000000000000000000000000000000270f"),
        ]);
        harness.test_function(&mut executor, "get_my_struct", &[], Some(&my_tuple1));
        harness.test_function(
            &mut executor,
            "mod_my_struct",
            &[my_tuple1],
            Some(&my_tuple2),
        );

        harness.test_function(&mut executor, "emit_my_event", &[], None);

        harness.events_emitted(
            executor,
            &[(
                "MyEvent",
                &[my_addrs, my_u128, my_string, my_u16s, my_bool, my_bytes],
            )],
        );

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn tuple_stress() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "tuple_stress.fe", "Foo", &[]);

        let my_num = uint_token(26);
        let my_bool = bool_token(true);
        let my_address = address_token("42");
        let my_tuple = tuple_token(&[my_num.clone(), my_bool.clone(), my_address.clone()]);
        let my_tuple_encoded = ethabi::Token::Bytes(vec![
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 66,
        ]);

        harness.test_function(
            &mut executor,
            "build_my_tuple",
            &[my_num.clone(), my_bool.clone(), my_address.clone()],
            Some(&my_tuple),
        );

        harness.test_function(
            &mut executor,
            "read_my_tuple_item0",
            &[my_tuple.clone()],
            Some(&my_num),
        );

        harness.test_function(
            &mut executor,
            "read_my_tuple_item1",
            &[my_tuple.clone()],
            Some(&my_bool),
        );

        harness.test_function(
            &mut executor,
            "read_my_tuple_item2",
            &[my_tuple.clone()],
            Some(&my_address),
        );

        let my_sto_u256 = uint_token(42);
        let my_sto_i32 = int_token(-26);
        let my_sto_tuple = tuple_token(&[my_sto_u256.clone(), my_sto_i32.clone()]);

        harness.test_function(
            &mut executor,
            "set_my_sto_tuple",
            &[my_sto_u256, my_sto_i32],
            None,
        );

        harness.test_function(&mut executor, "get_my_sto_tuple", &[], Some(&my_sto_tuple));

        harness.test_function(&mut executor, "emit_my_event", &[my_tuple.clone()], None);

        harness.test_function(&mut executor, "build_tuple_and_emit", &[], None);

        harness.test_function(
            &mut executor,
            "encode_my_tuple",
            &[my_tuple.clone()],
            Some(&my_tuple_encoded),
        );

        harness.events_emitted(
            executor,
            &[
                ("MyEvent", &[my_tuple]),
                (
                    "MyEvent",
                    &[tuple_token(&[
                        uint_token(42),
                        bool_token(false),
                        address_token("1a"),
                    ])],
                ),
            ],
        );

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn external_calls_stress() {
    with_executor(&|mut executor| {
        let foo_harness = deploy_contract(&mut executor, "external_calls.fe", "Foo", &[]);
        let foo_address = ethabi::Token::Address(foo_harness.address);
        let proxy_harness = deploy_contract(
            &mut executor,
            "external_calls.fe",
            "FooProxy",
            &[foo_address],
        );

        let my_tuple = tuple_token(&[uint_token(42), address_token("26")]);
        let my_string = string_token("hello world");
        let my_other_tuple = tuple_token(&[uint_token(99), address_token("ab")]);
        let my_other_string = string_token("foo");

        proxy_harness.test_function(
            &mut executor,
            "call_set_my_string",
            &[my_string.clone()],
            None,
        );
        proxy_harness.test_function(&mut executor, "call_get_my_string", &[], Some(&my_string));

        proxy_harness.test_function(
            &mut executor,
            "call_set_my_tuple",
            &[my_tuple.clone()],
            None,
        );
        proxy_harness.test_function(&mut executor, "call_get_my_tuple", &[], Some(&my_tuple));

        proxy_harness.test_function(
            &mut executor,
            "call_set_my_string_and_tuple",
            &[my_other_string.clone(), my_other_tuple.clone()],
            None,
        );
        proxy_harness.test_function(
            &mut executor,
            "call_get_my_tuple",
            &[],
            Some(&my_other_tuple),
        );
        proxy_harness.test_function(
            &mut executor,
            "call_get_my_string",
            &[],
            Some(&my_other_string),
        );

        proxy_harness.test_function(
            &mut executor,
            "call_get_tuple",
            &[],
            Some(&tuple_token(&[
                uint_token(42),
                uint_token(26),
                bool_token(false),
            ])),
        );
        proxy_harness.test_function(
            &mut executor,
            "call_get_string",
            &[],
            Some(&string_token("hi")),
        );
        proxy_harness.test_function(&mut executor, "call_get_array", &[], None);

        proxy_harness.test_function_reverts(&mut executor, "call_do_revert", &[], &[]);

        proxy_harness.test_function_reverts(&mut executor, "call_do_revert2", &[], &[]);

        proxy_harness.test_function_reverts(
            &mut executor,
            "call_do_revert_with_data",
            &[],
            &encode_revert("SomeError(uint256)", &[uint_token(4711)]),
        );

        proxy_harness.test_function_reverts(
            &mut executor,
            "call_do_revert_with_data2",
            &[],
            &encode_revert("SomeError(uint256)", &[uint_token(4711)]),
        );

        assert_harness_gas_report!(proxy_harness);
    });
}
