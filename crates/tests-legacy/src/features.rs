//! Simple contract tests that narrowly test a given feature

#![cfg(feature = "solc-backend")]
use evm::{Capture, ExitReason};
use evm_runtime::Handler;
use insta::assert_snapshot;
use primitive_types::{H160, U256};
use rstest::rstest;
use std::collections::BTreeMap;

use fe_common::utils::keccak;
use fe_compiler_test_utils::*;
use fe_compiler_test_utils::{self as test_utils};

const SOME_ADDRESS: &str = "2012301230123012301230123012301230123002";

pub fn deploy_contract(
    executor: &mut Executor,
    fixture: &str,
    contract_name: &str,
    init_params: &[ethabi::Token],
) -> ContractHarness {
    test_utils::deploy_contract(
        executor,
        &format!("features/{fixture}"),
        contract_name,
        init_params,
    )
}

pub fn load_contract(address: H160, fixture: &str, contract_name: &str) -> ContractHarness {
    test_utils::load_contract(address, &format!("features/{fixture}"), contract_name)
}

#[test]
fn test_to_2s_complement() {
    let minus_three = U256::from_dec_str(
        "115792089237316195423570985008687907853269984665640564039457584007913129639933",
    )
    .unwrap();
    assert_eq!(minus_three, to_2s_complement(-3));
    assert_eq!(U256::from(3), to_2s_complement(3));
}

#[test]
fn evm_sanity() {
    with_executor(&|mut executor| {
        let address = H160::zero();
        let amount = U256::from(1000);

        executor.state_mut().deposit(address, amount);
        assert_eq!(executor.balance(address), amount);
    })
}

#[test]
fn test_send_value() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "send_value.fe", "Foo", &[]);
        let contract_address = harness.address;
        let bob = "2000000000000000000000000000000000000002";
        let ten = U256::from(10);
        let zero = U256::from(0);

        // The contract has a zero balance, hence send_value fails with Error(0x100)
        assert_eq!(executor.balance(contract_address), zero);
        validate_revert(
            harness.capture_call(
                &mut executor,
                "send_them_wei",
                &[address_token(bob), uint_token(1)],
            ),
            &encode_revert("Error(uint256)", &[uint_token(0x100)]),
        );

        // Let's give the contract 10 wei and send bob 9 wei
        executor.state_mut().deposit(contract_address, ten);
        assert_eq!(executor.balance(contract_address), ten);
        assert_eq!(executor.balance(address(bob)), zero);

        harness.test_function(
            &mut executor,
            "send_them_wei",
            &[address_token(bob), uint_token(1)],
            None,
        );
        assert_eq!(executor.balance(contract_address), U256::from(9));
        assert_eq!(executor.balance(address(bob)), U256::from(1));

        // Let's deploy a contract that reverts when it receives ether
        let fail_contract = deploy_solidity_contract(
            &mut executor,
            "solidity/always_revert.sol",
            "Foo",
            &[],
            false,
        );
        executor.state_mut().deposit(fail_contract.address, ten);
        assert_eq!(executor.balance(fail_contract.address), ten);

        // Sending Ether to this contract reverts with Error(0x101)
        validate_revert(
            harness.capture_call(
                &mut executor,
                "send_them_wei",
                &[ethabi::Token::Address(fail_contract.address), uint_token(1)],
            ),
            &encode_revert("Error(uint256)", &[uint_token(0x101)]),
        );

        assert_harness_gas_report!(harness);
    })
}

#[test]
fn test_revert() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "revert.fe", "Foo", &[]);

        validate_revert(harness.capture_call(&mut executor, "bar", &[]), &[]);

        validate_revert(
            harness.capture_call(&mut executor, "revert_custom_error", &[]),
            &encode_revert("Error(uint256)", &[uint_token(0x100)]),
        );

        validate_revert(
            harness.capture_call(&mut executor, "revert_other_error", &[]),
            &encode_revert(
                "OtherError(uint256,bool)",
                &[uint_token(1), bool_token(true)],
            ),
        );

        validate_revert(
            harness.capture_call(&mut executor, "revert_other_error_from_sto", &[]),
            &encode_revert(
                "OtherError(uint256,bool)",
                &[uint_token(1), bool_token(true)],
            ),
        );
    })
}

#[test]
fn test_balances() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "balances.fe", "Foo", &[]);
        let bob = address("2000000000000000000000000000000000000002");
        let bob_token = ethabi::Token::Address(bob);

        harness.test_function(&mut executor, "my_balance", &[], Some(&uint_token(0)));

        harness.test_function(
            &mut executor,
            "other_balance",
            &[ethabi::Token::Address(harness.address)],
            Some(&uint_token(0)),
        );

        harness.test_function(
            &mut executor,
            "other_balance",
            &[bob_token.clone()],
            Some(&uint_token(0)),
        );

        executor.state_mut().deposit(harness.address, U256::from(5));
        executor.state_mut().deposit(bob, U256::from(10));

        assert_eq!(executor.balance(harness.address), U256::from(5));
        assert_eq!(executor.balance(bob), U256::from(10));

        harness.test_function(&mut executor, "my_balance", &[], Some(&uint_token(5)));

        harness.test_function(
            &mut executor,
            "other_balance",
            &[ethabi::Token::Address(harness.address)],
            Some(&uint_token(5)),
        );

        harness.test_function(
            &mut executor,
            "other_balance",
            &[bob_token],
            Some(&uint_token(10)),
        );

        assert_harness_gas_report!(harness);
    })
}

#[test]
fn test_assert() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "assert.fe", "Foo", &[]);

        validate_revert(
            harness.capture_call(&mut executor, "assert_sto_bool", &[]),
            &encoded_panic_assert(),
        );

        validate_revert(
            harness.capture_call(&mut executor, "assert_sto_string_msg", &[]),
            &encode_error_reason("hello"),
        );

        validate_revert(
            harness.capture_call(&mut executor, "bar", &[uint_token(4)]),
            &encoded_panic_assert(),
        );

        assert!(matches!(
            harness.capture_call(&mut executor, "bar", &[uint_token(42)]),
            evm::Capture::Exit((evm::ExitReason::Succeed(_), _))
        ));

        validate_revert(
            harness.capture_call(&mut executor, "revert_with_static_string", &[uint_token(4)]),
            &encode_error_reason("Must be greater than five"),
        );

        let reason = "A very looooooooooooooong reason that consumes multiple words";

        validate_revert(
            harness.capture_call(
                &mut executor,
                "revert_with",
                &[uint_token(4), string_token(reason)],
            ),
            &encode_error_reason(reason),
        );
    })
}

macro_rules! test_method_return {
    ($name:ident, $path:expr, $input:expr, $expected:expr) => {
        #[test]
        fn $name() {
            with_executor(&|mut executor| {
                let harness = deploy_contract(&mut executor, $path, "Foo", &[]);
                harness.test_function(&mut executor, "bar", $input, Some(&$expected));
                assert_harness_gas_report!(harness);
            })
        }
    };
}

test_method_return! { for_loop_with_static_array, "for_loop_with_static_array.fe", &[], uint_token(30) }
test_method_return! { for_loop_with_static_array_from_sto, "for_loop_with_static_array_from_sto.fe", &[], uint_token(6) }
test_method_return! { for_loop_with_break, "for_loop_with_break.fe", &[], uint_token(15) }
test_method_return! { for_loop_with_continue, "for_loop_with_continue.fe", &[], uint_token(17) }
test_method_return! { while_loop_with_continue, "while_loop_with_continue.fe", &[], uint_token(1) }
test_method_return! { while_loop, "while_loop.fe", &[], uint_token(3) }
test_method_return! { while_loop_test_from_sto, "while_loop_test_from_sto.fe", &[], uint_token(42) }
test_method_return! { while_loop_with_break, "while_loop_with_break.fe", &[], uint_token(1) }
test_method_return! { while_loop_with_break_2, "while_loop_with_break_2.fe", &[], uint_token(1) }
test_method_return! { if_statement_a, "if_statement.fe", &[uint_token(6)], uint_token(1) }
test_method_return! { if_statement_b, "if_statement.fe", &[uint_token(4)], uint_token(0) }
test_method_return! { if_statement_test_from_sto, "if_statement_test_from_sto.fe", &[], uint_token(42) }
test_method_return! { if_statement_2, "if_statement_2.fe", &[uint_token(6)], uint_token(1) }
test_method_return! { if_statement_with_block_declaration, "if_statement_with_block_declaration.fe", &[], uint_token(1) }
test_method_return! { ternary_expression_a, "ternary_expression.fe", &[uint_token(6)], uint_token(1) }
test_method_return! { ternary_expression_b, "ternary_expression.fe", &[uint_token(4)], uint_token(0) }
test_method_return! { call_statement_without_args, "call_statement_without_args.fe", &[], uint_token(100) }
test_method_return! { call_statement_with_args, "call_statement_with_args.fe", &[], uint_token(100) }
test_method_return! { call_statement_with_args_2, "call_statement_with_args_2.fe", &[], uint_token(100) }
test_method_return! { return_bool_true, "return_bool_true.fe", &[], bool_token(true) }
test_method_return! { return_bool_false, "return_bool_false.fe", &[], bool_token(false) }
test_method_return! { return_bool_inverted_a, "return_bool_inverted.fe", &[bool_token(true)], bool_token(false) }
test_method_return! { return_bool_inverted_b, "return_bool_inverted.fe", &[bool_token(false)], bool_token(true) }
test_method_return! { return_u256_from_called_fn_with_args, "return_u256_from_called_fn_with_args.fe", &[], uint_token(200) }
test_method_return! { return_u256_from_called_fn, "return_u256_from_called_fn.fe", &[], uint_token(42) }
test_method_return! { return_u256, "return_u256.fe", &[], uint_token(42) }
test_method_return! { return_i256, "return_i256.fe", &[], int_token(-3) }
test_method_return! { return_from_storage_i8, "return_from_storage_i8.fe", &[int_token(-3)], int_token(-6) }
test_method_return! { return_from_storage_array_i8, "return_from_storage_array_i8.fe", &[int_token(-10)], int_array_token(&[-10]) }
test_method_return! { return_from_memory_i8, "return_from_memory_i8.fe", &[int_token(-3)], int_token(-6) }
test_method_return! { return_identity_u256, "return_identity_u256.fe", &[uint_token(42)], uint_token(42) }
test_method_return! { return_identity_u128, "return_identity_u128.fe", &[uint_token(42)], uint_token(42) }
test_method_return! { return_identity_u64, "return_identity_u64.fe", &[uint_token(42)], uint_token(42) }
test_method_return! { return_identity_u32, "return_identity_u32.fe", &[uint_token(42)], uint_token(42) }
test_method_return! { return_identity_u16, "return_identity_u16.fe", &[uint_token(42)], uint_token(42) }
test_method_return! { return_identity_u8, "return_identity_u8.fe", &[uint_token(42)], uint_token(42) }
test_method_return! { return_u128_cast, "return_u128_cast.fe", &[], uint_token(42) }
test_method_return! { return_i128_cast, "return_i128_cast.fe", &[], int_token(-3) }
test_method_return! { return_cast_u8_to_signed_a, "return_cast_u8_to_signed.fe", &[uint_token(253)], int_token(-3) }
test_method_return! { return_cast_u8_to_signed_b, "return_cast_u8_to_signed.fe", &[uint_token(127)], int_token(127) }
test_method_return! { return_cast_u8_to_signed_c, "return_cast_u8_to_signed.fe", &[uint_token(128)], int_token(-128) }
test_method_return! { return_cast_u8_to_signed_d, "return_cast_u8_to_signed.fe", &[uint_token(129)], int_token(-127) }
test_method_return! { return_cast_u8_to_signed_e, "return_cast_u8_to_signed.fe", &[uint_token(0)], int_token(0) }
test_method_return! { return_cast_u8_to_signed_f, "return_cast_u8_to_signed.fe", &[uint_token(1)], int_token(1) }
test_method_return! { return_cast_i8_to_unsigned_a, "return_cast_i8_to_unsigned.fe", &[int_token(-3)], uint_token(253) }
test_method_return! { return_cast_i8_to_unsigned_b, "return_cast_i8_to_unsigned.fe", &[int_token(-128)], uint_token(128) }
test_method_return! { return_cast_i8_to_unsigned_c, "return_cast_i8_to_unsigned.fe", &[int_token(-127)], uint_token(129) }
test_method_return! { return_cast_i8_to_unsigned_d, "return_cast_i8_to_unsigned.fe", &[int_token(127)], uint_token(127) }
test_method_return! { return_cast_i8_to_unsigned_e, "return_cast_i8_to_unsigned.fe", &[int_token(0)], uint_token(0) }
test_method_return! { return_cast_i8_to_unsigned_f, "return_cast_i8_to_unsigned.fe", &[int_token(1)], uint_token(1) }
test_method_return! { return_msg_sig, "return_msg_sig.fe", &[], uint_token(4273672062) }
test_method_return! { return_sum_list_expression_1, "return_sum_list_expression_1.fe", &[], uint_token(210) }
test_method_return! { return_sum_list_expression_2, "return_sum_list_expression_2.fe", &[], uint_token(210) }
test_method_return! { pure_fn, "pure_fn.fe", &[uint_token(42), uint_token(26)], uint_token(68) }
test_method_return! { pure_fn_internal_call, "pure_fn_internal_call.fe", &[uint_token(42), uint_token(26)], uint_token(68) }
test_method_return! { pure_fn_standalone, "pure_fn_standalone.fe", &[uint_token(5)], uint_token(210) }
test_method_return! { value_semantics, "value_semantics.fe", &[], bool_token(true) }
// radix
test_method_return! { radix_hex, "radix_hex.fe", &[], uint_token(0xfe) }
test_method_return! { radix_octal, "radix_octal.fe", &[], uint_token(0o70) }
test_method_return! { radix_binary, "radix_binary.fe", &[], uint_token(0b10) }
test_method_return! { map_tuple, "map_tuple.fe", &[uint_token(1234)], uint_token(1234) }
test_method_return! { int_literal_coercion, "int_literal_coercion.fe", &[], uint_token(300) }
test_method_return! { struct_fns, "struct_fns.fe", &[uint_token(10), uint_token(20)], uint_token(100) }
test_method_return! { cast_address_to_u256, "cast_address_to_u256.fe", &[address_token(SOME_ADDRESS)], address_token(SOME_ADDRESS) }
test_method_return! { for_loop_with_complex_elem_array, "for_loop_with_complex_elem_array.fe", &[], int_token(222) }

#[test]
fn return_array() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "return_array.fe", "Foo", &[]);

        harness.test_function(
            &mut executor,
            "bar",
            &[uint_token(42)],
            Some(&uint_array_token(&[0, 0, 0, 42, 0])),
        );

        assert_harness_gas_report!(harness);
    })
}

#[test]
fn numeric_casts() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "numeric_casts.fe", "Foo", &[]);

        harness.test_function(&mut executor, "bar", &[], None);
        assert_harness_gas_report!(harness);
    })
}

#[test]
fn multi_param() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "multi_param.fe", "Foo", &[]);

        harness.test_function(
            &mut executor,
            "bar",
            &[uint_token(4), uint_token(42), uint_token(420)],
            Some(&uint_array_token(&[4, 42, 420])),
        );
        assert_harness_gas_report!(harness);
    })
}

#[rstest(
    fixture_file,
    case("u256_u256_map.fe"),
    case("u128_u128_map.fe"),
    case("u64_u64_map.fe"),
    case("u32_u32_map.fe"),
    case("u16_u16_map.fe"),
    case("u8_u8_map.fe")
)]
fn test_map(fixture_file: &str) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, fixture_file, "Foo", &[]);

        harness.test_function(
            &mut executor,
            "write_bar",
            &[uint_token(4), uint_token(42)],
            None,
        );

        harness.test_function(
            &mut executor,
            "write_bar",
            &[uint_token(26), uint_token(12)],
            None,
        );

        harness.test_function(
            &mut executor,
            "read_bar",
            &[uint_token(4)],
            Some(&uint_token(42)),
        );

        harness.test_function(
            &mut executor,
            "read_bar",
            &[uint_token(26)],
            Some(&uint_token(12)),
        );
        assert_harness_gas_report!(harness, fixture_file);
    })
}

#[test]
fn return_builtin_attributes() {
    let gas_price = 123;
    let origin = address_token("0000000000000000000000000000000000000001");
    let chain_id = 42;
    let block_number = 5;
    let block_coinbase = address_token("0000000000000000000000000000000000000002");
    let block_timestamp = 1234567890;
    let block_difficulty = 12345;
    let basefee = 1;

    let vicinity = evm::backend::MemoryVicinity {
        gas_price: U256::from(gas_price),
        origin: origin.clone().into_address().unwrap(),
        chain_id: U256::from(chain_id),
        block_hashes: Vec::new(),
        block_number: U256::from(block_number),
        block_coinbase: block_coinbase.clone().into_address().unwrap(),
        block_timestamp: U256::from(block_timestamp),
        block_difficulty: U256::from(block_difficulty),
        block_gas_limit: primitive_types::U256::MAX,
        block_base_fee_per_gas: U256::from(basefee),
    };

    let backend = evm::backend::MemoryBackend::new(&vicinity, BTreeMap::new());

    with_executor_backend(backend, &|mut executor| {
        let mut harness =
            deploy_contract(&mut executor, "return_builtin_attributes.fe", "Foo", &[]);
        let sender = address_token("1234000000000000000000000000000000005678");
        harness.caller = sender.clone().into_address().unwrap();
        let value = 55555;
        harness.value = U256::from(value);
        harness.test_function(&mut executor, "base_fee", &[], Some(&uint_token(basefee)));
        harness.test_function(&mut executor, "coinbase", &[], Some(&block_coinbase));
        harness.test_function(
            &mut executor,
            "prevrandao",
            &[],
            Some(&uint_token(block_difficulty)),
        );
        harness.test_function(
            &mut executor,
            "number",
            &[],
            Some(&uint_token(block_number)),
        );
        harness.test_function(
            &mut executor,
            "timestamp",
            &[],
            Some(&uint_token(block_timestamp)),
        );
        harness.test_function(&mut executor, "chainid", &[], Some(&uint_token(chain_id)));
        harness.test_function(&mut executor, "sender", &[], Some(&sender));
        harness.test_function(&mut executor, "value", &[], Some(&uint_token(value)));
        harness.test_function(&mut executor, "origin", &[], Some(&origin));
        harness.test_function(
            &mut executor,
            "gas_price",
            &[],
            Some(&uint_token(gas_price)),
        );
        assert_harness_gas_report!(harness);
    })
}

#[test]
fn nested_map() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "nested_map.fe", "Foo", &[]);

        let address1 = address_token("1000000000000000000000000000000000000001");
        let address2 = address_token("2000000000000000000000000000000000000002");
        let address3 = address_token("3000000000000000000000000000000000000003");

        // write bar (address -> address -> u256)
        harness.test_function(
            &mut executor,
            "write_bar",
            &[address1.clone(), address2.clone(), uint_token(12)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_bar",
            &[address1.clone(), address3.clone(), uint_token(13)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_bar",
            &[address2.clone(), address1.clone(), uint_token(21)],
            None,
        );

        // write baz (address -> u256 -> bool)
        harness.test_function(
            &mut executor,
            "write_baz",
            &[address1.clone(), uint_token(26), bool_token(true)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_baz",
            &[address2.clone(), uint_token(42), bool_token(true)],
            None,
        );
        harness.test_function(
            &mut executor,
            "write_baz",
            &[address2.clone(), uint_token(100), bool_token(false)],
            None,
        );

        // read bar
        harness.test_function(
            &mut executor,
            "read_bar",
            &[address1.clone(), address2.clone()],
            Some(&uint_token(12)),
        );
        harness.test_function(
            &mut executor,
            "read_bar",
            &[address1.clone(), address3],
            Some(&uint_token(13)),
        );
        harness.test_function(
            &mut executor,
            "read_bar",
            &[address2.clone(), address1.clone()],
            Some(&uint_token(21)),
        );

        // read baz
        harness.test_function(
            &mut executor,
            "read_baz",
            &[address1, uint_token(26)],
            Some(&bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "read_baz",
            &[address2.clone(), uint_token(42)],
            Some(&bool_token(true)),
        );
        harness.test_function(
            &mut executor,
            "read_baz",
            &[address2, uint_token(100)],
            Some(&bool_token(false)),
        );
        assert_harness_gas_report!(harness);
    })
}

#[test]
fn events() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "events.fe", "Foo", &[]);

        let addr1 = address_token("1234000000000000000000000000000000005678");
        let addr2 = address_token("9123000000000000000000000000000000004567");
        let addr_array = ethabi::Token::FixedArray(vec![addr1.clone(), addr2.clone()]);
        let bytes = bytes_token(&"ten bytes.".repeat(10));
        let nested_array_elem1 = ethabi::Token::FixedArray(vec![
            tuple_token(&[uint_token(0), uint_token(1)]),
            tuple_token(&[uint_token(2), uint_token(3)]),
        ]);
        let nested_array_elem2 = ethabi::Token::FixedArray(vec![
            tuple_token(&[uint_token(4), uint_token(5)]),
            tuple_token(&[uint_token(6), uint_token(7)]),
        ]);
        let nested_array = ethabi::Token::FixedArray(vec![nested_array_elem1, nested_array_elem2]);

        harness.test_function(&mut executor, "emit_nums", &[], None);
        harness.test_function(&mut executor, "emit_bases", &[addr1.clone()], None);
        harness.test_function(
            &mut executor,
            "emit_mix",
            &[addr1.clone(), bytes.clone()],
            None,
        );
        harness.test_function(
            &mut executor,
            "emit_addresses",
            &[addr1.clone(), addr2],
            None,
        );
        harness.test_function(&mut executor, "emit_nested_array", &[], None);

        harness.events_emitted(
            executor,
            &[
                ("Nums", &[uint_token(26), uint_token(42)]),
                ("Bases", &[uint_token(26), addr1.clone()]),
                ("Mix", &[uint_token(26), addr1, uint_token(42), bytes]),
                ("Addresses", &[addr_array]),
                ("NestedArray", &[nested_array]),
            ],
        );
        assert_harness_gas_report!(harness);
    })
}

#[test]
fn enum_match() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(
            &mut executor,
            "enum_match.fe",
            "Foo",
            &[uint_token(26), uint_token(42)],
        );

        harness.test_function(
            &mut executor,
            "simple_match",
            &[uint_token(1), uint_token(2)],
            Some(&uint_token(3)),
        );

        harness.test_function(
            &mut executor,
            "nested_match",
            &[uint_token(1), uint_token(2)],
            Some(&uint_token(3)),
        );

        harness.test_function(&mut executor, "nested_match2", &[], Some(&uint_token(3)));

        harness.test_function(&mut executor, "tuple_match", &[], Some(&uint_token(3)));

        harness.test_function(
            &mut executor,
            "boolean_literal_match",
            &[bool_token(true), bool_token(true)],
            Some(&uint_token(2)),
        );
        harness.test_function(
            &mut executor,
            "boolean_literal_match",
            &[bool_token(true), bool_token(false)],
            Some(&uint_token(1)),
        );
        harness.test_function(
            &mut executor,
            "boolean_literal_match",
            &[bool_token(false), bool_token(true)],
            Some(&uint_token(1)),
        );
        harness.test_function(
            &mut executor,
            "boolean_literal_match",
            &[bool_token(false), bool_token(false)],
            Some(&uint_token(0)),
        );

        harness.test_function(&mut executor, "wild_card", &[], Some(&uint_token(0)));
        harness.test_function(&mut executor, "match_in_if", &[], Some(&uint_token(3)));
        harness.test_function(&mut executor, "match_in_loop", &[], Some(&uint_token(15)));
        harness.test_function(
            &mut executor,
            "match_in_loop_simple",
            &[],
            Some(&uint_token(1)),
        );

        harness.test_function(
            &mut executor,
            "rest_pattern_head",
            &[uint_token(1), uint_token(2)],
            Some(&uint_token(3)),
        );
        harness.test_function(
            &mut executor,
            "rest_pattern_tail",
            &[uint_token(1), uint_token(2)],
            Some(&uint_token(3)),
        );
        harness.test_function(
            &mut executor,
            "rest_pattern_middle",
            &[uint_token(1), uint_token(2)],
            Some(&uint_token(6)),
        );

        harness.test_function(
            &mut executor,
            "simple_struct",
            &[int_token(1), int_token(2), bool_token(true)],
            Some(&int_token(3)),
        );
        harness.test_function(
            &mut executor,
            "simple_struct",
            &[int_token(1), int_token(2), bool_token(false)],
            Some(&int_token(-1)),
        );

        harness.test_function(&mut executor, "nested_struct", &[], Some(&uint_token(10)));

        harness.test_function(
            &mut executor,
            "enum_storage",
            &[uint_token(1), uint_token(2), bool_token(true)],
            Some(&uint_token(3)),
        );
        // harness.test_function(
        //     &mut executor,
        //     "enum_storage",
        //     &[uint_token(1), uint_token(2), bool_token(false)],
        //     Some(&uint_token(100)),
        // );

        assert_harness_gas_report!(harness);
    })
}

#[test]
fn constructor() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(
            &mut executor,
            "constructor.fe",
            "Foo",
            &[uint_token(26), uint_token(42)],
        );

        harness.test_function(&mut executor, "read_bar", &[], Some(&uint_token(68)));
        assert_harness_gas_report!(harness);
    })
}

#[test]
fn strings() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(
            &mut executor,
            "strings.fe",
            "Foo",
            &[
                string_token("string 1"),
                address_token("1000000000000000000000000000000000000001"),
                string_token("string 2"),
                uint_token(42),
                string_token("string 3"),
            ],
        );

        harness.test_function(
            &mut executor,
            "bar",
            &[string_token("string 4"), string_token("string 5")],
            Some(&string_token("string 5")),
        );

        harness.test_function(
            &mut executor,
            "return_static_string",
            &[],
            Some(&string_token("The quick brown fox jumps over the lazy dog")),
        );

        harness.test_function(
            &mut executor,
            "return_casted_static_string",
            &[],
            Some(&string_token("foo")),
        );

        harness.test_function(
            &mut executor,
            "return_special_chars",
            &[],
            Some(&string_token(
                "\n\"'\r\t
        foo\\",
            )),
        );

        harness.events_emitted(
            executor,
            &[(
                "MyEvent",
                &[
                    string_token("string 2"),
                    uint_token(42),
                    string_token("string 1"),
                    string_token("string 3"),
                    address_token("1000000000000000000000000000000000000001"),
                    string_token("static string"),
                    string_token("foo"),
                ],
            )],
        );
        assert_harness_gas_report!(harness);
    });
}

#[test]
fn test_numeric_sizes() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "numeric_sizes.fe", "Foo", &[]);
        harness.test_function(&mut executor, "assert_min_max", &[], None);
        for config in NumericAbiTokenBounds::get_all().iter() {
            harness.test_function(
                &mut executor,
                &format!("get_u{}_min", config.size),
                &[],
                Some(&config.u_min.clone()),
            );
            harness.test_function(
                &mut executor,
                &format!("get_u{}_const_min", config.size),
                &[],
                Some(&config.u_min.clone()),
            );
            harness.test_function(
                &mut executor,
                &format!("get_u{}_max", config.size),
                &[],
                Some(&config.u_max.clone()),
            );
            harness.test_function(
                &mut executor,
                &format!("get_u{}_const_max", config.size),
                &[],
                Some(&config.u_max.clone()),
            );
            harness.test_function(
                &mut executor,
                &format!("get_i{}_min", config.size),
                &[],
                Some(&config.i_min.clone()),
            );

            harness.test_function(
                &mut executor,
                &format!("get_i{}_const_min", config.size),
                &[],
                Some(&config.i_min.clone()),
            );
            harness.test_function(
                &mut executor,
                &format!("get_i{}_max", config.size),
                &[],
                Some(&config.i_max.clone()),
            );

            harness.test_function(
                &mut executor,
                &format!("get_i{}_const_max", config.size),
                &[],
                Some(&config.i_max.clone()),
            );
        }

        assert_harness_gas_report!(harness);
    })
}

#[test]
fn sized_vals_in_sto() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "sized_vals_in_sto.fe", "Foo", &[]);

        let num = uint_token(68);
        let nums = uint_array_token(&(0..42).collect::<Vec<_>>());
        let string = string_token("there are 26 protons in fe");

        harness.test_function(&mut executor, "write_num", &[num.clone()], None);
        harness.test_function(&mut executor, "read_num", &[], Some(&num));

        harness.test_function(&mut executor, "write_nums", &[nums.clone()], None);
        harness.test_function(&mut executor, "read_nums", &[], Some(&nums));

        harness.test_function(&mut executor, "write_str", &[string.clone()], None);
        harness.test_function(&mut executor, "read_str", &[], Some(&string));

        harness.test_function(&mut executor, "emit_event", &[], None);
        harness.events_emitted(executor, &[("MyEvent", &[num, nums, string])]);

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn checked_arithmetic() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(
            &mut executor,
            "checked_arithmetic.fe",
            "CheckedArithmetic",
            &[],
        );

        for config in NumericAbiTokenBounds::get_all().iter() {
            // ADDITION

            // unsigned: max_value + 1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("add_u{}", config.size),
                &[config.u_max.clone(), uint_token(1)],
                &encoded_over_or_underflow(),
            );

            // unsigned: max_value + 0 works
            harness.test_function(
                &mut executor,
                &format!("add_u{}", config.size),
                &[config.u_max.clone(), uint_token(0)],
                Some(&config.u_max),
            );

            // signed: max_value + 1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("add_i{}", config.size),
                &[config.i_max.clone(), int_token(1)],
                &encoded_over_or_underflow(),
            );

            // signed: max_value + 0 works
            harness.test_function(
                &mut executor,
                &format!("add_i{}", config.size),
                &[config.i_max.clone(), int_token(0)],
                Some(&config.i_max),
            );

            // signed: min_value + -1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("add_i{}", config.size),
                &[config.i_min.clone(), int_token(-1)],
                &encoded_over_or_underflow(),
            );

            // signed: min_value + 0 works
            harness.test_function(
                &mut executor,
                &format!("add_i{}", config.size),
                &[config.i_min.clone(), int_token(0)],
                Some(&config.i_min),
            );

            // SUBTRACTION
            // unsigned: min_value - 1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("sub_u{}", config.size),
                &[config.u_min.clone(), uint_token(1)],
                &encoded_over_or_underflow(),
            );

            // unsigned: min_value - 0 works
            harness.test_function(
                &mut executor,
                &format!("sub_u{}", config.size),
                &[config.u_min.clone(), uint_token(0)],
                Some(&config.u_min),
            );

            // signed: min_value - 1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("sub_i{}", config.size),
                &[config.i_min.clone(), int_token(1)],
                &encoded_over_or_underflow(),
            );

            // signed: min_value - 0 works
            harness.test_function(
                &mut executor,
                &format!("sub_i{}", config.size),
                &[config.i_min.clone(), int_token(0)],
                Some(&config.i_min),
            );

            // signed: max_value - -1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("sub_i{}", config.size),
                &[config.i_max.clone(), int_token(-1)],
                &encoded_over_or_underflow(),
            );

            // signed: max_value - -0 works
            harness.test_function(
                &mut executor,
                &format!("sub_i{}", config.size),
                &[config.i_max.clone(), int_token(-0)],
                Some(&config.i_max),
            );

            // DIVISON
            // unsigned: anything / 0 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("div_u{}", config.size),
                &[config.u_max.clone(), uint_token(0)],
                &encoded_div_or_mod_by_zero(),
            );

            // unsigned: 3 / 2 works
            harness.test_function(
                &mut executor,
                &format!("div_u{}", config.size),
                &[uint_token(3), uint_token(2)],
                Some(&uint_token(1)),
            );

            // signed: anything / 0 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("div_i{}", config.size),
                &[config.i_max.clone(), int_token(0)],
                &encoded_div_or_mod_by_zero(),
            );

            // signed: min_value / -1 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("div_i{}", config.size),
                &[config.i_min.clone(), int_token(-1)],
                &encoded_over_or_underflow(),
            );

            // signed: 3 / -2 works
            harness.test_function(
                &mut executor,
                &format!("div_i{}", config.size),
                &[int_token(3), int_token(-2)],
                Some(&int_token(-1)),
            );

            // EXPONENTIATION
            // unsigned: max ** 2 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("pow_u{}", config.size),
                &[config.u_max.clone(), uint_token(2)],
                &encoded_over_or_underflow(),
            );

            // unsigned: 2 ** (bit_len-1) works
            harness.test_function(
                &mut executor,
                &format!("pow_u{}", config.size),
                &[uint_token(2), uint_token(config.size - 1)],
                Some(&ethabi::Token::Uint(
                    U256::from(2).pow(U256::from(config.size - 1)),
                )),
            );

            // signed: max ** 2 fails (overflow)
            harness.test_function_reverts(
                &mut executor,
                &format!("pow_i{}", config.size),
                &[config.i_max.clone(), uint_token(2)],
                &encoded_over_or_underflow(),
            );

            // signed: min ** 3 fails (underflow)
            harness.test_function_reverts(
                &mut executor,
                &format!("pow_i{}", config.size),
                &[config.i_min.clone(), uint_token(3)],
                &encoded_over_or_underflow(),
            );

            // signed: 2 ** (bit_len-2) works
            harness.test_function(
                &mut executor,
                &format!("pow_i{}", config.size),
                &[int_token(2), uint_token(config.size - 2)],
                Some(&ethabi::Token::Int(
                    U256::from(2).pow(U256::from(config.size - 2)),
                )),
            );

            // signed: -2 ** (bit_len-1) works
            harness.test_function(
                &mut executor,
                &format!("pow_i{}", config.size),
                &[int_token(-2), uint_token(config.size - 1)],
                Some(&ethabi::Token::Int(get_2s_complement_for_negative(
                    U256::from(2).pow(U256::from(config.size - 1)),
                ))),
            );

            // MODULO
            // unsigned: anything % 0 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mod_u{}", config.size),
                &[config.u_max.clone(), uint_token(0)],
                &encoded_div_or_mod_by_zero(),
            );

            // unsigned: max_value % 2 works
            harness.test_function(
                &mut executor,
                &format!("mod_u{}", config.size),
                &[config.u_max.clone(), uint_token(2)],
                Some(&uint_token(1)),
            );

            // signed: anything % 0 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mod_i{}", config.size),
                &[config.i_max.clone(), int_token(0)],
                &encoded_div_or_mod_by_zero(),
            );

            // unsigned: max_value % 2 works
            harness.test_function(
                &mut executor,
                &format!("mod_i{}", config.size),
                &[config.i_max.clone(), int_token(2)],
                Some(&int_token(1)),
            );

            // signed: 13 % -3 works
            harness.test_function(
                &mut executor,
                &format!("mod_i{}", config.size),
                &[int_token(13), int_token(-3)],
                Some(&int_token(1)),
            );

            // signed: -13 % 3 works
            harness.test_function(
                &mut executor,
                &format!("mod_i{}", config.size),
                &[int_token(-13), int_token(3)],
                Some(&int_token(-1)),
            );

            // MULTIPLICATION
            // unsigned: max_value * 2 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mul_u{}", config.size),
                &[config.u_max.clone(), uint_token(2)],
                &encoded_over_or_underflow(),
            );

            // unsigned: max_value * 1 works
            harness.test_function(
                &mut executor,
                &format!("mul_u{}", config.size),
                &[config.u_max.clone(), uint_token(1)],
                Some(&config.u_max),
            );

            // signed: max_value * 2 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mul_i{}", config.size),
                &[config.i_max.clone(), int_token(2)],
                &encoded_over_or_underflow(),
            );

            // signed: max_value * 1 works
            harness.test_function(
                &mut executor,
                &format!("mul_i{}", config.size),
                &[config.i_max.clone(), int_token(1)],
                Some(&config.i_max),
            );

            // signed: max_value * -2 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mul_i{}", config.size),
                &[config.i_max.clone(), int_token(-2)],
                &encoded_over_or_underflow(),
            );

            // signed: min_value * -2 fails
            harness.test_function_reverts(
                &mut executor,
                &format!("mul_i{}", config.size),
                &[config.i_min.clone(), int_token(-2)],
                &encoded_over_or_underflow(),
            );

            harness.test_function(
                &mut executor,
                &format!("mul_i{}", config.size),
                &[config.i_min.clone(), int_token(1)],
                Some(&config.i_min),
            );
        }

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn structs() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "structs.fe", "Foo", &[]);

        harness.test_function(&mut executor, "create_house", &[], None);
        harness.test_function(&mut executor, "bar", &[], Some(&uint_token(102)));

        let encoded_house = [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1, 44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 1, 244, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
        ];
        let token = ethabi::Token::Bytes(ethabi::Bytes::from(encoded_house));

        let house = ethabi::Token::Tuple(vec![
            uint_token(1000000),
            uint_token(250),
            uint_token(6),
            bool_token(true),
        ]);
        harness.test_function(&mut executor, "set_house", &[house.clone()], None);
        harness.test_function(&mut executor, "get_house", &[], Some(&house));

        harness.test_function(&mut executor, "encode_house", &[], Some(&token));

        harness.test_function(
            &mut executor,
            "hashed_house",
            &[],
            Some(&uint_token_from_dec_str(
                "46276961562062403346660092841258592376337652487249021183958956662511039738107",
            )),
        );

        harness.test_function(&mut executor, "create_mixed", &[], Some(&uint_token(1)));

        harness.test_function(
            &mut executor,
            "complex_struct_in_memory",
            &[],
            Some(&string_token("foo")),
        );

        harness.test_function(
            &mut executor,
            "complex_struct_in_storage",
            &[],
            Some(&string_token("foo")),
        );

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn return_complex_struct() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "return_complex_struct.fe", "Foo", &[]);

        let static_inner = tuple_token(&[int_token(10), int_token(20)]);
        let static_complex = tuple_token(&[static_inner, int_token(30)]);
        harness.test_function(&mut executor, "static_complex", &[], Some(&static_complex));

        let string = string_token("Hello");
        let string_complex = tuple_token(&[string, int_token(30)]);
        harness.test_function(&mut executor, "string_complex", &[], Some(&string_complex));

        let bytes = ethabi::Token::Bytes(ethabi::Bytes::from([1, 2, 3, 4, 5, 6, 7, 8]));
        let bytes_complex = tuple_token(&[bytes, int_token(30)]);
        harness.test_function(&mut executor, "bytes_complex", &[], Some(&bytes_complex));

        let nested_dynamic_complex = tuple_token(&[bytes_complex, static_complex, string_complex]);
        harness.test_function(
            &mut executor,
            "nested_dynamic_complex",
            &[],
            Some(&nested_dynamic_complex),
        );

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn keccak() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "keccak.fe", "Keccak", &[]);
        // The expected value in clear text is on purpose for added clarity. All other
        // tests use get_keccak256 to calculate the expected results on the fly.
        harness.test_function(
            &mut executor,
            "return_hash_from_u256",
            &[ethabi::Token::Bytes(U256::from(1).to_be_bytes().to_vec())],
            Some(&uint_token_from_dec_str(
                "80084422859880547211683076133703299733277748156566366325829078699459944778998",
            )),
        );

        harness.test_function(
            &mut executor,
            "return_hash_from_u256",
            &[ethabi::Token::Bytes(U256::from(1).to_be_bytes().to_vec())],
            Some(&ethabi::Token::Uint(
                keccak::full_as_bytes(&U256::from(1).to_be_bytes()).into(),
            )),
        );

        harness.test_function(
            &mut executor,
            "return_hash_from_u8",
            &[ethabi::Token::Bytes([1].into())],
            Some(&ethabi::Token::Uint(
                keccak::full_as_bytes(&1u8.to_be_bytes()).into(),
            )),
        );

        harness.test_function(
            &mut executor,
            "return_hash_from_u8",
            &[ethabi::Token::Bytes([0].into())],
            Some(&ethabi::Token::Uint(
                keccak::full_as_bytes(&0u8.to_be_bytes()).into(),
            )),
        );

        harness.test_function(
            &mut executor,
            "return_hash_from_foo",
            &[bytes_token("foo")],
            Some(&ethabi::Token::Uint(
                keccak::full_as_bytes("foo".as_bytes()).into(),
            )),
        );

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn short_circuit() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "short_circuit.fe", "Foo", &[]);
        harness.test_function(&mut executor, "bar", &[uint_token(6)], Some(&uint_token(1)));

        harness.test_function_reverts(&mut executor, "bar", &[uint_token(1)], &[]);

        harness.test_function(
            &mut executor,
            "short_circuit_and",
            &[bool_token(false)],
            Some(&bool_token(false)),
        );
        harness.test_function_reverts(&mut executor, "short_circuit_and", &[bool_token(true)], &[]);

        harness.test_function(
            &mut executor,
            "short_circuit_or",
            &[bool_token(true)],
            Some(&bool_token(true)),
        );
        harness.test_function_reverts(&mut executor, "short_circuit_or", &[bool_token(false)], &[]);

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn two_contracts() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "two_contracts.fe", "Foo", &[]);
        harness.test_function(&mut executor, "foo", &[], Some(&uint_token(42)));
        assert_harness_gas_report!(harness);
    })
}

#[test]
fn external_contract() {
    with_executor(&|mut executor| {
        let foo_harness = deploy_contract(&mut executor, "external_contract.fe", "Foo", &[]);
        let proxy_harness = deploy_contract(&mut executor, "external_contract.fe", "FooProxy", &[]);

        let foo_address = ethabi::Token::Address(foo_harness.address);
        let my_num = uint_token(26);
        let my_addrs = address_array_token(&["0", "1", "42", "3", "4"]);
        let my_string = string_token("hello world");

        proxy_harness.test_function(
            &mut executor,
            "call_emit_event",
            &[
                foo_address.clone(),
                my_num.clone(),
                my_addrs.clone(),
                my_string.clone(),
            ],
            None,
        );

        let a = 26;
        let b = 42;
        let c = 26 * 42;

        proxy_harness.test_function(
            &mut executor,
            "call_build_array",
            &[foo_address, uint_token(a), uint_token(b)],
            Some(&uint_array_token(&[a, c, b])),
        );

        proxy_harness.test_function(
            &mut executor,
            "add",
            &[uint_token(a), uint_token(b)],
            Some(&uint_token(68)),
        );

        foo_harness.events_emitted(executor, &[("MyEvent", &[my_num, my_addrs, my_string])]);

        assert_harness_gas_report!(proxy_harness);
    })
}

#[test]
fn create2_contract() {
    with_executor(&|mut executor| {
        let factory_harness =
            deploy_contract(&mut executor, "create2_contract.fe", "FooFactory", &[]);

        let foo_address = factory_harness
            .call_function(&mut executor, "create2_foo", &[])
            .expect("factory did not return an address")
            .into_address()
            .expect("not an address");

        let foo_harness = load_contract(foo_address, "create2_contract.fe", "Foo");

        foo_harness.test_function(&mut executor, "get_my_num", &[], Some(&uint_token(42)));

        assert_harness_gas_report!(factory_harness);
    })
}

#[test]
fn create_contract() {
    with_executor(&|mut executor| {
        let factory_harness =
            deploy_contract(&mut executor, "create_contract.fe", "FooFactory", &[]);

        let foo_address = factory_harness
            .call_function(&mut executor, "create_foo", &[])
            .expect("factory did not return an address")
            .into_address()
            .expect("not an address");

        let foo_harness = load_contract(foo_address, "create_contract.fe", "Foo");

        foo_harness.test_function(&mut executor, "get_my_num", &[], Some(&uint_token(42)));

        assert_harness_gas_report!(factory_harness);
    })
}

#[test]
fn create_contract_from_init() {
    with_executor(&|mut executor| {
        let factory_harness = deploy_contract(
            &mut executor,
            "create_contract_from_init.fe",
            "FooFactory",
            &[],
        );

        let foo_address = factory_harness
            .call_function(&mut executor, "get_foo_addr", &[])
            .expect("factory did not return an address")
            .into_address()
            .expect("not an address");

        let foo_harness = load_contract(foo_address, "create_contract_from_init.fe", "Foo");

        foo_harness.test_function(&mut executor, "get_my_num", &[], Some(&uint_token(42)));

        assert_harness_gas_report!(factory_harness);
    })
}

#[rstest(
    fixture_file,
    contract_name,
    case("ownable.fe", "Ownable"),
    case("empty.fe", "Empty"),
    case("return_unit.fe", "Foo")
)]
fn can_deploy_fixture(fixture_file: &str, contract_name: &str) {
    with_executor(&|mut executor| {
        deploy_contract(&mut executor, fixture_file, contract_name, &[]);
    })
}

#[test]
fn self_address() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "self_address.fe", "Foo", &[]);
        harness.test_function(
            &mut executor,
            "my_address",
            &[],
            Some(&ethabi::Token::Address(harness.address)),
        );
    });
}

#[rstest(
    target,
    op,
    value,
    expected,
    case(2, "add", 5, 7),
    case(42, "sub", 26, 16),
    case(10, "mul", 42, 420),
    case(43, "div", 5, 8),
    case(43, "mod", 5, 3),
    case(3, "pow", 5, 243),
    case(1, "lshift", 7, 128),
    case(128, "rshift", 7, 1),
    case(26, "bit_or", 42, 58),
    case(26, "bit_xor", 42, 48),
    case(26, "bit_and", 42, 10),
    case(2, "add_from_sto", 5, 7),
    case(2, "add_from_mem", 5, 7)
)]
fn aug_assign(target: u64, op: &str, value: u64, expected: u64) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "aug_assign.fe", "Foo", &[]);
        harness.test_function(
            &mut executor,
            op,
            &[uint_token(target), uint_token(value)],
            Some(&uint_token(expected)),
        );
        assert_harness_gas_report!(harness, op);
    });
}

#[test]
fn tuple_destructuring() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "tuple_destructuring.fe", "Foo", &[]);
        harness.test_function(&mut executor, "bar", &[], Some(&uint_token(42)));
        harness.test_function(
            &mut executor,
            "baz",
            &[uint_token(1), bool_token(false)],
            Some(&uint_token(1)),
        );
        assert_harness_gas_report!(harness);
    });
}

#[test]
fn abi_decode_checks() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "abi_decode_checks.fe", "Foo", &[]);
        let revert_data = encoded_invalid_abi_data();

        // decode_u256
        {
            let input = [uint_token(99999999)];
            let data = harness.build_calldata("decode_u256", &input);

            // add a byte
            let mut tampered_data = data.clone();
            tampered_data.push(42);
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // remove last 8 bytes
            let mut tampered_data = data.clone();
            tampered_data.truncate(data.len() - 8);
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);
        }

        // // decode_u128_bool
        {
            let input = [uint_token(99999999), bool_token(true)];
            let data = harness.build_calldata("decode_u128_bool", &input);

            // add a byte
            let mut tampered_data = data.clone();
            tampered_data.push(42);
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of `u128`
            let mut tampered_data = data.clone();
            // 4 bytes past end of selector (4 + 4)
            tampered_data[9] = 26;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of u128
            let mut tampered_data = data;
            // 8 bytes past end of u128 (4 + 32 + 8)
            tampered_data[44] = 1;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);
        }

        // decode_u256_bytes_tuple_array
        {
            let head_size = 32 + 32 + 64 + (26 * 32);
            let input = [
                uint_token(99999999),
                bytes_token(&"ten bytes.".repeat(10)),
                tuple_token(&[address_token("a"), uint_token(42)]),
                int_array_token(&(-10..16).collect::<Vec<_>>()),
            ];
            let data = harness.build_calldata("decode_u256_bytes_tuple_array", &input);

            // add a byte
            let mut tampered_data = data.clone();
            tampered_data.push(42);
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // remove a byte
            let mut tampered_data = data.clone();
            tampered_data.truncate(tampered_data.len() - 1);
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // give invalid length to bytes. it expects 100, we give 99
            let mut tampered_data = data.clone();
            // final byte in data size location for bytes[100]
            let byte_index = 4 + head_size + 31;
            tampered_data[byte_index] = 99;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of bytes
            let mut tampered_data = data.clone();
            // the first byte directly following the bytes' data
            let byte_index = 4 + head_size + 32 + 100;
            tampered_data[byte_index] = 128; // set the first bit to `1`
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of bytes
            let mut tampered_data = data.clone();
            // the first byte directly following the bytes' data
            let byte_index = 4 + head_size + 32 + 127;
            tampered_data[byte_index] = 1; // set the last bit to `1`
                                           // sanity check
            assert_eq!(tampered_data.len(), byte_index + 1);
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of the tuple
            let mut tampered_data = data.clone();
            // first byte in the address padding
            let byte_index = 4 + 32 + 32;
            // set the last bit in the address padding to `1`
            tampered_data[byte_index] = 128;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of the tuple
            let mut tampered_data = data.clone();
            // last byte in the address padding
            let byte_index = 4 + 32 + 32 + 11;
            // set the last bit in the address padding to `1`
            tampered_data[byte_index] = 1;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of the tuple
            let mut tampered_data = data.clone();
            // 5 bytes past the end of address
            let byte_index = 4 + 32 + 32 + 32 + 5;
            tampered_data[byte_index] = 26;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place zero byte in padded region of a negative int
            let mut tampered_data = data.clone();
            // index 2 of array and 0 bytes in
            let byte_index = 4 + 32 + 32 + 64 + (2 * 32);
            tampered_data[byte_index] = 0;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of a positive int
            let mut tampered_data = data;
            // index 12 of array and 4 bytes in
            let byte_index = 4 + 32 + 32 + 64 + (12 * 32 + 4);
            tampered_data[byte_index] = 26;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);
        }

        // decode_string_address_bytes_bool
        {
            let func_name = "decode_string_address_bytes_bool";
            let input = [
                string_token("hello Fe"),
                address_token("baddad"),
                bytes_token(&"ten bytes.".repeat(100)),
                bool_token(true),
            ];
            let data = harness.build_calldata(func_name, &input);

            let head_size = 32 + 32 + 32 + 32;
            let string_data_size = 64;
            let string_size = 8;
            let bytes_data_size = 32 + 1024;
            let bytes_size = 1000;

            // add 100 bytes
            let mut tampered_data = data.clone();
            tampered_data.append(vec![42; 100].as_mut());
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // remove 20 bytes
            let mut tampered_data = data.clone();
            tampered_data.truncate(tampered_data.len() - 20);
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // set string length to value that extends beyond the next data offset
            let mut tampered_data = data.clone();
            let byte_index = 4 + head_size + 31;
            // data section would now occupy 64 + 32 bytes, instead of 32 + 32 bytes
            // this would break the equivalence of string's `data_offset + data_size` and
            // the bytes' `data_offset`, making the encoding invalid
            tampered_data[byte_index] = 33;
            // the string length is completely valid otherwise. 32 for example will not
            // revert tampered_data[byte_index] = 32;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of the string
            let mut tampered_data = data.clone();
            // last byte in string encoding
            let byte_index = 4 + head_size + string_data_size - 1;
            // set last bit to 1
            tampered_data[byte_index] = 1;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of the string
            let mut tampered_data = data.clone();
            // first byte in padded section of string encoding
            let byte_index = 4 + head_size + 32 + string_size;
            // set first bit to 1
            tampered_data[byte_index] = 128;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of the bytes
            let mut tampered_data = data.clone();
            // last byte in bytes encoding
            let byte_index = 4 + head_size + string_data_size + bytes_data_size - 1;
            // set last bit to 1
            tampered_data[byte_index] = 1;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // place non-zero byte in padded region of the bytes
            let mut tampered_data = data;
            // first byte in padded section of string encoding
            let byte_index = 4 + head_size + string_data_size + 32 + bytes_size;
            // set first bit to 1
            tampered_data[byte_index] = 128;
            harness.test_call_reverts(&mut executor, tampered_data, &revert_data);

            // invalid since bytes has size 990 instead of 1000
            let invalid_input = [
                string_token("hello Fe"),
                address_token("baddad"),
                bytes_token(&"ten bytes.".repeat(99)),
                bool_token(true),
            ];
            harness.test_function_reverts(&mut executor, func_name, &invalid_input, &revert_data);

            // invalid since string has size 100, which is greater than 80
            let invalid_input = [
                string_token(&"hello Fe..".repeat(10)),
                address_token("baddad"),
                bytes_token(&"ten bytes.".repeat(100)),
                bool_token(true),
            ];
            harness.test_function_reverts(&mut executor, func_name, &invalid_input, &revert_data);
        }
    });
}

#[test]
fn abi_decode_complex() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "abi_decode_complex.fe", "Foo", &[]);

        let static_inner = tuple_token(&[int_token(10), int_token(20)]);
        let static_complex = tuple_token(&[static_inner, int_token(30)]);
        harness.test_function(
            &mut executor,
            "decode_static_complex",
            &[static_complex.clone()],
            Some(&static_complex),
        );

        let string = string_token("Hello");
        let string_complex = tuple_token(&[string, int_token(30)]);
        harness.test_function(
            &mut executor,
            "decode_string_complex",
            &[string_complex.clone()],
            Some(&string_complex),
        );

        let bytes = ethabi::Token::Bytes(ethabi::Bytes::from([1, 2, 3, 4, 5, 6, 7, 8]));
        let bytes_complex = tuple_token(&[bytes, int_token(30)]);
        harness.test_function(
            &mut executor,
            "decode_bytes_complex",
            &[bytes_complex.clone()],
            Some(&bytes_complex),
        );

        let nested_dynamic_complex =
            tuple_token(&[bytes_complex, static_complex.clone(), string_complex]);
        harness.test_function(
            &mut executor,
            "decode_nested_dynamic_complex",
            &[nested_dynamic_complex.clone()],
            Some(&nested_dynamic_complex),
        );

        let static_complex_array =
            ethabi::Token::FixedArray(std::iter::repeat(static_complex).take(3).collect());
        harness.test_function(
            &mut executor,
            "decode_static_complex_elem_array",
            &[static_complex_array.clone()],
            Some(&static_complex_array),
        );

        let dynamic_complex_array =
            ethabi::Token::FixedArray(std::iter::repeat(nested_dynamic_complex).take(3).collect());
        harness.test_function(
            &mut executor,
            "decode_dynamic_complex_elem_array",
            &[dynamic_complex_array.clone()],
            Some(&dynamic_complex_array),
        );

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn intrinsics() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "intrinsics.fe", "Intrinsics", &[]);

        executor
            .state_mut()
            .deposit(harness.address, U256::from(123));

        harness.test_function(
            &mut executor,
            "add",
            &[uint_token(10), uint_token(100)],
            Some(&uint_token(110)),
        );
        harness.test_function(&mut executor, "self_balance", &[], Some(&uint_token(123)));
        harness.test_function(&mut executor, "calldatasize", &[], Some(&uint_token(4)));
        harness.test_function(
            &mut executor,
            "calldatacopy",
            &[uint_token(36), uint_token(32)],
            Some(&uint_token(32)),
        );
        harness.test_function(&mut executor, "callvalue", &[], Some(&uint_token(0)));
        harness.test_function(&mut executor, "basefee", &[], Some(&uint_token(0)));

        harness.test_function(
            &mut executor,
            "caller",
            &[],
            Some(&ethabi::Token::Address(harness.caller)),
        );

        assert_harness_gas_report!(harness);
    });
}

#[rstest(
    method,
    params,
    expected,
    case("bar", &[int_token(-10)], Some(int_array_token(&[-10]))),
    case("bar", &[int_token(100)], Some(int_array_token(&[100]))),
)]
fn signext_int_array1(method: &str, params: &[ethabi::Token], expected: Option<ethabi::Token>) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "return_from_storage_array_i8.fe", "Foo", &[]);

        harness.test_function(&mut executor, method, params, expected.as_ref());

        assert_harness_gas_report!(harness, params);
    })
}

#[rstest(
    method,
    value,
    case("i8_array", int_array_token(&[-10, 100, -128, 127])),
    case("i32_array", int_array_token(&[-10, 100, -2147483648, 2147483647])),
)]
fn signext_int_array2(method: &str, value: ethabi::Token) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "return_int_array.fe", "Foo", &[]);

        harness.test_function(&mut executor, method, &[value.clone()], Some(&value));

        assert_harness_gas_report!(harness, method);
    })
}

#[test]
fn ctx_param_simple() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "ctx_param_simple.fe", "Foo", &[]);

        harness.test_function(&mut executor, "bar", &[], Some(&uint_token(0)));

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn ctx_param_internal_func_call() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "ctx_param_internal_func_call.fe", "Foo", &[]);

        harness.test_function(&mut executor, "bar", &[], Some(&uint_token(0)));

        assert_harness_gas_report!(harness);
    });
}

#[test]
fn ctx_init_in_call() {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, "ctx_init_in_call.fe", "Foo", &[]);

        match harness.capture_call_raw_bytes(&mut executor, vec![]) {
            Capture::Exit((ExitReason::Succeed(_), return_bytes)) => {
                assert_eq!(
                    [&[0; 12], harness.address.as_bytes()].concat(),
                    return_bytes
                )
            }
            Capture::Exit(_) => panic!("call didn't succeed"),
            Capture::Trap(_) => panic!("trapped!"),
        }
    });
}

// These tests are expected to make assertions in Fe only
#[rstest(
    fixture_file,
    case::simple_traits("simple_traits.fe"),
    case::self_type("self_type.fe"),
    case::trait_associated_functions("trait_associated_functions.fe"),
    case::generic_functions("generic_functions.fe"),
    case::generic_functions_primitves("generic_functions_primitves.fe"),
    case::contract_pure_fns("contract_pure_fns.fe")
)]
fn execution_tests(fixture_file: &str) {
    with_executor(&|mut executor| {
        let harness = deploy_contract(&mut executor, fixture_file, "Example", &[]);
        harness.test_function(&mut executor, "run_test", &[], None);
        assert_harness_gas_report!(harness, fixture_file);
    })
}
