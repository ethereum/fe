use fe_common::diagnostics::{diagnostics_string, print_diagnostics};
use fe_common::files::FileStore;
use insta::assert_snapshot;

fn error_string(path: &str, src: &str) -> String {
    let mut files = FileStore::new();
    let id = files.add_file(path, src);

    let fe_module = match fe_parser::parse_file(src, id) {
        Ok((module, _)) => module,
        Err(diags) => {
            print_diagnostics(&diags, &files);
            panic!("parsing failed");
        }
    };
    let err = fe_analyzer::analyze(&fe_module, id).unwrap_err();

    let mut errstr = diagnostics_string(&err.diagnostics, &files);
    if let Some(classic) = err.classic {
        errstr.push_str(&format!("\n\n{}", classic.format_user(&src)));
    }
    errstr
}

macro_rules! test_file {
    ($name:ident) => {
        #[test]
        fn $name() {
            let path = format!("tests/fixtures/errors/{}.fe", stringify!($name));
            dbg!(&path);
            let src = std::fs::read_to_string(&path).unwrap();
            assert_snapshot!(error_string(&path, &src));
        }
    };
}

macro_rules! test_stmt {
    ($name:ident, $stmt:expr) => {
        #[test]
        fn $name() {
            let src = format!(
                "contract C:\n pub def f():\n  {}",
                $stmt.replace('\n', "\n  ")
            );
            assert_snapshot!(error_string("[snippet]", &src));
        }
    };
}

test_stmt! { array_non_primitive, "x: (u8, u8)[10]" }
test_stmt! { array_mixed_types, "x: u256[3] = [1, address(0), \"hi\"]" }
test_stmt! { assert_reason_not_string, "assert true, 1" }
test_stmt! { assign_int, "5 = 6" }
test_stmt! { assign_call, "self.f() = 10" }
test_stmt! { assign_type_mismatch, "x: u256 = 10\nx = address(0)" }
test_stmt! { aug_assign_non_numeric, "a: u256 = 1\nb: bool = true\na += b" }
test_stmt! { bad_map, "x: map<u8, u8, u8>" }
test_stmt! { bad_map2, "x: map<address, 100>" }
test_stmt! { bad_map3, "x: map<>" }
test_stmt! { bad_map4, "x: map<y>" }
test_stmt! { bad_map5, "x: map<map<u8, u8>, address>" }
test_stmt! { bad_map6, "x: map<10, 20>" }
test_stmt! { binary_op_add_uints, "a: u256 = 1\nb: u8 = 2\na + b" }
test_stmt! { binary_op_lshift_bool, "a: bool = true\nb: i256\na << b" }
test_stmt! { binary_op_lshift_with_int, "a: u256 = 1\nb: i256 = 2\na << b" }
test_stmt! { binary_op_pow_int, "a: u256 = 1\nb: i256 = 2\na ** b" }
test_stmt! { binary_op_boolean_mismatch1, "10 and true" }
test_stmt! { binary_op_boolean_mismatch2, "false or 1" }
test_stmt! { binary_op_boolean_mismatch3, "1 or 2" }
test_stmt! { break_without_loop, "break" }
test_stmt! { break_without_loop_2, "if true:\n  break" }
test_stmt! { call_undefined_function_on_contract, "self.doesnt_exist()" }
test_stmt! { clone_arg_count, "x: u256[2] = [5, 6]\ny: u256[2] = x.clone(y)" }
test_stmt! { continue_without_loop, "continue" }
test_stmt! { continue_without_loop_2, "if true:\n  continue" }
test_stmt! { emit_undefined_event, "emit MyEvent()" }
test_stmt! { keccak_called_with_wrong_type, "x: u256[1]\nkeccak256(foo=x, 10)" }
test_stmt! { non_bool_and, "x: bool = true\ny: u256 = 1\nx = x and y" }
test_stmt! { non_bool_or, "x: bool = true\ny: u256 = 1\nx = x or y" }
test_stmt! { overflow_i128_neg, "i128(-170141183460469231731687303715884105729)" }
test_stmt! { overflow_i128_pos, "i128(170141183460469231731687303715884105728)" }
test_stmt! { overflow_i16_neg, "i16(-32769)" }
test_stmt! { overflow_i16_pos, "i16(32768)" }
test_stmt! { overflow_i256_neg, "i256(-57896044618658097711785492504343953926634992332820282019728792003956564819969)" }
test_stmt! { overflow_i256_pos, "i256(57896044618658097711785492504343953926634992332820282019728792003956564819968)" }
test_stmt! { overflow_i32_neg, "i32(-2147483649)" }
test_stmt! { overflow_i32_pos, "i32(2147483648)" }
test_stmt! { overflow_i64_neg, "i64(-9223372036854775809)" }
test_stmt! { overflow_i64_pos, "i64(9223372036854775808)" }
test_stmt! { overflow_i8_neg, "i8(-129)" }
test_stmt! { overflow_i8_pos, "i8(128)" }
test_stmt! { overflow_literal_too_big, "115792089237316195423570985008687907853269984665640564039457584007913129639936" }
test_stmt! { overflow_literal_too_small, "-115792089237316195423570985008687907853269984665640564039457584007913129639936" }
test_stmt! { overflow_u128_neg, "u128(-1)" }
test_stmt! { overflow_u128_pos, "u128(340282366920938463463374607431768211456)" }
test_stmt! { overflow_u16_neg, "u16(-1)" }
test_stmt! { overflow_u16_pos, "u16(65536)" }
test_stmt! { overflow_u256_neg, "u256(-1)" }
test_stmt! { overflow_u256_pos, "u256(115792089237316195423570985008687907853269984665640564039457584007913129639936)" }
test_stmt! { overflow_u32_neg, "u32(-1)" }
test_stmt! { overflow_u32_pos, "u32(4294967296)" }
test_stmt! { overflow_u64_neg, "u64(-1)" }
test_stmt! { overflow_u64_pos, "u64(18446744073709551616)" }
test_stmt! { overflow_u8_neg, "u8(-1)" }
test_stmt! { overflow_u8_pos, "u8(256)" }
test_stmt! { pow_with_signed_exponent, "base: i128\nexp: i128\nbase ** exp" }
// Exponent can be unsigned but needs to be same size or smaller
test_stmt! { pow_with_wrong_capacity, "base: i128\nexp: u256\nbase ** exp" }
test_stmt! { string_capacity_mismatch, "string3(\"too long\")" }
test_stmt! { ternary_type_mismatch, "10 if 100 else true" }
test_stmt! { type_constructor_from_variable, "x: u8\ny: u16 = u16(x)" }
test_stmt! { type_constructor_arg_count, "x: u8 = u8(1, 10)" }
test_stmt! { unary_minus_on_bool, "x: bool = true\n-x" }
test_stmt! { unary_not_on_int, "x: u256 = 10\nnot x" }
test_stmt! { undefined_type, "x: foobar = 10" }
test_stmt! { undefined_generic_type, "x: foobar<u256> = 10" }
test_stmt! { unexpected_return, "return 1" }

test_file! { bad_tuple_attr1 }
test_file! { bad_tuple_attr2 }
test_file! { call_event_with_wrong_types }
test_file! { call_undefined_function_on_external_contract }
test_file! { call_undefined_function_on_memory_struct }
test_file! { call_undefined_function_on_storage_struct }
test_file! { circular_dependency_create }
test_file! { circular_dependency_create2 }
test_file! { duplicate_contract_in_module }
test_file! { duplicate_event_in_contract }
test_file! { duplicate_field_in_contract }
test_file! { duplicate_field_in_struct }
test_file! { duplicate_method_in_contract }
test_file! { duplicate_struct_in_module }
test_file! { duplicate_typedef_in_module }
test_file! { duplicate_var_in_child_scope }
test_file! { duplicate_var_in_contract_method }
test_file! { emit_bad_args }
test_file! { external_call_type_error }
test_file! { external_call_wrong_number_of_params }
test_file! { indexed_event }
test_file! { mismatch_return_type }
test_file! { missing_return }
test_file! { missing_return_in_else }
test_file! { needs_mem_copy }
test_file! { not_in_scope }
test_file! { not_in_scope_2 }
test_file! { return_addition_with_mixed_types }
test_file! { return_call_to_fn_with_param_type_mismatch }
test_file! { return_call_to_fn_without_return }
test_file! { return_from_init }
test_file! { return_lt_mixed_types }
test_file! { strict_boolean_if_else }
test_file! { struct_call_bad_args }
test_file! { struct_call_without_kw_args }
