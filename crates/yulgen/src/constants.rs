use fe_analyzer::namespace::types::Integer;
use maplit::hashmap;
use std::collections::HashMap;
use yultsur::*;

/// Return a hashmap containing min/max YUL literals for each supported integer
/// size
pub fn numeric_min_max() -> HashMap<Integer, (yul::Expression, yul::Expression)> {
    hashmap! {
        Integer::U256 => (
            literal_expression! {0x0},
            literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff},
        ),
        Integer::U128 => (
            literal_expression! {0x0},
            literal_expression! {0xffffffffffffffffffffffffffffffff},
        ),
        Integer::U64 => (
            literal_expression! {0x0},
            literal_expression! {0xffffffffffffffff},
        ),
        Integer::U32 => (
            literal_expression! {0x0},
            literal_expression! {0xffffffff},
        ),
        Integer::U16 => (
            literal_expression! {0x0},
            literal_expression! {0xffff},
        ),
        Integer::U8 => (
            literal_expression! {0x0},
            literal_expression! {0xff},
        ),
        Integer::I256 => (
            literal_expression! {0x8000000000000000000000000000000000000000000000000000000000000000},
            literal_expression! {0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff}
        ),
        Integer::I128 => (
            literal_expression! {0xffffffffffffffffffffffffffffffff80000000000000000000000000000000},
            literal_expression! {0x7fffffffffffffffffffffffffffffff}
        ),
        Integer::I64 => (
            literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000},
            literal_expression! {0x7fffffffffffffff}
        ),
        Integer::I32 => (
            literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000},
            literal_expression! {0x7fffffff}
        ),
        Integer::I16 => (
            literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000},
            literal_expression! {0x7fff}
        ),
        Integer::I8 => (
            literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80},
            literal_expression! {0x7f}
        )
    }
}

// Panic codes as defined by solidity
// https://docs.soliditylang.org/en/v0.8.6/control-structures.html?highlight=0x12#panic-via-assert-and-error-via-require

pub const PANIC_FAILED_ASSERTION: usize = 0x01;
pub const PANIC_OVER_OR_UNDERFLOW: usize = 0x11;
pub const PANIC_DIV_OR_MOD_BY_ZERO: usize = 0x12;
pub const PANIC_OUT_OF_BOUNDS: usize = 0x32;

pub const ERROR_INVALID_ABI_DATA: usize = 0x103;
