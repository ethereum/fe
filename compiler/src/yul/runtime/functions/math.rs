use fe_analyzer::namespace::types::Integer;
use yultsur::*;

fn checked_add_unsigned(size: Integer, max_value: yul::Expression) -> yul::Statement {
    if size.is_signed() {
        panic!("Expected unsigned integer")
    }
    let size: &str = size.into();
    let fn_name = identifier! {(format!("checked_add_{}", size.to_lowercase()))};
    function_definition! {
        function [fn_name](val1, val2) -> sum {
            // overflow, if val1 > (max_value - val2)
            (if (gt(val1, (sub([max_value], val2)))) { (revert(0, 0)) })
            (sum := add(val1, val2))
        }
    }
}

/// Add two u256 numbers. Revert if result overflows.
pub fn checked_add_u256() -> yul::Statement {
    checked_add_unsigned(
        Integer::U256,
        literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff},
    )
}

/// Add two u128 numbers. Revert if result overflows.
pub fn checked_add_u128() -> yul::Statement {
    checked_add_unsigned(
        Integer::U128,
        literal_expression! {0xffffffffffffffffffffffffffffffff},
    )
}

/// Add two u64 numbers. Revert if result overflows.
pub fn checked_add_u64() -> yul::Statement {
    checked_add_unsigned(Integer::U64, literal_expression! {0xffffffffffffffff})
}

/// Add two u32 numbers. Revert if result overflows.
pub fn checked_add_u32() -> yul::Statement {
    checked_add_unsigned(Integer::U32, literal_expression! {0xffffffff})
}

/// Add two u16 numbers. Revert if result overflows.
pub fn checked_add_u16() -> yul::Statement {
    checked_add_unsigned(Integer::U16, literal_expression! {0xffff})
}

/// Add two u8 numbers. Revert if result overflows.
pub fn checked_add_u8() -> yul::Statement {
    checked_add_unsigned(Integer::U8, literal_expression! {0xff})
}

fn checked_add_signed(
    size: Integer,
    min_value: yul::Expression,
    max_value: yul::Expression,
) -> yul::Statement {
    if !size.is_signed() {
        panic!("Expected signed integer")
    }
    let size: &str = size.into();
    let fn_name = identifier! {(format!("checked_add_{}", size.to_lowercase()))};
    function_definition! {
        function [fn_name](val1, val2) -> sum {
            // overflow, if val1 >= 0 and val2 > (max_value - val1)
            (if (and((iszero((slt(val1, 0)))), (sgt(val2, (sub([max_value], val1)))))) { (revert(0, 0)) })
            // underflow, if val1 < 0 and val2 < (min_val - val1)
            (if (and((slt(val1, 0)), (slt(val2, (sub([min_value], val1)))))) { (revert(0, 0)) })
            (sum := add(val1, val2))
        }
    }
}

/// Add two i256 numbers. Revert if result over- or underflows.
pub fn checked_add_i256() -> yul::Statement {
    checked_add_signed(
        Integer::I256,
        literal_expression! {0x8000000000000000000000000000000000000000000000000000000000000000},
        literal_expression! {0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff},
    )
}

/// Add two i128 numbers. Revert if result over- or underflows.
pub fn checked_add_i128() -> yul::Statement {
    checked_add_signed(
        Integer::I128,
        literal_expression! {0xffffffffffffffffffffffffffffffff80000000000000000000000000000000},
        literal_expression! {0x7fffffffffffffffffffffffffffffff},
    )
}

/// Add two i64 numbers. Revert if result over- or underflows.
pub fn checked_add_i64() -> yul::Statement {
    checked_add_signed(
        Integer::I64,
        literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000},
        literal_expression! {0x7fffffffffffffff},
    )
}

/// Add two i32 numbers. Revert if result over- or underflows.
pub fn checked_add_i32() -> yul::Statement {
    checked_add_signed(
        Integer::I32,
        literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000},
        literal_expression! {0x7fffffff},
    )
}

/// Add two i16 numbers. Revert if result over- or underflows.
pub fn checked_add_i16() -> yul::Statement {
    checked_add_signed(
        Integer::I16,
        literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000},
        literal_expression! {0x7fff},
    )
}

/// Add two i8 numbers. Revert if result over- or underflows.
pub fn checked_add_i8() -> yul::Statement {
    checked_add_signed(
        Integer::I8,
        literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80},
        literal_expression! {0x7f},
    )
}
