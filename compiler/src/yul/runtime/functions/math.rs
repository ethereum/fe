use crate::yul::constants::numeric_min_max;
use crate::yul::names;
use fe_analyzer::namespace::types::Integer;
use yultsur::*;

/// Return a vector of runtime functions for additions with over-/underflow
/// protection
pub fn checked_add_fns() -> Vec<yul::Statement> {
    vec![
        checked_add_unsigned(Integer::U256),
        checked_add_unsigned(Integer::U128),
        checked_add_unsigned(Integer::U64),
        checked_add_unsigned(Integer::U32),
        checked_add_unsigned(Integer::U16),
        checked_add_unsigned(Integer::U8),
        checked_add_signed(Integer::I256),
        checked_add_signed(Integer::I128),
        checked_add_signed(Integer::I64),
        checked_add_signed(Integer::I32),
        checked_add_signed(Integer::I16),
        checked_add_signed(Integer::I8),
    ]
}

/// Return a vector of runtime functions for subtraction with over-/underflow
/// protection
pub fn checked_sub_fns() -> Vec<yul::Statement> {
    vec![
        checked_sub_unsigned(),
        checked_sub_signed(Integer::I256),
        checked_sub_signed(Integer::I128),
        checked_sub_signed(Integer::I64),
        checked_sub_signed(Integer::I32),
        checked_sub_signed(Integer::I16),
        checked_sub_signed(Integer::I8),
    ]
}

// Return all math runtime functions
pub fn all() -> Vec<yul::Statement> {
    [checked_add_fns(), checked_sub_fns()].concat()
}

fn checked_add_unsigned(size: Integer) -> yul::Statement {
    if size.is_signed() {
        panic!("Expected unsigned integer")
    }
    let fn_name = names::checked_add(&size);
    let max_value = get_max(size);

    function_definition! {
        function [fn_name](val1, val2) -> sum {
            // overflow, if val1 > (max_value - val2)
            (if (gt(val1, (sub([max_value], val2)))) { (revert(0, 0)) })
            (sum := add(val1, val2))
        }
    }
}

fn checked_add_signed(size: Integer) -> yul::Statement {
    if !size.is_signed() {
        panic!("Expected signed integer")
    }
    let (min_value, max_value) = get_min_max(size.clone());
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

fn checked_sub_unsigned() -> yul::Statement {
    function_definition! {
        function checked_sub_unsigned(val1, val2) -> diff {
            // underflow, if val2  > val1
            (if (lt(val1, val2)) { (revert(0, 0)) })
            (diff := sub(val1, val2))
        }
    }
}

fn checked_sub_signed(size: Integer) -> yul::Statement {
    if !size.is_signed() {
        panic!("Expected signed integer")
    }
    let fn_name = names::checked_sub(&size);
    let (min_value, max_value) = get_min_max(size);

    function_definition! {
        function [fn_name](val1, val2) -> diff {
            // underflow, if val2 >= 0 and val1 < (min_value + val2)
            (if (and((iszero((slt(val2, 0)))), (slt(val1, (add([min_value], val2)))))) { (revert(0, 0)) })
            // overflow, if val2 < 0 and val1 > (max_value + val2)
            (if (and((slt(val2, 0)), (sgt(val1, (add([max_value], val2)))))) { (revert(0, 0)) })
            (diff := sub(val1, val2))
        }
    }
}

fn get_min_max(integer: Integer) -> (yul::Expression, yul::Expression) {
    let map = numeric_min_max();

    map.get(&integer)
        .expect("Expected integer to be in map")
        .to_owned()
}

fn get_max(integer: Integer) -> yul::Expression {
    let (_, max) = get_min_max(integer);
    max
}
