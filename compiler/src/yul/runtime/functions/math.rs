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

/// Return a vector of runtime functions for divisions with
/// over-/underflow protection
pub fn checked_div_fns() -> Vec<yul::Statement> {
    vec![
        checked_div_unsigned(),
        checked_div_signed(Integer::I256),
        checked_div_signed(Integer::I128),
        checked_div_signed(Integer::I64),
        checked_div_signed(Integer::I32),
        checked_div_signed(Integer::I16),
        checked_div_signed(Integer::I8),
    ]
}

/// Return a vector of runtime functions for multiplications with
/// over-/underflow protection
pub fn checked_mul_fns() -> Vec<yul::Statement> {
    vec![
        checked_mul_unsigned(Integer::U256),
        checked_mul_unsigned(Integer::U128),
        checked_mul_unsigned(Integer::U64),
        checked_mul_unsigned(Integer::U32),
        checked_mul_unsigned(Integer::U16),
        checked_mul_unsigned(Integer::U8),
        checked_mul_signed(Integer::I256),
        checked_mul_signed(Integer::I128),
        checked_mul_signed(Integer::I64),
        checked_mul_signed(Integer::I32),
        checked_mul_signed(Integer::I16),
        checked_mul_signed(Integer::I8),
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
    [
        checked_add_fns(),
        checked_div_fns(),
        checked_mul_fns(),
        checked_sub_fns(),
    ]
    .concat()
}

fn checked_mul_unsigned(size: Integer) -> yul::Statement {
    if size.is_signed() {
        panic!("Expected unsigned integer")
    }
    let fn_name = names::checked_mul(&size);
    let max_value = get_max(size);

    function_definition! {
        function [fn_name](val1, val2) -> product {
            // overflow, if val1 != 0 and val2 > (max_value / val1)
            (if (and((iszero((iszero(val1)))), (gt(val2, (div([max_value], val1)))))) { (revert(0, 0)) })
            (product := mul(val1, val2))
        }
    }
}

fn checked_mul_signed(size: Integer) -> yul::Statement {
    if !size.is_signed() {
        panic!("Expected signed integer")
    }
    let fn_name = names::checked_mul(&size);
    let (min_value, max_value) = get_min_max(size);

    function_definition! {
        function [fn_name](val1, val2) -> product {
            // overflow, if val1 > 0, val2 > 0 and val1 > (max_value / val2)
            (if (and((and((sgt(val1, 0)), (sgt(val2, 0)))), (gt(val1, (div([max_value.clone()], val2)))))) { (revert(0, 0)) })
            // underflow, if val1 > 0, val2 < 0 and val2 < (min_value / val1)
            (if (and((and((sgt(val1, 0)), (slt(val2, 0)))), (slt(val2, (sdiv([min_value.clone()], val1)))))) { (revert(0, 0)) })
            // underflow, if val1 < 0, val2 > 0 and val1 < (min_value / val2)
            (if (and((and((slt(val1, 0)), (sgt(val2, 0)))), (slt(val1, (sdiv([min_value], val2)))))) { (revert(0, 0)) })
            // overflow, if val1 < 0, val2 < 0 and val1 < (max_value / val2)
            (if (and((and((slt(val1, 0)), (slt(val2, 0)))), (slt(val1, (sdiv([max_value], val2)))))) { (revert(0, 0)) })
            (product := mul(val1, val2))
        }
    }
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
    let fn_name = names::checked_add(&size);
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

fn checked_div_unsigned() -> yul::Statement {
    function_definition! {
        function checked_div_unsigned(val1, val2) -> result {
            (if (iszero(val2)) { (revert(0, 0)) })
            (result := div(val1, val2))
        }
    }
}

fn checked_div_signed(size: Integer) -> yul::Statement {
    if !size.is_signed() {
        panic!("Expected signed integer")
    }
    let (min_value, _) = get_min_max(size.clone());
    let fn_name = names::checked_div(&size);
    function_definition! {
        function [fn_name](val1, val2) -> result {
            (if (iszero(val2)) { (revert(0, 0)) })

            // overflow for min_val / -1
            (if (and( (eq(val1, [min_value])), (eq(val2, (sub(0, 1))))) ) { (revert(0, 0)) })
            (result := sdiv(val1, val2))
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
