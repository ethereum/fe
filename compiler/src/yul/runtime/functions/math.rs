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

/// Return a vector of runtime functions for exponentiation with over-/underflow
/// protection
pub fn checked_exp_fns() -> Vec<yul::Statement> {
    vec![
        checked_exp_unsigned(),
        checked_exp_signed(),
        checked_exp_helper(),
        _checked_exp_unsigned(Integer::U256),
        _checked_exp_unsigned(Integer::U128),
        _checked_exp_unsigned(Integer::U64),
        _checked_exp_unsigned(Integer::U32),
        _checked_exp_unsigned(Integer::U16),
        _checked_exp_unsigned(Integer::U8),
        _checked_exp_signed(Integer::I256),
        _checked_exp_signed(Integer::I128),
        _checked_exp_signed(Integer::I64),
        _checked_exp_signed(Integer::I32),
        _checked_exp_signed(Integer::I16),
        _checked_exp_signed(Integer::I8),
    ]
}

/// Return a vector of runtime functions for checked modulo arithmetic
pub fn checked_mod_fns() -> Vec<yul::Statement> {
    vec![checked_mod_unsigned(), checked_mod_signed()]
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
        checked_exp_fns(),
        checked_mod_fns(),
        checked_mul_fns(),
        checked_sub_fns(),
    ]
    .concat()
}

fn checked_mod_unsigned() -> yul::Statement {
    function_definition! {
        function checked_mod_unsigned(val1, val2) -> result {
            (if (iszero(val2)) { (revert(0, 0)) })
            (result := mod(val1, val2))
        }
    }
}

fn checked_mod_signed() -> yul::Statement {
    function_definition! {
        function checked_mod_signed(val1, val2) -> result {
            (if (iszero(val2)) { (revert(0, 0)) })
            (result := smod(val1, val2))
        }
    }
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

fn _checked_exp_signed(size: Integer) -> yul::Statement {
    if !size.is_signed() {
        panic!("Expected signed integer")
    }
    let (min_value, max_value) = get_min_max(size.clone());
    let fn_name = names::checked_exp(&size);
    function_definition! {
        function [fn_name](base, exponent) -> power {
            (power := checked_exp_signed(base, exponent, [min_value], [max_value]))
        }
    }
}

fn _checked_exp_unsigned(size: Integer) -> yul::Statement {
    if size.is_signed() {
        panic!("Expected unsigned integer")
    }
    let (_, max_value) = get_min_max(size.clone());
    let fn_name = names::checked_exp(&size);
    function_definition! {
        function [fn_name](base, exponent) -> power {
            (power := checked_exp_unsigned(base, exponent, [max_value]))
        }
    }
}

fn checked_exp_helper() -> yul::Statement {
    // TODO: Refactor once https://github.com/ethereum/fe/issues/314 is resolved
    yul::Statement::FunctionDefinition(yul::FunctionDefinition {
        name: identifier! { ("checked_exp_helper") },
        parameters: identifiers! { ("_power") ("_base") ("exponent") ("max")},
        returns: identifiers! { ("power") ("base")},
        block: block! {
            (power := _power)
            (base := _base)
            (for {} (gt(exponent, 1)) {}
            {
                // overflow check for base * base
                (if (gt(base, (div(max, base)))) { (revert(0, 0)) })
                (if (and(exponent, 1)) {
                    // No checks for power := mul(power, base) needed, because the check
                    // for base * base above is sufficient, since:
                    // |power| <= base (proof by induction) and thus:
                    // |power * base| <= base * base <= max <= |min| (for signed)
                    // (this is equally true for signed and unsigned exp)
                    (power := (mul(power, base)))
                })
                (base := (mul(base, base)))
                (exponent := shr(1, exponent))
            })
        },
    })
}

fn checked_exp_signed() -> yul::Statement {
    // Refactor once https://github.com/ethereum/fe/issues/314 is resolved
    let checked_exp_helper_call = yul::Statement::Assignment(yul::Assignment {
        identifiers: identifiers! { ("power") ("base")},
        expression: expression! { checked_exp_helper(power, base, exponent, max) },
    });

    function_definition! {
        function checked_exp_signed(base, exponent, min, max) -> power {
            // Currently, `leave` avoids this function being inlined.
            // YUL team is working on optimizer improvements to fix that.

            // Note that 0**0 == 1
            ([switch! {
                switch exponent
                (case 0 {
                    (power := 1 )
                    (leave)
                })
                (case 1 {
                    (power := base )
                    (leave)
                })
            }])
            (if (iszero(base)) {
                (power := 0 )
                (leave)
            })
            (power := 1 )
            // We pull out the first iteration because it is the only one in which
            // base can be negative.
            // Exponent is at least 2 here.
            // overflow check for base * base
            ([switch! {
                switch (sgt(base, 0))
                (case 1 {
                    (if (gt(base, (div(max, base)))) {
                        (revert(0, 0))
                    })
                })
                (case 0 {
                    (if (slt(base, (sdiv(max, base)))) {
                        (revert(0, 0))
                    })
                })
            }])
            (if (and(exponent, 1)) {
                (power := base )
            })
            (base := (mul(base, base)))
            (exponent := shr(1, exponent))
            // // Below this point, base is always positive.
            ([checked_exp_helper_call]) // power = 1, base = 16 which is wrong
            (if (and((sgt(power, 0)), (gt(power, (div(max, base)))))) { (revert(0, 0)) })
            (if (and((slt(power, 0)), (slt(power, (sdiv(min, base)))))) { (revert(0, 0)) })
            (power := (mul(power, base)))
        }
    }
}

fn checked_exp_unsigned() -> yul::Statement {
    // TODO: Refactor once https://github.com/ethereum/fe/issues/314 is resolved
    let checked_exp_helper_call = yul::Statement::Assignment(yul::Assignment {
        identifiers: identifiers! { ("power") ("base")},
        expression: expression! { checked_exp_helper(1, base, exponent, max) },
    });

    function_definition! {
        function checked_exp_unsigned(base, exponent, max) -> power {
            // Currently, `leave` avoids this function being inlined.
            // YUL team is working on optimizer improvements to fix that.

            // Note that 0**0 == 1
            (if (iszero(exponent)) {
                (power := 1 )
                (leave)
            })
            (if (iszero(base)) {
                (power := 0 )
                (leave)
            })
            // Specializations for small bases
            ([switch! {
                switch base
                // 0 is handled above
                (case 1 {
                    (power := 1 )
                    (leave)
                })
                (case 2 {
                    (if (gt(exponent, 255)) {
                        (revert(0, 0))
                    })
                    (power := (exp(2, exponent)))
                    (if (gt(power, max)) {
                        (revert(0, 0))
                    })
                    (leave)
                })
            }])
            (if (and((sgt(power, 0)), (gt(power, (div(max, base)))))) { (revert(0, 0)) })

            (if (or((and((lt(base, 11)), (lt(exponent, 78)))), (and((lt(base, 307)), (lt(exponent, 32)))))) {
                (power := (exp(base, exponent)))
                (if (gt(power, max)) {
                    (revert(0, 0))
                })
                (leave)
            })

            ([checked_exp_helper_call])
            (if (gt(power, (div(max, base)))) {
                (revert(0, 0))
            })
            (power := (mul(power, base)))
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
