use crate::{db::CodegenDb, yul::YulVariable};

use super::{DefaultRuntimeProvider, RuntimeFunction, RuntimeProvider};

use fe_mir::ir::{TypeId, TypeKind};

use yultsur::*;

pub(super) fn dispatch_safe_add(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    lhs: yul::Expression,
    rhs: yul::Expression,
    ty: TypeId,
) -> yul::Expression {
    debug_assert!(ty.is_integral(db.upcast()));
    let min_value = get_min_value(db, ty);
    let max_value = get_max_value(db, ty);

    if ty.is_signed(db.upcast()) {
        let name = "$safe_add_signed";
        let args = vec![lhs, rhs, min_value, max_value];
        provider.create_then_call(name, args, |provider| {
            make_safe_add_signed(provider, db, name)
        })
    } else {
        let name = "$safe_add_unsigned";
        let args = vec![lhs, rhs, max_value];
        provider.create_then_call(name, args, |provider| {
            make_safe_add_unsigned(provider, db, name)
        })
    }
}

pub(super) fn dispatch_safe_sub(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    lhs: yul::Expression,
    rhs: yul::Expression,
    ty: TypeId,
) -> yul::Expression {
    debug_assert!(ty.is_integral(db.upcast()));
    let min_value = get_min_value(db, ty);
    let max_value = get_max_value(db, ty);

    if ty.is_signed(db.upcast()) {
        let name = "$safe_sub_signed";
        let args = vec![lhs, rhs, min_value, max_value];
        provider.create_then_call(name, args, |provider| {
            make_safe_sub_signed(provider, db, name)
        })
    } else {
        let name = "$safe_sub_unsigned";
        let args = vec![lhs, rhs];
        provider.create_then_call(name, args, |provider| {
            make_safe_sub_unsigned(provider, db, name)
        })
    }
}

pub(super) fn dispatch_safe_mul(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    lhs: yul::Expression,
    rhs: yul::Expression,
    ty: TypeId,
) -> yul::Expression {
    debug_assert!(ty.is_integral(db.upcast()));
    let min_value = get_min_value(db, ty);
    let max_value = get_max_value(db, ty);

    if ty.is_signed(db.upcast()) {
        let name = "$safe_mul_signed";
        let args = vec![lhs, rhs, min_value, max_value];
        provider.create_then_call(name, args, |provider| {
            make_safe_mul_signed(provider, db, name)
        })
    } else {
        let name = "$safe_mul_unsigned";
        let args = vec![lhs, rhs, max_value];
        provider.create_then_call(name, args, |provider| {
            make_safe_mul_unsigned(provider, db, name)
        })
    }
}

pub(super) fn dispatch_safe_div(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    lhs: yul::Expression,
    rhs: yul::Expression,
    ty: TypeId,
) -> yul::Expression {
    debug_assert!(ty.is_integral(db.upcast()));
    let min_value = get_min_value(db, ty);

    if ty.is_signed(db.upcast()) {
        let name = "$safe_div_signed";
        let args = vec![lhs, rhs, min_value];
        provider.create_then_call(name, args, |provider| {
            make_safe_div_signed(provider, db, name)
        })
    } else {
        let name = "$safe_div_unsigned";
        let args = vec![lhs, rhs];
        provider.create_then_call(name, args, |provider| {
            make_safe_div_unsigned(provider, db, name)
        })
    }
}

pub(super) fn dispatch_safe_mod(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    lhs: yul::Expression,
    rhs: yul::Expression,
    ty: TypeId,
) -> yul::Expression {
    debug_assert!(ty.is_integral(db.upcast()));
    if ty.is_signed(db.upcast()) {
        let name = "$safe_mod_signed";
        let args = vec![lhs, rhs];
        provider.create_then_call(name, args, |provider| {
            make_safe_mod_signed(provider, db, name)
        })
    } else {
        let name = "$safe_mod_unsigned";
        let args = vec![lhs, rhs];
        provider.create_then_call(name, args, |provider| {
            make_safe_mod_unsigned(provider, db, name)
        })
    }
}

pub(super) fn dispatch_safe_pow(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    lhs: yul::Expression,
    rhs: yul::Expression,
    ty: TypeId,
) -> yul::Expression {
    debug_assert!(ty.is_integral(db.upcast()));
    let min_value = get_min_value(db, ty);
    let max_value = get_max_value(db, ty);

    if ty.is_signed(db.upcast()) {
        let name = "$safe_pow_signed";
        let args = vec![lhs, rhs, min_value, max_value];
        provider.create_then_call(name, args, |provider| {
            make_safe_pow_signed(provider, db, name)
        })
    } else {
        let name = "$safe_pow_unsigned";
        let args = vec![lhs, rhs, max_value];
        provider.create_then_call(name, args, |provider| {
            make_safe_pow_unsigned(provider, db, name)
        })
    }
}

fn make_safe_add_signed(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let lhs = YulVariable::new("$lhs");
    let rhs = YulVariable::new("$rhs");
    let min_value = YulVariable::new("$min_value");
    let max_value = YulVariable::new("$max_value");
    let ret = YulVariable::new("$ret");

    let func = function_definition! {
        function [func_name.ident()]([lhs.ident()], [rhs.ident()], [min_value.ident()], [max_value.ident()]) -> [ret.ident()] {
            (if (and((iszero((slt([lhs.expr()], 0)))), (sgt([rhs.expr()], (sub([max_value.expr()], [lhs.expr()])))))) { [revert_with_overflow(provider, db)] })
            (if (and((slt([lhs.expr()], 0)), (slt([rhs.expr()], (sub([min_value.expr()], [lhs.expr()])))))) { [revert_with_overflow(provider, db)] })
            ([ret.ident()] := add([lhs.expr()], [rhs.expr()]))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_add_unsigned(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let lhs = YulVariable::new("$lhs");
    let rhs = YulVariable::new("$rhs");
    let max_value = YulVariable::new("$max_value");
    let ret = YulVariable::new("$ret");

    let func = function_definition! {
        function [func_name.ident()]([lhs.ident()], [rhs.ident()], [max_value.ident()]) -> [ret.ident()] {
            (if (gt([lhs.expr()], (sub([max_value.expr()], [rhs.expr()])))) { [revert_with_overflow(provider, db)] })
            ([ret.ident()] := add([lhs.expr()], [rhs.expr()]))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_sub_signed(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let lhs = YulVariable::new("$lhs");
    let rhs = YulVariable::new("$rhs");
    let min_value = YulVariable::new("$min_value");
    let max_value = YulVariable::new("$max_value");
    let ret = YulVariable::new("$ret");

    let func = function_definition! {
        function [func_name.ident()]([lhs.ident()], [rhs.ident()], [min_value.ident()], [max_value.ident()]) -> [ret.ident()] {
            (if (and((iszero((slt([rhs.expr()], 0)))), (slt([lhs.expr()], (add([min_value.expr()], [rhs.expr()])))))) { [revert_with_overflow(provider, db)] })
            (if (and((slt([rhs.expr()], 0)), (sgt([lhs.expr()], (add([max_value.expr()], [rhs.expr()])))))) { [revert_with_overflow(provider, db)] })
            ([ret.ident()] := sub([lhs.expr()], [rhs.expr()]))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_sub_unsigned(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let lhs = YulVariable::new("$lhs");
    let rhs = YulVariable::new("$rhs");
    let ret = YulVariable::new("$ret");

    let func = function_definition! {
        function [func_name.ident()]([lhs.ident()], [rhs.ident()]) -> [ret.ident()] {
            (if (lt([lhs.expr()], [rhs.expr()])) { [revert_with_overflow(provider, db)] })
            ([ret.ident()] := sub([lhs.expr()], [rhs.expr()]))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_mul_signed(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let lhs = YulVariable::new("$lhs");
    let rhs = YulVariable::new("$rhs");
    let min_value = YulVariable::new("$min_value");
    let max_value = YulVariable::new("$max_value");
    let ret = YulVariable::new("$ret");

    let func = function_definition! {
        function [func_name.ident()]([lhs.ident()], [rhs.ident()], [min_value.ident()], [max_value.ident()]) -> [ret.ident()] {
            // overflow, if lhs > 0, rhs > 0 and lhs > (max_value / rhs)
            (if (and((and((sgt([lhs.expr()], 0)), (sgt([rhs.expr()], 0)))), (gt([lhs.expr()], (div([max_value.expr()], [rhs.expr()])))))) { [revert_with_overflow(provider, db)] })
            // underflow, if lhs > 0, rhs < 0 and rhs < (min_value / lhs)
            (if (and((and((sgt([lhs.expr()], 0)), (slt([rhs.expr()], 0)))), (slt([rhs.expr()], (sdiv([min_value.expr()], [lhs.expr()])))))) { [revert_with_overflow(provider, db)] })
            // underflow, if lhs < 0, rhs > 0 and lhs < (min_value / rhs)
            (if (and((and((slt([lhs.expr()], 0)), (sgt([rhs.expr()], 0)))), (slt([lhs.expr()], (sdiv([min_value.expr()], [rhs.expr()])))))) { [revert_with_overflow(provider, db)] })
            // overflow, if lhs < 0, rhs < 0 and lhs < (max_value / rhs)
            (if (and((and((slt([lhs.expr()], 0)), (slt([rhs.expr()], 0)))), (slt([lhs.expr()], (sdiv([max_value.expr()], [rhs.expr()])))))) { [revert_with_overflow(provider, db)] })
            ([ret.ident()] := mul([lhs.expr()], [rhs.expr()]))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_mul_unsigned(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let lhs = YulVariable::new("$lhs");
    let rhs = YulVariable::new("$rhs");
    let max_value = YulVariable::new("$max_value");
    let ret = YulVariable::new("$ret");

    let func = function_definition! {
        function [func_name.ident()]([lhs.ident()], [rhs.ident()], [max_value.ident()]) -> [ret.ident()] {
            // overflow, if lhs != 0 and rhs > (max_value / lhs)
            (if (and((iszero((iszero([lhs.expr()])))), (gt([rhs.expr()], (div([max_value.expr()], [lhs.expr()])))))) { [revert_with_overflow(provider ,db)] })
            ([ret.ident()] := mul([lhs.expr()], [rhs.expr()]))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_div_signed(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let lhs = YulVariable::new("$lhs");
    let rhs = YulVariable::new("$rhs");
    let min_value = YulVariable::new("$min_value");
    let ret = YulVariable::new("$ret");

    let func = function_definition! {
        function [func_name.ident()]([lhs.ident()], [rhs.ident()], [min_value.ident()]) -> [ret.ident()] {
            (if (iszero([rhs.expr()])) { [revert_with_zero_division(provider, db)] })
            (if (and( (eq([lhs.expr()], [min_value.expr()])), (eq([rhs.expr()], (sub(0, 1))))) ) { [revert_with_overflow(provider, db)] })
            ([ret.ident()] := sdiv([lhs.expr()], [rhs.expr()]))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_div_unsigned(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let lhs = YulVariable::new("$lhs");
    let rhs = YulVariable::new("$rhs");
    let ret = YulVariable::new("$ret");

    let func = function_definition! {
        function [func_name.ident()]([lhs.ident()], [rhs.ident()]) -> [ret.ident()] {
            (if (iszero([rhs.expr()])) { [revert_with_zero_division(provider, db)] })
            ([ret.ident()] := div([lhs.expr()], [rhs.expr()]))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_mod_signed(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let lhs = YulVariable::new("$lhs");
    let rhs = YulVariable::new("$rhs");
    let ret = YulVariable::new("$ret");

    let func = function_definition! {
        function [func_name.ident()]([lhs.ident()], [rhs.ident()]) -> [ret.ident()] {
            (if (iszero([rhs.expr()])) { [revert_with_zero_division(provider, db)] })
            ([ret.ident()] := smod([lhs.expr()], [rhs.expr()]))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_mod_unsigned(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let lhs = YulVariable::new("$lhs");
    let rhs = YulVariable::new("$rhs");
    let ret = YulVariable::new("$ret");

    let func = function_definition! {
        function [func_name.ident()]([lhs.ident()], [rhs.ident()]) -> [ret.ident()] {
            (if (iszero([rhs.expr()])) { [revert_with_zero_division(provider, db)] })
            ([ret.ident()] := mod([lhs.expr()], [rhs.expr()]))
        }
    };
    RuntimeFunction::from_statement(func)
}

const SAFE_POW_HELPER_NAME: &str = "safe_pow_helper";

fn make_safe_pow_unsigned(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let base = YulVariable::new("base");
    let exponent = YulVariable::new("exponent");
    let max_value = YulVariable::new("max_value");
    let power = YulVariable::new("power");

    let safe_pow_helper_call = yul::Statement::Assignment(yul::Assignment {
        identifiers: vec![base.ident(), power.ident()],
        expression: {
            let args = vec![
                base.expr(),
                exponent.expr(),
                literal_expression! {1},
                max_value.expr(),
            ];
            provider.create_then_call(SAFE_POW_HELPER_NAME, args, |provider| {
                make_safe_exp_helper(provider, db, SAFE_POW_HELPER_NAME)
            })
        },
    });

    let func = function_definition! {
        function [func_name.ident()]([base.ident()], [exponent.ident()], [max_value.ident()]) -> [power.ident()] {
            // Currently, `leave` avoids this function being inlined.
            // YUL team is working on optimizer improvements to fix that.

            // Note that 0**0 == 1
            (if (iszero([exponent.expr()])) {
                ([power.ident()] := 1 )
                (leave)
            })
            (if (iszero([base.expr()])) {
                ([power.ident()] := 0 )
                (leave)
            })
            // Specializations for small bases
            ([switch! {
                switch [base.expr()]
                // 0 is handled above
                (case 1 {
                    ([power.ident()] := 1 )
                    (leave)
                })
                (case 2 {
                    (if (gt([exponent.expr()], 255)) {
                        [revert_with_overflow(provider, db)]
                    })
                    ([power.ident()] := (exp(2, [exponent.expr()])))
                    (if (gt([power.expr()], [max_value.expr()])) {
                        [revert_with_overflow(provider, db)]
                    })
                    (leave)
                })
            }])
            (if (and((sgt([power.expr()], 0)), (gt([power.expr()], (div([max_value.expr()], [base.expr()])))))) { [revert_with_overflow(provider, db)] })

            (if (or((and((lt([base.expr()], 11)), (lt([exponent.expr()], 78)))), (and((lt([base.expr()], 307)), (lt([exponent.expr()], 32)))))) {
                ([power.ident()] := (exp([base.expr()], [exponent.expr()])))
                (if (gt([power.expr()], [max_value.expr()])) {
                    [revert_with_overflow(provider, db)]
                })
                (leave)
            })

            ([safe_pow_helper_call])
            (if (gt([power.expr()], (div([max_value.expr()], [base.expr()])))) {
                [revert_with_overflow(provider, db)]
            })
            ([power.ident()] := (mul([power.expr()], [base.expr()])))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_pow_signed(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let base = YulVariable::new("base");
    let exponent = YulVariable::new("exponent");
    let min_value = YulVariable::new("min_value");
    let max_value = YulVariable::new("max_value");
    let power = YulVariable::new("power");

    let safe_pow_helper_call = yul::Statement::Assignment(yul::Assignment {
        identifiers: vec![base.ident(), power.ident()],
        expression: {
            let args = vec![base.expr(), exponent.expr(), power.expr(), max_value.expr()];
            provider.create_then_call(SAFE_POW_HELPER_NAME, args, |provider| {
                make_safe_exp_helper(provider, db, SAFE_POW_HELPER_NAME)
            })
        },
    });

    let func = function_definition! {
        function [func_name.ident()]([base.ident()], [exponent.ident()], [min_value.ident()], [max_value.ident()]) -> [power.ident()] {
            // Currently, `leave` avoids this function being inlined.
            // YUL team is working on optimizer improvements to fix that.

            // Note that 0**0 == 1
            ([switch! {
                switch [exponent.expr()]
                (case 0 {
                    ([power.ident()] := 1 )
                    (leave)
                })
                (case 1 {
                    ([power.ident()] := [base.expr()] )
                    (leave)
                })
            }])
            (if (iszero([base.expr()])) {
                ([power.ident()] := 0 )
                (leave)
            })
            ([power.ident()] := 1 )
            // We pull out the first iteration because it is the only one in which
            // base can be negative.
            // Exponent is at least 2 here.
            // overflow check for base * base
            ([switch! {
                switch (sgt([base.expr()], 0))
                (case 1 {
                    (if (gt([base.expr()], (div([max_value.expr()], [base.expr()])))) {
                        [revert_with_overflow(provider, db)]
                    })
                })
                (case 0 {
                    (if (slt([base.expr()], (sdiv([max_value.expr()], [base.expr()])))) {
                        [revert_with_overflow(provider, db)]
                    })
                })
            }])
            (if (and([exponent.expr()], 1)) {
                ([power.ident()] := [base.expr()] )
            })
            ([base.ident()] := (mul([base.expr()], [base.expr()])))
            ([exponent.ident()] := shr(1, [exponent.expr()]))
            // // Below this point, base is always positive.
            ([safe_pow_helper_call]) // power = 1, base = 16 which is wrong
            (if (and((sgt([power.expr()], 0)), (gt([power.expr()], (div([max_value.expr()], [base.expr()])))))) { [revert_with_overflow(provider , db)] })
            (if (and((slt([power.expr()], 0)), (slt([power.expr()], (sdiv([min_value.expr()], [base.expr()])))))) { [revert_with_overflow(provider, db)] })
            ([power.ident()] := (mul([power.expr()], [base.expr()])))
        }
    };
    RuntimeFunction::from_statement(func)
}

fn make_safe_exp_helper(
    provider: &mut DefaultRuntimeProvider,
    db: &dyn CodegenDb,
    func_name: &str,
) -> RuntimeFunction {
    let func_name = YulVariable::new(func_name);
    let base = YulVariable::new("base");
    let exponent = YulVariable::new("exponent");
    let power = YulVariable::new("power");
    let max_value = YulVariable::new("max_value");
    let ret_power = YulVariable::new("ret_power");
    let ret_base = YulVariable::new("ret_base");

    let func = function_definition! {
        function [func_name.ident()]([base.ident()], [exponent.ident()], [power.ident()], [max_value.ident()])
            -> [(vec![ret_base.ident(), ret_power.ident()])...] {
            ([ret_base.ident()] := [base.expr()])
            ([ret_power.ident()] := [power.expr()])
            (for {} (gt([exponent.expr()], 1)) {}
                {
                    // overflow check for base * base
                    (if (gt([ret_base.expr()], (div([max_value.expr()], [ret_base.expr()])))) { [revert_with_overflow(provider, db)] })
                    (if (and([exponent.expr()], 1)) {
                        // No checks for power := mul(power, base) needed, because the check
                        // for base * base above is sufficient, since:
                        // |power| <= base (proof by induction) and thus:
                        // |power * base| <= base * base <= max <= |min| (for signed)
                        // (this is equally true for signed and unsigned exp)
                        ([ret_power.ident()] := (mul([ret_power.expr()], [ret_base.expr()])))
                    })
                    ([ret_base.ident()] := mul([ret_base.expr()], [ret_base.expr()]))
                    ([exponent.ident()] := shr(1, [exponent.expr()]))
                })
        }
    };
    RuntimeFunction::from_statement(func)
}

fn revert_with_overflow(provider: &mut dyn RuntimeProvider, db: &dyn CodegenDb) -> yul::Statement {
    const PANIC_OVERFLOW: usize = 0x11;

    super::panic_revert_numeric(provider, db, literal_expression! {(PANIC_OVERFLOW)})
}

fn revert_with_zero_division(
    provider: &mut dyn RuntimeProvider,
    db: &dyn CodegenDb,
) -> yul::Statement {
    pub const PANIC_ZERO_DIVISION: usize = 0x12;

    super::panic_revert_numeric(provider, db, literal_expression! {(PANIC_ZERO_DIVISION)})
}

fn get_max_value(db: &dyn CodegenDb, ty: TypeId) -> yul::Expression {
    match &ty.data(db.upcast()).kind {
        TypeKind::I8 => literal_expression! {0x7f},
        TypeKind::I16 => literal_expression! {0x7fff},
        TypeKind::I32 => literal_expression! {0x7fffffff},
        TypeKind::I64 => literal_expression! {0x7fffffffffffffff},
        TypeKind::I128 => literal_expression! {0x7fffffffffffffffffffffffffffffff},
        TypeKind::I256 => {
            literal_expression! {0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff}
        }
        TypeKind::U8 => literal_expression! {0xff},
        TypeKind::U16 => literal_expression! {0xffff},
        TypeKind::U32 => literal_expression! {0xffffffff},
        TypeKind::U64 => literal_expression! {0xffffffffffffffff},
        TypeKind::U128 => literal_expression! {0xffffffffffffffffffffffffffffffff},
        TypeKind::U256 => {
            literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff}
        }
        _ => unreachable!(),
    }
}

fn get_min_value(db: &dyn CodegenDb, ty: TypeId) -> yul::Expression {
    debug_assert! {ty.is_integral(db.upcast())};

    match &ty.data(db.upcast()).kind {
        TypeKind::I8 => {
            literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff80}
        }
        TypeKind::I16 => {
            literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8000}
        }
        TypeKind::I32 => {
            literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffffffffffff80000000}
        }
        TypeKind::I64 => {
            literal_expression! {0xffffffffffffffffffffffffffffffffffffffffffffffff8000000000000000}
        }
        TypeKind::I128 => {
            literal_expression! {0xffffffffffffffffffffffffffffffff80000000000000000000000000000000}
        }
        TypeKind::I256 => {
            literal_expression! {0x8000000000000000000000000000000000000000000000000000000000000000}
        }

        _ => literal_expression! {0x0},
    }
}
