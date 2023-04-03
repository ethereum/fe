//! This module provides evaluator for constant expression to resolve const
//! generics.

use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

use crate::{
    context::{AnalyzerContext, Constant},
    errors::ConstEvalError,
    namespace::types::{self, Base, Type},
};

use fe_common::{numeric, Span};
use fe_parser::{
    ast::{self, BinOperator, BoolOperator, CompOperator, UnaryOperator},
    node::Node,
};

/// Evaluate expression.
///
/// # Panics
///
/// 1. Panics if type analysis on an `expr` is not performed beforehand.
/// 2. Panics if an `expr` has an invalid type.
pub(crate) fn eval_expr(
    context: &mut dyn AnalyzerContext,
    expr: &Node<ast::Expr>,
) -> Result<Constant, ConstEvalError> {
    let typ = context.expr_typ(expr);

    match &expr.kind {
        ast::Expr::Ternary {
            if_expr,
            test,
            else_expr,
        } => eval_ternary(context, if_expr, test, else_expr),
        ast::Expr::BoolOperation { left, op, right } => eval_bool_op(context, left, op, right),
        ast::Expr::BinOperation { left, op, right } => eval_bin_op(context, left, op, right, &typ),
        ast::Expr::UnaryOperation { op, operand } => eval_unary_op(context, op, operand),
        ast::Expr::CompOperation { left, op, right } => eval_comp_op(context, left, op, right),
        ast::Expr::Bool(val) => Ok(Constant::Bool(*val)),
        ast::Expr::Name(name) => match context.constant_value_by_name(name, expr.span)? {
            Some(const_value) => Ok(const_value),
            _ => Err(not_const_error(context, expr.span)),
        },

        ast::Expr::Num(num) => {
            // We don't validate the string representing number here,
            // because we assume the string has been already validate in type analysis.
            let span = expr.span;
            Constant::from_num_str(context, num, &typ, span)
        }

        ast::Expr::Str(s) => Ok(Constant::Str(s.clone())),

        // TODO: Need to evaluate attribute getter, constant constructor and const fn call.
        ast::Expr::Subscript { .. }
        | ast::Expr::Path(_)
        | ast::Expr::Attribute { .. }
        | ast::Expr::Call { .. }
        | ast::Expr::List { .. }
        | ast::Expr::Repeat { .. }
        | ast::Expr::Tuple { .. }
        | ast::Expr::Unit => Err(not_const_error(context, expr.span)),
    }
}

/// Evaluates ternary expression.
fn eval_ternary(
    context: &mut dyn AnalyzerContext,
    then_expr: &Node<ast::Expr>,
    cond: &Node<ast::Expr>,
    else_expr: &Node<ast::Expr>,
) -> Result<Constant, ConstEvalError> {
    // In constant evaluation, we don't apply short circuit property for safety.
    let then = eval_expr(context, then_expr)?;
    let cond = eval_expr(context, cond)?;
    let else_ = eval_expr(context, else_expr)?;

    match cond {
        Constant::Bool(cond) => {
            if cond {
                Ok(then)
            } else {
                Ok(else_)
            }
        }
        _ => panic!("ternary condition is not a bool type"),
    }
}

/// Evaluates logical expressions.
fn eval_bool_op(
    context: &mut dyn AnalyzerContext,
    lhs: &Node<ast::Expr>,
    op: &Node<ast::BoolOperator>,
    rhs: &Node<ast::Expr>,
) -> Result<Constant, ConstEvalError> {
    // In constant evaluation, we don't apply short circuit property for safety.
    let (lhs, rhs) = (eval_expr(context, lhs)?, eval_expr(context, rhs)?);
    let (lhs, rhs) = match (lhs, rhs) {
        (Constant::Bool(lhs), Constant::Bool(rhs)) => (lhs, rhs),
        _ => panic!("an argument of a logical expression is not bool type"),
    };

    match op.kind {
        BoolOperator::And => Ok(Constant::Bool(lhs && rhs)),
        BoolOperator::Or => Ok(Constant::Bool(lhs || rhs)),
    }
}

/// Evaluates binary expressions.
fn eval_bin_op(
    context: &mut dyn AnalyzerContext,
    lhs: &Node<ast::Expr>,
    op: &Node<ast::BinOperator>,
    rhs: &Node<ast::Expr>,
    typ: &Type,
) -> Result<Constant, ConstEvalError> {
    let span = lhs.span + rhs.span;
    let lhs_ty = extract_int_typ(&context.expr_typ(lhs));

    let (lhs, rhs) = (eval_expr(context, lhs)?, eval_expr(context, rhs)?);
    let (lhs, rhs) = (lhs.extract_numeric(), rhs.extract_numeric());

    let result = match op.kind {
        BinOperator::Add => lhs + rhs,
        BinOperator::Sub => lhs - rhs,
        BinOperator::Mult => lhs * rhs,

        BinOperator::Div => {
            if rhs.is_zero() {
                return Err(zero_division_error(context, span));
            } else if lhs_ty.is_signed() && lhs == &(lhs_ty.min_value()) && rhs == &(-BigInt::one())
            {
                return Err(overflow_error(context, span));
            } else {
                lhs / rhs
            }
        }

        BinOperator::Mod => {
            if rhs.is_zero() {
                return Err(zero_division_error(context, span));
            }
            lhs % rhs
        }

        BinOperator::Pow => {
            // We assume `rhs` type is unsigned numeric.
            if let Some(exponent) = rhs.to_u32() {
                lhs.pow(exponent)
            } else if lhs.is_zero() {
                BigInt::zero()
            } else if lhs.is_one() {
                BigInt::one()
            } else {
                // Exponent is larger than u32::MAX and lhs is not zero nor one,
                // then this trivially causes overflow.
                return Err(overflow_error(context, span));
            }
        }

        BinOperator::LShift => {
            if let Some(exponent) = rhs.to_usize() {
                let type_bits = lhs_ty.bits();
                // If rhs is larger than or equal to lhs type bits, then we emits overflow
                // error.
                if exponent >= type_bits {
                    return Err(overflow_error(context, span));
                } else {
                    let mask = make_mask(typ);
                    (lhs * BigInt::from(2_u8).pow(exponent as u32)) & mask
                }
            } else {
                // If exponent is larger than usize::MAX, it causes trivially overflow.
                return Err(overflow_error(context, span));
            }
        }

        BinOperator::RShift => {
            if let Some(exponent) = rhs.to_usize() {
                let type_bits = lhs_ty.bits();
                // If rhs is larger than or equal to lhs type bits, then we emits overflow
                // error.
                if exponent >= type_bits {
                    return Err(overflow_error(context, span));
                } else {
                    let mask = make_mask(typ);
                    (lhs / BigInt::from(2_u8).pow(exponent as u32)) & mask
                }
            } else {
                // If exponent is larger than usize::MAX, it causes trivially overflow.
                return Err(overflow_error(context, span));
            }
        }

        BinOperator::BitOr => lhs | rhs,
        BinOperator::BitXor => lhs ^ rhs,
        BinOperator::BitAnd => lhs & rhs,
    };

    Constant::make_const_numeric_with_ty(context, result, typ, span)
}

fn eval_unary_op(
    context: &mut dyn AnalyzerContext,
    op: &Node<ast::UnaryOperator>,
    arg: &Node<ast::Expr>,
) -> Result<Constant, ConstEvalError> {
    let arg = eval_expr(context, arg)?;

    match op.kind {
        UnaryOperator::Invert => Ok(Constant::Int(!arg.extract_numeric())),
        UnaryOperator::Not => Ok(Constant::Bool(!arg.extract_bool())),
        UnaryOperator::USub => Ok(Constant::Int(-arg.extract_numeric())),
    }
}

/// Evaluates comp operation.
fn eval_comp_op(
    context: &mut dyn AnalyzerContext,
    lhs: &Node<ast::Expr>,
    op: &Node<ast::CompOperator>,
    rhs: &Node<ast::Expr>,
) -> Result<Constant, ConstEvalError> {
    let (lhs, rhs) = (eval_expr(context, lhs)?, eval_expr(context, rhs)?);

    let res = match (lhs, rhs) {
        (Constant::Int(lhs), Constant::Int(rhs)) => match op.kind {
            CompOperator::Eq => lhs == rhs,
            CompOperator::NotEq => lhs != rhs,
            CompOperator::Lt => lhs < rhs,
            CompOperator::LtE => lhs <= rhs,
            CompOperator::Gt => lhs > rhs,
            CompOperator::GtE => lhs >= rhs,
        },

        (Constant::Bool(lhs), Constant::Bool(rhs)) => match op.kind {
            CompOperator::Eq => lhs == rhs,
            CompOperator::NotEq => lhs != rhs,
            CompOperator::Lt => !lhs & rhs,
            CompOperator::LtE => lhs <= rhs,
            CompOperator::Gt => lhs & !rhs,
            CompOperator::GtE => lhs >= rhs,
        },

        _ => panic!("arguments of comp op have invalid type"),
    };

    Ok(Constant::Bool(res))
}

impl Constant {
    /// Returns constant from numeric literal represented by string.
    ///
    /// # Panics
    /// Panics if `s` is invalid string for numeric literal.
    pub fn from_num_str(
        context: &mut dyn AnalyzerContext,
        s: &str,
        typ: &Type,
        span: Span,
    ) -> Result<Self, ConstEvalError> {
        let literal = numeric::Literal::new(s);
        let num = literal.parse::<BigInt>().unwrap();
        match typ {
            Type::Base(Base::Numeric(_)) => {
                Self::make_const_numeric_with_ty(context, num, typ, span)
            }
            Type::Base(Base::Address) => {
                if num >= BigInt::zero() && num <= types::address_max() {
                    Ok(Constant::Address(num))
                } else {
                    Err(overflow_error(context, span))
                }
            }
            _ => unreachable!(),
        }
    }

    /// Returns constant from numeric literal that fits type bits.
    /// If `val` doesn't fit type bits, then return `Err`.
    ///
    /// # Panics
    /// Panics if `typ` is invalid string for numeric literal.
    fn make_const_numeric_with_ty(
        context: &mut dyn AnalyzerContext,
        val: BigInt,
        typ: &Type,
        span: Span,
    ) -> Result<Self, ConstEvalError> {
        // Overflowing check.
        if extract_int_typ(typ).fits(val.clone()) {
            Ok(Constant::Int(val))
        } else {
            Err(overflow_error(context, span))
        }
    }

    /// Extracts numeric value from a `Constant`.
    ///
    /// # Panics
    /// Panics if a `self` variant is not a numeric.
    fn extract_numeric(&self) -> &BigInt {
        match self {
            Constant::Int(val) => val,
            _ => panic!("can't extract numeric value from {self:?}"),
        }
    }

    /// Extracts bool value from a `Constant`.
    ///
    /// # Panics
    /// Panics if a `self` variant is not a bool.
    fn extract_bool(&self) -> bool {
        match self {
            Constant::Bool(val) => *val,
            _ => panic!("can't extract bool value from {self:?}"),
        }
    }
}

fn not_const_error(context: &mut dyn AnalyzerContext, span: Span) -> ConstEvalError {
    ConstEvalError::new(context.error(
        "expression is not a constant",
        span,
        "expression is required to be constant here",
    ))
}

fn overflow_error(context: &mut dyn AnalyzerContext, span: Span) -> ConstEvalError {
    ConstEvalError::new(context.error(
        "overflow error",
        span,
        "overflow occurred during constant evaluation",
    ))
}

fn zero_division_error(context: &mut dyn AnalyzerContext, span: Span) -> ConstEvalError {
    ConstEvalError::new(context.error(
        "zero division error",
        span,
        "zero division occurred during constant evaluation",
    ))
}

/// Returns integer types embedded in `typ`.
///
/// # Panic
/// Panics if `typ` is not a numeric type.
fn extract_int_typ(typ: &Type) -> types::Integer {
    match typ {
        Type::Base(Base::Numeric(int_ty)) => *int_ty,
        _ => {
            panic!("invalid binop expression type")
        }
    }
}

/// Returns bit mask corresponding to typ.
/// e.g. If type is `Type::Base(Base::Numeric(Integer::I32))`, then returns
/// `0xffff_ffff`.
///
/// # Panic
/// Panics if `typ` is not a numeric type.
fn make_mask(typ: &Type) -> BigInt {
    let bits = extract_int_typ(typ).bits();
    (BigInt::one() << bits) - 1
}
