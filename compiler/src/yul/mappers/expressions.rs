use crate::errors::CompileError;
use crate::yul::namespace::scopes::{ContractDef, FunctionDef, FunctionScope, Scope, Shared};
use crate::yul::namespace::types::{Base, FixedSize};

use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;
use yultsur::*;

/// Builds a Yul expression from a Vyper expression.
pub fn expr<'a>(
    scope: Shared<FunctionScope>,
    expr: &'a vyp::Expr<'a>,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    match expr {
        vyp::Expr::Name(name) => expr_name(scope, name.to_string()),
        vyp::Expr::Num(num) => Ok((literal_expression! {(num)}, FixedSize::Base(Base::U256))),
        vyp::Expr::Subscript { value, slices } => expr_subscript(scope, &value.node, &slices.node),
        _ => Err(CompileError::static_str("Expression not supported")),
    }
}

/// Retrieves the &str value of a name expression
pub fn expr_name_str<'a>(expr: &'a vyp::Expr<'a>) -> Result<&'a str, CompileError> {
    if let vyp::Expr::Name(name) = expr {
        return Ok(*name);
    }

    Err(CompileError::static_str("Not a name expression."))
}

/// Retrieves the &str value of a name expression and converts it to a String
pub fn expr_name_string<'a>(expr: &'a vyp::Expr<'a>) -> Result<String, CompileError> {
    expr_name_str(expr).map(|name| name.to_string())
}

fn expr_name<'a>(
    scope: Shared<FunctionScope>,
    name: String,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    match scope.borrow().def(name.clone()) {
        Some(FunctionDef::Base(base)) => {
            Ok((identifier_expression! {(name)}, FixedSize::Base(base)))
        }
        Some(FunctionDef::Array(array)) => {
            Ok((identifier_expression! {(name)}, FixedSize::Array(array)))
        }
        None => Err(CompileError::static_str("Function definition not found")),
    }
}

fn expr_subscript<'a>(
    scope: Shared<FunctionScope>,
    value: &'a vyp::Expr<'a>,
    slices: &'a Vec<Spanned<vyp::Slice<'a>>>,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    match value {
        vyp::Expr::Attribute { value, attr } => {
            expr_subscript_attribute(scope, &value.node, attr.node.to_string(), slices)
        }
        vyp::Expr::Name(name) => expr_subscript_name(scope, name.to_string(), slices),
        _ => Err(CompileError::static_str(
            "Subscript expression not supported",
        )),
    }
}

fn expr_subscript_name<'a>(
    scope: Shared<FunctionScope>,
    name: String,
    slices: &'a Vec<Spanned<vyp::Slice<'a>>>,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    let (key, _) = slices_index(Rc::clone(&scope), slices)?;

    match scope.borrow().def(name.clone()) {
        Some(FunctionDef::Array(array)) => {
            Ok((array.mload_elem(name, key)?, FixedSize::Base(array.inner)))
        }
        None => Err(CompileError::static_str("Function definition not found")),
        _ => Err(CompileError::static_str(
            "Function definition not supported",
        )),
    }
}

fn expr_subscript_attribute<'a>(
    scope: Shared<FunctionScope>,
    value: &'a vyp::Expr<'a>,
    name: String,
    slices: &'a Vec<Spanned<vyp::Slice<'a>>>,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    let (key, _) = slices_index(Rc::clone(&scope), slices)?;

    match expr_name_str(value)? {
        "self" => expr_subscript_self(scope, name, key),
        _ => Err(CompileError::static_str("Unknown attribute value")),
    }
}

fn expr_subscript_self(
    scope: Shared<FunctionScope>,
    name: String,
    key: yul::Expression,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    match scope.borrow().contract_def(name) {
        Some(ContractDef::Map { index, map }) => Ok((map.sload(index, key)?, map.value)),
        _ => Err(CompileError::static_str(
            "Contract definition not supported",
        )),
    }
}

pub fn slices_index(
    scope: Shared<FunctionScope>,
    slices: &Vec<Spanned<vyp::Slice>>,
) -> Result<(yul::Expression, FixedSize), CompileError> {
    if let Some(first_slice) = slices.first() {
        if let vyp::Slice::Index(index) = &first_slice.node {
            return Ok(expr(scope, index)?);
        }

        return Err(CompileError::static_str("First slice is not an index"));
    }

    return Err(CompileError::static_str("No slices in vector"));
}
