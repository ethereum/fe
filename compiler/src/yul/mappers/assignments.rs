use crate::errors::CompileError;
use crate::yul::mappers::{expressions, types};
use crate::yul::namespace::scopes::{
    ContractDef, ContractScope, FunctionDef, FunctionScope, Scope, Shared,
};
use crate::yul::namespace::types::FixedSize;

use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;
use yultsur::*;

pub fn assign<'a>(
    scope: Shared<FunctionScope>,
    targets: &'a Vec<Spanned<vyp::Expr>>,
    value: &'a vyp::Expr<'a>,
) -> Result<yul::Statement, CompileError> {
    let targets = targets
        .iter()
        .map(|t| &t.node)
        .collect::<Vec<&'a vyp::Expr<'a>>>();

    match targets.first() {
        Some(vyp::Expr::Name(name)) => assign_name(scope, name.to_string(), value),
        Some(vyp::Expr::Subscript {
            value: target,
            slices,
        }) => assign_subscript(scope, &target.node, &slices.node, value),
        _ => Err(CompileError::static_str("Targets not supported")),
    }
}

fn assign_subscript<'a>(
    scope: Shared<FunctionScope>,
    target: &'a vyp::Expr<'a>,
    slices: &'a Vec<Spanned<vyp::Slice<'a>>>,
    value: &'a vyp::Expr<'a>,
) -> Result<yul::Statement, CompileError> {
    let name = expressions::expr_name_string(target)?;
    let (key, _) = expressions::slices_index(Rc::clone(&scope), slices)?;
    let (value, _) = expressions::expr(Rc::clone(&scope), value)?;

    match scope.borrow().def(name.clone()) {
        Some(FunctionDef::Array(array)) => array.mstore_elem(name, key, value),
        None => Err(CompileError::static_str("No definition found")),
        _ => Err(CompileError::static_str(
            "Definition not supported in subscript assignment",
        )),
    }
}

fn assign_name<'a>(
    scope: Shared<FunctionScope>,
    name: String,
    value: &'a vyp::Expr<'a>,
) -> Result<yul::Statement, CompileError> {
    let identifier = identifier! {(name)};
    let (value, value_type) = expressions::expr(Rc::clone(&scope), value)?;

    match value_type {
        FixedSize::Array(array) => scope.borrow_mut().add_array(name, array),
        FixedSize::Base(base) => scope.borrow_mut().add_base(name, base),
    };

    Ok(statement! { [identifier] := [value] })
}
