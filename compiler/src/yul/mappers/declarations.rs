use crate::errors::CompileError;
use crate::yul::mappers::expressions::{expr, expr_name_string};
use crate::yul::mappers::types;
use crate::yul::namespace::scopes::{
    ContractDef, ContractScope, FunctionDef, FunctionScope, Scope, Shared,
};
use crate::yul::namespace::types::FixedSize;

use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;
use yultsur::*;

pub fn var_decl<'a>(
    scope: Shared<FunctionScope>,
    target: &'a vyp::Expr<'a>,
    typ: &'a vyp::TypeDesc<'a>,
    value: &'a Option<Spanned<vyp::Expr>>,
) -> Result<yul::Statement, CompileError> {
    match typ {
        vyp::TypeDesc::Base { .. } => var_decl_base(scope, target, typ, value),
        vyp::TypeDesc::Array { typ: _, dimension } => var_decl_array(scope, target, typ, value),
        vyp::TypeDesc::Map { .. } => {
            Err(CompileError::static_str("Cannot declare map in function"))
        }
    }
}

fn var_decl_base<'a>(
    scope: Shared<FunctionScope>,
    target: &'a vyp::Expr<'a>,
    typ: &'a vyp::TypeDesc<'a>,
    value: &'a Option<Spanned<vyp::Expr>>,
) -> Result<yul::Statement, CompileError> {
    let name = expr_name_string(target)?;
    let typ = types::type_desc_base(Scope::Function(Rc::clone(&scope)), typ)?;

    scope.borrow_mut().add_base(name.clone(), typ);

    if let Some(value) = value {
        let (value, _) = expr(scope, &value.node)?;
        Ok(statement! { let [identifier! {(name)}] := [value] })
    } else {
        Ok(statement! { let [identifier! {(name)}] := 0 })
    }
}

fn var_decl_array<'a>(
    scope: Shared<FunctionScope>,
    target: &'a vyp::Expr<'a>,
    typ: &'a vyp::TypeDesc<'a>,
    value: &'a Option<Spanned<vyp::Expr>>,
) -> Result<yul::Statement, CompileError> {
    let name = expr_name_string(target)?;
    let typ = types::type_desc_array(Scope::Function(Rc::clone(&scope)), typ)?;
    let size = literal_expression! {(typ.size())};

    scope.borrow_mut().add_array(name.clone(), typ);

    if let Some(value) = value {
        Err(CompileError::static_str("Array copying not supported yet"))
    } else {
        Ok(statement! { let [identifier! {(name)}] := alloc([size]) })
    }
}
