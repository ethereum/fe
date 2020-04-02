use crate::errors::CompileError;
use crate::yul::mappers::{assignments, declarations, expressions, types};
use crate::yul::namespace::scopes::{
    ContractDef, ContractScope, FunctionScope, Scope, Shared,
};
use crate::yul::namespace::types::FixedSize;
use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;
use yultsur::*;

/// Builds a Yul function definition from a Vyper function definition.
pub fn func_def<'a>(
    contract_scope: Shared<ContractScope>,
    _qual: &'a Option<Spanned<vyp::FuncQual>>,
    name: String,
    args: &'a Vec<Spanned<vyp::FuncDefArg<'a>>>,
    return_type: &'a Option<Spanned<vyp::TypeDesc>>,
    body: &'a Vec<Spanned<vyp::FuncStmt>>,
) -> Result<yul::Statement, CompileError> {
    // Create a function scope that points to the parent.
    let function_scope = FunctionScope::new(Rc::clone(&contract_scope));

    // Map function args to Yul identifiers and `FixedSize` types.
    // Parameters are added to `function_scope` as scope variable definitions
    // in `func_def_arg`.
    let mut param_names = vec![];
    let mut param_types = vec![];
    for arg in args {
        let (name, typ) = func_def_arg(Rc::clone(&function_scope), &arg.node)?;
        param_names.push(name);
        param_types.push(typ);
    }

    // Map the return value to a `FixedSize` type.
    let return_type = if let Some(return_type) = return_type {
        Some(types::type_desc_fixed_size(
            Scope::Function(Rc::clone(&function_scope)),
            &return_type.node,
        )?)
    } else {
        None
    };

    // Add this function to the contract scope. This is used to generate
    // the ABI and assists in declaring variables in other functions.
    contract_scope
        .borrow_mut()
        .add_function(name.clone(), param_types, return_type.clone());

    // Map all Vyper statements to Yul statements.
    let function_name = identifier! {(name)};
    let function_statements = body
        .iter()
        .map(|stmt| func_stmt(Rc::clone(&function_scope), &stmt.node))
        .collect::<Result<Vec<yul::Statement>, CompileError>>()?;

    // Different return types require slightly different functions.
    if let Some(return_type) = return_type {
        match return_type {
            // Base types are returned by value. All memory used by the function is cleared.
            FixedSize::Base(_) => Ok(function_definition! {
                function [function_name]([param_names...]) -> return_val {
                    (let ptr := avail())
                    [function_statements...]
                    (free(ptr))
                }
            }),
            // Arrays need to keep memory allocated after completing.
            // FIXME: Copy arrays to the lowest available pointer to save memory.
            FixedSize::Array(array) => {
                let size = literal_expression! {(array.size())};

                Ok(function_definition! {
                    function [function_name]([param_names...]) -> return_val {
                        [function_statements...]
                        (free((add(return_val, [size]))))
                    }
                })
            }
        }
    } else {
        // Nothing is returned. All memory used by the function is freed.
        Ok(function_definition! {
            function [function_name]([param_names...]) {
                (let ptr := avail())
                [function_statements...]
                (free(ptr))
            }
        })
    }
}

fn func_def_arg<'a>(
    scope: Shared<FunctionScope>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> Result<(yul::Identifier, FixedSize), CompileError> {
    let name = arg.name.node.to_string();
    let typ = types::type_desc_fixed_size(Scope::Function(Rc::clone(&scope)), &arg.typ.node)?;

    match typ.clone() {
        FixedSize::Base(base) => scope.borrow_mut().add_base(name.clone(), base),
        FixedSize::Array(array) => scope.borrow_mut().add_array(name.clone(), array),
    }

    Ok((identifier! {(name)}, typ))
}

fn func_stmt<'a>(
    scope: Shared<FunctionScope>,
    stmt: &'a vyp::FuncStmt<'a>,
) -> Result<yul::Statement, CompileError> {
    match stmt {
        vyp::FuncStmt::Return { value } => func_return(scope, value),
        vyp::FuncStmt::VarDecl { target, typ, value } => {
            declarations::var_decl(scope, &target.node, &typ.node, value)
        }
        vyp::FuncStmt::Assign { targets, value } => {
            assignments::assign(scope, targets, &value.node)
        }
        vyp::FuncStmt::Emit { value } => emit(scope, &value.node),
        _ => unimplemented!("Function statement"),
    }
}

fn emit(scope: Shared<FunctionScope>, value: &vyp::Expr) -> Result<yul::Statement, CompileError> {
    if let vyp::Expr::Call { func, args } = value {
        let event_name = expressions::expr_name_string(&func.node)?;
        let event_values = args
            .node
            .iter()
            .map(|a| call_arg(Rc::clone(&scope), &a.node))
            .collect::<Result<Vec<yul::Expression>, CompileError>>()?;

        if let Some(ContractDef::Event(event)) = scope.borrow().contract_def(event_name) {
            return event.emit(event_values);
        }

        return Err(CompileError::static_str("No definition found"));
    }

    Err(CompileError::static_str("Invalid expression in emit statement"))
}

fn call_arg(
    scope: Shared<FunctionScope>,
    arg: &vyp::CallArg,
) -> Result<yul::Expression, CompileError> {
    match arg {
        vyp::CallArg::Arg(value) => {
            let (value, _) = expressions::expr(scope, value)?;
            Ok(value)
        }
        vyp::CallArg::Kwarg(vyp::Kwarg { name: _, value }) => {
            let (value, _) = expressions::expr(scope, &value.node)?;
            Ok(value)
        }
    }
}

fn func_return(
    scope: Shared<FunctionScope>,
    value: &Option<Spanned<vyp::Expr>>,
) -> Result<yul::Statement, CompileError> {
    if let Some(value) = value {
        let (value, _) = expressions::expr(scope, &value.node)?;
        return Ok(statement! { return_val := [value] });
    }

    unimplemented!("Empty returns")
}
