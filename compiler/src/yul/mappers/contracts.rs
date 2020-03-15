use crate::errors::CompileError;
use crate::yul::mappers::{constructor, functions, types};
use crate::yul::namespace::scopes::{ContractScope, ModuleScope, Scope, Shared};
use crate::yul::namespace::types::Type;
use crate::yul::runtime::abi as runtime_abi;
use crate::yul::runtime::functions as runtime_functions;

use std::rc::Rc;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;
use yultsur::*;

/// Builds a Yul object from a Vyper contract.
pub fn contract_def<'a>(
    module_scope: Shared<ModuleScope>,
    name: String,
    body: &'a Vec<Spanned<vyp::ContractStmt<'a>>>,
) -> Result<yul::Statement, CompileError> {
    let contract_scope = ContractScope::new(Rc::clone(&module_scope));

    let mut statements = body
        .iter()
        .map(|stmt| contract_stmt(Rc::clone(&contract_scope), &stmt.node))
        .collect::<Result<Vec<Option<yul::Statement>>, CompileError>>()?
        .into_iter()
        .filter_map(|stmt| stmt)
        .collect::<Vec<yul::Statement>>();

    statements.append(&mut runtime_functions::all());
    statements.push(runtime_abi::switch(
        &contract_scope.borrow().interface,
        &contract_scope.borrow().defs,
    )?);

    Ok(yul::Statement::Object(yul::Object {
        name: identifier! { Contract },
        code: constructor::code(),
        objects: vec![yul::Object {
            name: identifier! { runtime },
            code: yul::Code {
                block: yul::Block { statements },
            },
            objects: vec![],
        }],
    }))
}

/// Builds a Yul statement from a Vyper contract statement.
fn contract_stmt<'a>(
    scope: Shared<ContractScope>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<Option<yul::Statement>, CompileError> {
    match stmt {
        vyp::ContractStmt::ContractField { qual, name, typ } => {
            contract_field(scope, qual, name.node.to_string(), &typ.node)?;
            Ok(None)
        }
        vyp::ContractStmt::FuncDef {
            qual,
            name,
            args,
            return_type,
            body,
        } => {
            let function =
                functions::func_def(scope, qual, name.node.to_string(), args, return_type, body)?;
            Ok(Some(function))
        }
        _ => Err(CompileError::static_str(
            "Unable to translate module statement.",
        )),
    }
}

/// Creates a new contract variable entry from a Vyper contract field
fn contract_field<'a>(
    scope: Shared<ContractScope>,
    qual: &'a Option<Spanned<vyp::ContractFieldQual>>,
    name: String,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<(), CompileError> {
    match types::type_desc(Scope::Contract(Rc::clone(&scope)), typ)? {
        Type::Map(map) => scope.borrow_mut().add_map(name, map),
        _ => {
            return Err(CompileError::static_str(
                "Contract field type not supported",
            ))
        }
    };

    Ok(())
}
