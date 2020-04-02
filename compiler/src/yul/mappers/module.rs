use crate::errors::CompileError;
use crate::yul::mappers::contracts;
use crate::yul::namespace::scopes::{ModuleScope, Shared};
use crate::yul::namespace::types;
use std::rc::Rc;
use vyper_parser::ast as vyp;
use yultsur::yul;

/// Builds a vector of Yul contracts from a Vyper module.
pub fn module(module: &vyp::Module) -> Result<Vec<yul::Object>, CompileError> {
    let scope = ModuleScope::new();

    Ok(module
        .body
        .iter()
        .map(|stmt| module_stmt(Rc::clone(&scope), &stmt.node))
        .collect::<Result<Vec<Option<yul::Statement>>, CompileError>>()?
        .into_iter()
        .filter_map(|statement| {
            if let Some(yul::Statement::Object(object)) = statement {
                return Some(object);
            }

            None
        })
        .collect::<Vec<yul::Object>>())
}

fn module_stmt(
    scope: Shared<ModuleScope>,
    stmt: &vyp::ModuleStmt,
) -> Result<Option<yul::Statement>, CompileError> {
    match stmt {
        vyp::ModuleStmt::TypeDef { name, typ } => {
            type_def(scope, name.node.to_string(), &typ.node)?;
            Ok(None)
        }
        vyp::ModuleStmt::ContractDef { name, body } => {
            let contract = contracts::contract_def(scope, name.node.to_string(), body)?;
            Ok(Some(contract))
        }
        _ => unimplemented!("Module statement"),
    }
}

fn type_def(
    scope: Shared<ModuleScope>,
    name: String,
    typ: &vyp::TypeDesc,
) -> Result<(), CompileError> {
    let typ = types::type_desc(&scope.borrow().defs, &typ)?;
    scope.borrow_mut().add_type_def(name, typ);

    Ok(())
}
