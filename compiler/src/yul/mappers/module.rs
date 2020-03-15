use crate::errors::CompileError;
use crate::yul::mappers::contracts;
use crate::yul::namespace::scopes::{ModuleDef, ModuleScope, Shared};
use crate::yul::namespace::types;

use std::rc::Rc;
use vyper_parser::ast as vyp;
use yultsur::yul;

/// Builds a vector of Yul contracts from a Vyper module.
pub fn module<'a>(module: &'a vyp::Module<'a>) -> Result<Vec<yul::Object>, CompileError> {
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

/// Builds a Yul statement from a Vyper module statement.
pub fn module_stmt<'a>(
    scope: Shared<ModuleScope>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<Option<yul::Statement>, CompileError> {
    match stmt {
        vyp::ModuleStmt::TypeDef { .. } => {
            type_def(scope, stmt)?;
            Ok(None)
        }
        vyp::ModuleStmt::ContractDef { name, body } => {
            let contract = contracts::contract_def(scope, name.node.to_string(), body)?;
            Ok(Some(contract))
        }
        _ => Err(CompileError::static_str("Unsupported module statement.")),
    }
}

/// Adds the custom type def to the module scope.
pub fn type_def<'a>(
    scope: Shared<ModuleScope>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<(), CompileError> {
    if let vyp::ModuleStmt::TypeDef { name, typ } = stmt {
        let typ = types::type_desc(&scope.borrow().defs, &typ.node)?;
        scope
            .borrow_mut()
            .defs
            .insert(name.node.to_string(), ModuleDef::Type(typ));
        return Ok(());
    }

    Err(CompileError::static_str(
        "Type definition translation requires TypeDef.",
    ))
}
