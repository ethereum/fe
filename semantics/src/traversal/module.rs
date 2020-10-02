use crate::errors::SemanticError;
use crate::namespace::scopes::{
    ModuleScope,
    Shared,
};
use crate::namespace::types;
use crate::traversal::contracts;
use crate::Context;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::rc::Rc;

/// Gather context information for a module and check for type errors.
pub fn module(context: Shared<Context>, module: &fe::Module) -> Result<(), SemanticError> {
    let scope = ModuleScope::new();

    for stmt in module.body.iter() {
        match &stmt.node {
            fe::ModuleStmt::TypeDef { .. } => type_def(Rc::clone(&scope), stmt)?,
            fe::ModuleStmt::ContractDef { .. } => {
                contracts::contract_def(Rc::clone(&scope), Rc::clone(&context), stmt)?
            }
            fe::ModuleStmt::FromImport { .. } => unimplemented!(),
            fe::ModuleStmt::SimpleImport { .. } => unimplemented!(),
        }
    }

    Ok(())
}

fn type_def(
    scope: Shared<ModuleScope>,
    def: &Spanned<fe::ModuleStmt>,
) -> Result<(), SemanticError> {
    if let fe::ModuleStmt::TypeDef { name, typ } = &def.node {
        let typ = types::type_desc(&scope.borrow().defs, &typ.node)?;
        scope.borrow_mut().add_type_def(name.node.to_string(), typ);
        return Ok(());
    }

    unreachable!()
}
