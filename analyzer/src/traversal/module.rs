use crate::errors::SemanticError;
use crate::namespace::scopes::{
    ModuleScope,
    Shared,
};
use crate::namespace::types;
use crate::traversal::{
    contracts,
    functions,
    structs,
};
use crate::Context;
use crate::ContractAttributes;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::rc::Rc;

/// Gather context information for a module and check for type errors.
pub fn module(context: Shared<Context>, module: &fe::Module) -> Result<(), SemanticError> {
    let scope = ModuleScope::new();

    // Walk over the module but ignore everything that is *inside* contracts.
    // This is to make contract types known prior to where we know their exact shape
    // so that they can already be used as types in other contracts.
    for stmt in module.body.iter() {
        match &stmt.node {
            fe::ModuleStmt::TypeDef { .. } => type_def(Rc::clone(&scope), stmt)?,
            fe::ModuleStmt::StructDef { name, body } => {
                structs::struct_def(Rc::clone(&scope), name.node, body)?
            }
            fe::ModuleStmt::ContractDef { .. } => {
                contracts::contract_def(Rc::clone(&scope), Rc::clone(&context), stmt)?
            }
            fe::ModuleStmt::FromImport { .. } => unimplemented!(),
            fe::ModuleStmt::SimpleImport { .. } => unimplemented!(),
        }
    }

    // Now inspect the bodies of the contract. At this point they can use other
    // contract types in storage declarations and function signatures because we
    // know the types from the previous walk.
    for module_stmt in module.body.iter() {
        if let fe::ModuleStmt::ContractDef { .. } = &module_stmt.node {
            contracts::contract_body(Rc::clone(&scope), Rc::clone(&context), module_stmt)?;
        }
    }

    // Final walk: At this point we know all contract types with their exact shape.
    // What's left is to walk through the function bodies to analyze them.
    for module_stmt in module.body.iter() {
        if let fe::ModuleStmt::ContractDef { name, body } = &module_stmt.node {
            let contract_scope = scope.borrow().contract_def(name.node).expect("msg").scope;
            for stmt in body.iter() {
                if let fe::ContractStmt::FuncDef { .. } = &stmt.node {
                    functions::func_body(Rc::clone(&contract_scope), Rc::clone(&context), stmt)
                        .map_err(|error| error.with_context(stmt.span))?;
                }
            }
            // At this point, the contract attributes are final and can be added to the
            // context
            let contract_attributes = ContractAttributes::from(Rc::clone(&contract_scope));
            context
                .borrow_mut()
                .add_contract(module_stmt, contract_attributes);
        }
    }

    Ok(())
}

fn type_def(
    scope: Shared<ModuleScope>,
    def: &Spanned<fe::ModuleStmt>,
) -> Result<(), SemanticError> {
    if let fe::ModuleStmt::TypeDef { name, typ } = &def.node {
        let typ = types::type_desc(&scope.borrow().type_defs, &typ.node)?;
        scope.borrow_mut().add_type_def(name.node, typ);
        return Ok(());
    }

    unreachable!()
}
