use crate::errors::SemanticError;
use crate::namespace::scopes::{ModuleScope, Scope, Shared};
use crate::traversal::{contracts, structs, types};
use crate::Context;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::rc::Rc;

/// Gather context information for a module and check for type errors.
pub fn module(context: Shared<Context>, module: &fe::Module) -> Result<(), SemanticError> {
    let scope = ModuleScope::new();

    let mut contracts = vec![];

    for stmt in module.body.iter() {
        match &stmt.kind {
            fe::ModuleStmt::TypeDef { .. } => {
                type_def(Rc::clone(&context), Rc::clone(&scope), stmt)?
            }
            fe::ModuleStmt::StructDef { name, fields } => {
                structs::struct_def(Rc::clone(&context), Rc::clone(&scope), &name.kind, fields)?
            }
            fe::ModuleStmt::ContractDef { .. } => {
                // Collect contract statements and the scope that we create for them. After we
                // have walked all contracts once, we walk over them again for a
                // more detailed inspection.
                let contract_scope =
                    contracts::contract_def(Rc::clone(&scope), Rc::clone(&context), stmt)?;
                contracts.push((stmt, contract_scope))
            }
            fe::ModuleStmt::FromImport { .. } => unimplemented!(),
            fe::ModuleStmt::SimpleImport { .. } => unimplemented!(),
        }
    }

    for (stmt, scope) in contracts.iter() {
        if let fe::ModuleStmt::ContractDef { .. } = stmt.kind {
            contracts::contract_body(Rc::clone(&scope), Rc::clone(&context), stmt)?
        }
    }

    context.borrow_mut().set_module(scope.into());

    Ok(())
}

fn type_def(
    context: Shared<Context>,
    scope: Shared<ModuleScope>,
    def: &Node<fe::ModuleStmt>,
) -> Result<(), SemanticError> {
    if let fe::ModuleStmt::TypeDef { name, typ } = &def.kind {
        let typ = types::type_desc(&Scope::Module(Rc::clone(&scope)), context, &typ)?;
        scope.borrow_mut().add_type_def(&name.kind, typ)?;
        return Ok(());
    }

    unreachable!()
}
