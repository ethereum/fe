use crate::errors::CompileError;
use crate::yul::mappers::contracts;
use crate::yul::namespace::scopes::{
    ModuleScope,
    Shared,
};
use crate::yul::namespace::types;
use std::collections::HashMap;
use std::rc::Rc;
use fe_parser::ast as vyp;
use fe_parser::span::Spanned;
use yultsur::yul;

pub type YulContracts = HashMap<String, yul::Object>;

/// Builds a vector of Yul contracts from a Fe module.
pub fn module(module: &vyp::Module) -> Result<YulContracts, CompileError> {
    let scope = ModuleScope::new();

    module
        .body
        .iter()
        .try_fold(YulContracts::new(), |mut contracts, stmt| {
            match &stmt.node {
                vyp::ModuleStmt::TypeDef { .. } => type_def(Rc::clone(&scope), stmt)?,
                vyp::ModuleStmt::ContractDef { name, .. } => {
                    let contract = contracts::contract_def(Rc::clone(&scope), stmt)?;

                    if contracts.insert(name.node.to_string(), contract).is_some() {
                        return Err(CompileError::static_str("duplicate contract def"));
                    }
                }
                vyp::ModuleStmt::FromImport { .. } => unimplemented!(),
                vyp::ModuleStmt::SimpleImport { .. } => unimplemented!(),
            }

            Ok(contracts)
        })
}

fn type_def(
    scope: Shared<ModuleScope>,
    def: &Spanned<vyp::ModuleStmt>,
) -> Result<(), CompileError> {
    if let vyp::ModuleStmt::TypeDef { name, typ } = &def.node {
        let typ = types::type_desc(&scope.borrow().defs, &typ.node)?;
        scope.borrow_mut().add_type_def(name.node.to_string(), typ);
        return Ok(());
    }

    unreachable!()
}
