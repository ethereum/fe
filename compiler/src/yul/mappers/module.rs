use crate::errors::CompileError;
use crate::yul::mappers::contracts;
use crate::yul::namespace::scopes::{
    ModuleScope,
    Shared,
};
use crate::yul::namespace::types;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::collections::HashMap;
use std::rc::Rc;
use yultsur::yul;

pub type YulContracts = HashMap<String, yul::Object>;

/// Builds a vector of Yul contracts from a Fe module.
pub fn module(module: &fe::Module) -> Result<YulContracts, CompileError> {
    let scope = ModuleScope::new();

    module
        .body
        .iter()
        .try_fold(YulContracts::new(), |mut contracts, stmt| {
            match &stmt.node {
                fe::ModuleStmt::TypeDef { .. } => type_def(Rc::clone(&scope), stmt)?,
                fe::ModuleStmt::ContractDef { name, .. } => {
                    let contract = contracts::contract_def(Rc::clone(&scope), stmt)?;

                    if contracts.insert(name.node.to_string(), contract).is_some() {
                        return Err(CompileError::static_str("duplicate contract def"));
                    }
                }
                fe::ModuleStmt::FromImport { .. } => unimplemented!(),
                fe::ModuleStmt::SimpleImport { .. } => unimplemented!(),
            }

            Ok(contracts)
        })
}

fn type_def(scope: Shared<ModuleScope>, def: &Spanned<fe::ModuleStmt>) -> Result<(), CompileError> {
    if let fe::ModuleStmt::TypeDef { name, typ } = &def.node {
        let typ = types::type_desc(&scope.borrow().defs, &typ.node)?;
        scope.borrow_mut().add_type_def(name.node.to_string(), typ);
        return Ok(());
    }

    unreachable!()
}
