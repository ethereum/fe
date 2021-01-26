use crate::errors::CompileError;
use crate::yul::mappers::contracts;
use fe_analyzer::Context;
use fe_parser::ast as fe;
use std::collections::HashMap;
use yultsur::yul;

pub type YulContracts = HashMap<String, yul::Object>;

/// Builds a vector of Yul contracts from a Fe module.
pub fn module(context: &Context, module: &fe::Module) -> Result<YulContracts, CompileError> {
    module
        .body
        .iter()
        .try_fold(YulContracts::new(), |mut contracts, stmt| {
            match &stmt.node {
                fe::ModuleStmt::TypeDef { .. } => {}
                fe::ModuleStmt::ContractDef { name, .. } => {
                    let contract = contracts::contract_def(context, stmt)?;

                    if contracts.insert(name.node.to_string(), contract).is_some() {
                        return Err(CompileError::static_str("duplicate contract def"));
                    }
                }
                fe::ModuleStmt::StructDef { .. } => {}
                fe::ModuleStmt::FromImport { .. } => unimplemented!(),
                fe::ModuleStmt::SimpleImport { .. } => unimplemented!(),
            }

            Ok(contracts)
        })
}
