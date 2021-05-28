use crate::yul::mappers::contracts;
use fe_analyzer::context::Context;
use fe_parser::ast as fe;
use std::collections::HashMap;
use yultsur::yul;

pub type YulContracts = HashMap<String, yul::Object>;

/// Builds a vector of Yul contracts from a Fe module.
pub fn module(context: &Context, module: &fe::Module) -> YulContracts {
    module
        .body
        .iter()
        .fold(YulContracts::new(), |mut contracts, stmt| {
            match &stmt.kind {
                fe::ModuleStmt::Pragma { .. } => {}
                fe::ModuleStmt::TypeDef { .. } => {}
                fe::ModuleStmt::ContractDef { name, .. } => {
                    // Map the set of created contract names to their Yul objects so they can be
                    // included in the Yul contract that deploys them.
                    let created_contracts = context
                        .get_contract(stmt)
                        .expect("invalid attributes")
                        .created_contracts
                        .iter()
                        .map(|contract_name| contracts[contract_name].clone())
                        .collect::<Vec<_>>();

                    let contract = contracts::contract_def(context, stmt, created_contracts);

                    if contracts.insert(name.kind.clone(), contract).is_some() {
                        panic!("duplicate contract definition");
                    }
                }
                fe::ModuleStmt::StructDef { .. } => {}
                fe::ModuleStmt::FromImport { .. } => unimplemented!(),
                fe::ModuleStmt::SimpleImport { .. } => unimplemented!(),
            }

            contracts
        })
}
