use crate::yul::mappers::contracts;
use crate::yul::AnalyzerContext;
use fe_parser::ast as fe;
use std::collections::HashMap;
use yultsur::yul;

pub type YulContracts = HashMap<String, yul::Object>;

/// Builds a vector of Yul contracts from a Fe module.
pub fn module(analysis: &AnalyzerContext, module: &fe::Module) -> YulContracts {
    module
        .body
        .iter()
        .fold(YulContracts::new(), |mut contracts, stmt| {
            match &stmt {
                fe::ModuleStmt::Pragma(_) => {}
                fe::ModuleStmt::TypeAlias(_) => {}
                fe::ModuleStmt::Contract(def) => {
                    let contract = contracts::contract_def(analysis, def, &contracts);

                    if contracts
                        .insert(def.kind.name.kind.clone(), contract)
                        .is_some()
                    {
                        panic!("duplicate contract definition");
                    }
                }
                fe::ModuleStmt::Struct(_) => {}
                fe::ModuleStmt::Import(_) => unimplemented!(),
            }

            contracts
        })
}
