use crate::mappers::contracts;
use crate::{AnalyzerDb, ModuleId};
use std::collections::HashMap;
use yultsur::yul;

pub type YulContracts = HashMap<String, yul::Object>;

/// Builds a vector of Yul contracts from a Fe module.
pub fn module(db: &dyn AnalyzerDb, module: ModuleId) -> YulContracts {
    module
        .all_contracts(db)
        .iter()
        .fold(YulContracts::new(), |mut contracts, id| {
            let yul_contract = contracts::contract_def(db, *id, &contracts);

            if contracts.insert(id.name(db), yul_contract).is_some() {
                panic!("duplicate contract definition");
            }
            contracts
        })
}
