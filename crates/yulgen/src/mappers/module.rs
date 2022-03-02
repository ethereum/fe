use crate::{ModuleId, YulgenDb};
use std::collections::HashMap;
use yultsur::yul;

pub type YulContracts = HashMap<String, yul::Object>;

/// Builds a vector of Yul contracts from a Fe module.
pub fn module(db: &dyn YulgenDb, module: ModuleId) -> YulContracts {
    module
        .all_contracts(db.upcast())
        .iter()
        .fold(YulContracts::new(), |mut contracts, id| {
            let yul_contract = db.contract_object(*id);

            if contracts
                .insert(id.name(db.upcast()).to_string(), yul_contract)
                .is_some()
            {
                panic!("duplicate contract definition");
            }
            contracts
        })
}

pub fn module_runtimes(db: &dyn YulgenDb, module: ModuleId) -> YulContracts {
    module
        .all_contracts(db.upcast())
        .iter()
        .fold(YulContracts::new(), |mut contracts, id| {
            let yul_contract = db.contract_runtime_object(*id);

            if contracts
                .insert(id.name(db.upcast()).to_string(), yul_contract)
                .is_some()
            {
                panic!("duplicate contract definition");
            }
            contracts
        })
}
