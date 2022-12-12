use std::rc::Rc;

use fe_analyzer::namespace::items::{self as analyzer_items, FunctionId};

use crate::{db::MirDb, ir::FunctionSigId};

pub fn mir_lower_contract_all_functions(
    db: &dyn MirDb,
    contract: analyzer_items::ContractId,
) -> Rc<Vec<(FunctionSigId, FunctionId)>> {
    contract
        .all_functions(db.upcast())
        .iter()
        .map(|func| {
            let sig = func.sig(db.upcast());
            (db.mir_lowered_func_signature(sig), *func)
        })
        .collect::<Vec<_>>()
        .into()
}
