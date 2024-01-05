use std::rc::Rc;

use crate::{db::MirDb, ir::FunctionId};

pub fn mir_lower_contract_all_functions(
    db: &dyn MirDb,
    contract: analyzer_items::ContractId,
) -> Rc<Vec<FunctionId>> {
    contract
        .all_functions(db.upcast())
        .iter()
        .map(|func| db.mir_lowered_func_signature(*func))
        .collect::<Vec<_>>()
        .into()
}
