use std::rc::Rc;

#[salsa::tracked(return_ref)]
pub fn symbol_name(db: &dyn CodegenDb, contract: ContractId) -> Rc<String> {
    let module = contract.module(db.upcast());

    format!(
        "{}${}",
        module.name(db.upcast()),
        contract.name(db.upcast())
    )
    .into()
}

#[salsa::tracked(return_ref)]
pub fn deployer_symbol_name(db: &dyn CodegenDb, contract: ContractId) -> Rc<String> {
    format!("deploy_{}", symbol_name(db, contract).as_ref()).into()
}
