use crate::yul::names;
use fe_analyzer::namespace::types::Contract;
use yultsur::*;

/// Make a call to a contract of the given type and address with a set of
/// parameters.
pub fn contract_call(
    contract: Contract,
    func_name: String,
    address: yul::Expression,
    params: Vec<yul::Expression>,
) -> yul::Expression {
    let func_name = names::contract_call(&contract.name, &func_name);
    expression! { [func_name]([address], [params...]) }
}
