use crate::yul::names;
use fe_analyzer::namespace::types::Contract;
use yultsur::*;

/// Make a call to a contract of the given type and address with a set of
/// parameters.
pub fn call(
    contract: Contract,
    func_name: &str,
    address: yul::Expression,
    params: Vec<yul::Expression>,
) -> yul::Expression {
    let func_name = names::contract_call(&contract.name, func_name);
    expression! { [func_name]([address], [params...]) }
}

/// Executes the `create2` operation for a given contract with the given value
/// and salt.
pub fn create2(
    contract: &Contract,
    value: yul::Expression,
    salt: yul::Expression,
) -> yul::Expression {
    let name = literal_expression! { (format!("\"{}\"", contract.name)) };
    expression! {
        contract_create2(
            (dataoffset([name.clone()])),
            (datasize([name])),
            [value],
            [salt]
        )
    }
}

/// Executes the `create` operation for a given contract with the given value.
pub fn create(contract: &Contract, value: yul::Expression) -> yul::Expression {
    let name = literal_expression! { (format!("\"{}\"", contract.name)) };
    expression! {
        contract_create(
            (dataoffset([name.clone()])),
            (datasize([name])),
            [value]
        )
    }
}
