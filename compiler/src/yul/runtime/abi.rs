use crate::errors::CompileError;
use crate::yul::namespace::scopes::ContractDef;
#[allow(unused_imports)]
use crate::yul::namespace::types::{Base, FixedSize};
use std::collections::HashMap;
use tiny_keccak::{Hasher, Keccak};
use yultsur::*;

/// Builds a switch statement that dispatches calls to the contract.
pub fn dispatcher(
    interface: &Vec<String>,
    defs: &HashMap<String, ContractDef>,
) -> Result<yul::Statement, CompileError> {
    let arms = interface
        .into_iter()
        .map(|name| dispatch_arm(name.to_owned(), defs))
        .collect::<Result<Vec<yul::Case>, CompileError>>()?;

    Ok(switch! {
        switch (cloadn(0, 4))
        [arms...]
    })
}

fn dispatch_arm(
    name: String,
    defs: &HashMap<String, ContractDef>
) -> Result<yul::Case, CompileError> {
    if let Some(ContractDef::Function { params, returns }) = defs.get(&name) {
        let selector = selector(name.clone(), &params);

        if let Some(returns) = returns {
            let selection = selection(name, &params)?;
            let return_data = returns.encode(selection)?;
            let return_size = literal_expression! {(returns.padded_size())};

            let selection_with_return = statement! { return([return_data], [return_size]) };

            return Ok(case! { case [selector] { [selection_with_return] } })
        }

        let selection = selection_as_statement(name, &params)?;

        return Ok(case! { case [selector] { [selection] } })
    }

    Err(CompileError::static_str("No definition for name"))
}

fn selector(
    name: String,
    params: &Vec<FixedSize>
) -> yul::Literal {
    let signature = format!(
        "{}({})",
        name,
        params
            .iter()
            .map(|param| param.abi_name())
            .collect::<Vec<String>>()
            .join(",")
    );

    let mut keccak = Keccak::v256();
    let mut selector = [0u8; 4];

    keccak.update(signature.as_bytes());
    keccak.finalize(&mut selector);

    literal! {(format!("0x{}", hex::encode(selector)))}
}

fn selection(
    name: String,
    params: &Vec<FixedSize>
) -> Result<yul::Expression, CompileError> {
    let mut ptr = 4;
    let mut decoded_params = vec![];

    for param in params.iter() {
        decoded_params.push(param.decode(literal_expression! {(ptr)})?);
        ptr += param.padded_size();
    }

    let name = identifier! {(name)};

    Ok(expression! { [name]([decoded_params...]) })
}


fn selection_as_statement(
    name: String,
    params: &Vec<FixedSize>
) -> Result<yul::Statement, CompileError> {
    Ok(yul::Statement::Expression(selection(name, params)?))
}

#[test]
fn test_selector_literal_basic() {
    assert_eq!(
        selector("foo".to_string(), &vec![]).to_string(),
        String::from("0xc2985578"),
    )
}

#[test]
fn test_selector_literal() {
    assert_eq!(
        selector("bar".to_string(), &vec![FixedSize::Base(Base::U256)]).to_string(),
        String::from("0x0423a132"),
    )
}
