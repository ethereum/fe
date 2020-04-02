use crate::errors::CompileError;
use crate::yul::namespace::scopes::ContractDef;
#[allow(unused_imports)]
use crate::yul::namespace::types::{Base, FixedSize};
use std::collections::HashMap;
use tiny_keccak::{Hasher, Keccak};
use yultsur::*;

/// Builds a switch statement from the contract ABI that handles contract calls.
pub fn switch(
    interface: &Vec<String>,
    defs: &HashMap<String, ContractDef>,
) -> Result<yul::Statement, CompileError> {
    let cases = interface
        .into_iter()
        .map(|name| case(name.to_owned(), defs))
        .collect::<Result<Vec<yul::Case>, CompileError>>()?;

    Ok(switch! {
        switch (cloadn(0, 4))
        [cases...]
    })
}

fn case(name: String, defs: &HashMap<String, ContractDef>) -> Result<yul::Case, CompileError> {
    if let Some(def) = defs.get(&name) {
        return match def {
            ContractDef::Function { params, returns } => {
                function_call_case(name, params, returns.to_owned())
            }
            _ => Err(CompileError::static_str(
                "Cannot create case from definition",
            )),
        };
    }

    Err(CompileError::static_str("No definition for name"))
}

fn function_call_case(
    name: String,
    params: &Vec<FixedSize>,
    returns: Option<FixedSize>,
) -> Result<yul::Case, CompileError> {
    let selector = selector_literal(name.clone(), &params);
    let name = identifier! {(name)};
    let params = parameter_expressions(&params)?;

    if let Some(returns) = returns {
        let return_size = literal_expression! {(returns.padded_size())};
        let function_call = expression! { [name]([params...]) };

        Ok(case! {
            case [selector] {
                (return([returns.encode(function_call)?], [return_size]))
            }
        })
    } else {
        let function_call = statement! { [name]([params...]) };

        Ok(case! {
            case [selector] { [function_call] }
        })
    }
}

fn selector_literal(name: String, params: &Vec<FixedSize>) -> yul::Literal {
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

fn parameter_expressions(params: &Vec<FixedSize>) -> Result<Vec<yul::Expression>, CompileError> {
    let mut ptr = 4;
    let mut expressions = vec![];

    for param in params.iter() {
        expressions.push(param.decode(literal_expression! {(ptr)})?);
        ptr += param.padded_size();
    }

    Ok(expressions)
}

#[test]
fn selector_literal_basic() {
    assert_eq!(
        selector_literal("foo".to_string(), &vec![]).to_string(),
        String::from("0xc2985578"),
        "Incorrect selector"
    )
}

#[test]
fn test_selector_literal() {
    assert_eq!(
        selector_literal("bar".to_string(), &vec![FixedSize::Base(Base::U256)]).to_string(),
        String::from("0x0423a132"),
    )
}
