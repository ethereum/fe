use crate::abi::utils as abi_utils;
use crate::errors::CompileError;
#[cfg(test)]
use fe_semantics::namespace::types::Base;
use fe_semantics::namespace::types::FixedSize;
use fe_semantics::FunctionAttributes;
use yultsur::*;

/// Builds a switch statement that dispatches calls to the contract.
pub fn dispatcher(attributes: Vec<FunctionAttributes>) -> Result<yul::Statement, CompileError> {
    let arms = attributes
        .iter()
        .map(|a| dispatch_arm(a.to_owned()))
        .collect::<Result<Vec<yul::Case>, CompileError>>()?;

    Ok(switch! {
        switch (cloadn(0, 4))
        [arms...]
    })
}

fn dispatch_arm(attributes: FunctionAttributes) -> Result<yul::Case, CompileError> {
    let selector = selector(attributes.name.clone(), &attributes.param_types);

    if let Some(return_type) = attributes.return_type {
        let selection = selection(attributes.name, &attributes.param_types)?;
        let return_data = return_type.encode(selection);
        let return_size = literal_expression! {(return_type.padded_size())};

        let selection_with_return = statement! { return([return_data], [return_size]) };

        return Ok(case! { case [selector] { [selection_with_return] } });
    }

    let selection = selection_as_statement(attributes.name, &attributes.param_types)?;

    Ok(case! { case [selector] { [selection] } })
}

fn selector(name: String, params: &[FixedSize]) -> yul::Literal {
    let params = params
        .iter()
        .map(|param| param.abi_name())
        .collect::<Vec<String>>();

    literal! {(abi_utils::func_selector(name, params))}
}

fn selection(name: String, params: &[FixedSize]) -> Result<yul::Expression, CompileError> {
    let mut ptr = 4;
    let mut decoded_params = vec![];

    for param in params.iter() {
        decoded_params.push(param.decode(literal_expression! {(ptr)}));
        ptr += param.padded_size();
    }

    let name = identifier! {(name)};

    Ok(expression! { [name]([decoded_params...]) })
}

fn selection_as_statement(
    name: String,
    params: &[FixedSize],
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
