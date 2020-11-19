use crate::abi::utils as abi_utils;
use crate::errors::CompileError;
use crate::yul::abi::operations as abi_operations;
#[cfg(test)]
use fe_semantics::namespace::types::Base;
use fe_semantics::namespace::types::{
    AbiDecodeLocation,
    AbiEncoding,
    FixedSize,
};
use fe_semantics::FunctionAttributes;
use yultsur::*;

/// Builds a switch statement that dispatches calls to the contract.
pub fn dispatcher(attributes: Vec<FunctionAttributes>) -> Result<yul::Statement, CompileError> {
    let arms = attributes
        .iter()
        .map(|arm| dispatch_arm(arm.to_owned()))
        .collect::<Result<Vec<yul::Case>, CompileError>>()?;

    Ok(switch! {
        switch (cloadn(0, 4))
        [arms...]
    })
}

fn dispatch_arm(attributes: FunctionAttributes) -> Result<yul::Case, CompileError> {
    let selector = selector(attributes.name.clone(), &attributes.param_types);

    if !attributes.return_type.is_empty_tuple() {
        let selection = selection(attributes.name, &attributes.param_types)?;
        let return_data = abi_operations::encode(
            vec![attributes.return_type.clone()],
            vec![expression! { raw_return }],
        );

        let return_size = abi_operations::encode_size(
            vec![attributes.return_type],
            vec![expression! { raw_return }],
        );

        let selection_with_return = statement! { return([return_data], [return_size]) };

        return Ok(case! { case [selector] {
            (let raw_return := [selection])
            ([selection_with_return])
        } });
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
    let decoded_params = abi_operations::decode(
        params.to_owned(),
        literal_expression! { 4 },
        AbiDecodeLocation::Calldata,
    );

    let name = identifier! { (name) };

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
        selector("foo".to_string(), &[]).to_string(),
        String::from("0xc2985578"),
    )
}

#[test]
fn test_selector_literal() {
    assert_eq!(
        selector("bar".to_string(), &[FixedSize::Base(Base::U256)]).to_string(),
        String::from("0x0423a132"),
    )
}
