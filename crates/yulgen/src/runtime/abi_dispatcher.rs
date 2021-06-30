use crate::names;
use crate::operations::abi as abi_operations;
use fe_abi::utils as abi_utils;
use fe_analyzer::context::FunctionAttributes;
use fe_analyzer::namespace::types::{AbiDecodeLocation, AbiEncoding, FixedSize};
use yultsur::*;

/// Builds a switch statement that dispatches calls to the contract.
pub fn dispatcher(attributes: Vec<FunctionAttributes>) -> yul::Statement {
    let arms = attributes
        .iter()
        .map(|arm| dispatch_arm(arm.to_owned()))
        .collect::<Vec<_>>();

    if arms.is_empty() {
        return statement! { pop(0) };
    } else {
        switch! {
            switch (cloadn(0, 4))
            [arms...]
        }
    }
}

fn dispatch_arm(attributes: FunctionAttributes) -> yul::Case {
    let selector = selector(&attributes.name, &attributes.param_types());

    if !attributes.return_type.is_unit() {
        let selection = selection(&attributes.name, &attributes.param_types());
        let return_data = abi_operations::encode(
            vec![attributes.return_type.clone()],
            vec![expression! { raw_return }],
        );

        let return_size = abi_operations::encode_size(
            vec![attributes.return_type],
            vec![expression! { raw_return }],
        );

        let selection_with_return = statement! { return([return_data], [return_size]) };

        return case! {
            case [selector] {
                (let raw_return := [selection])
                ([selection_with_return])
            }
        };
    } else {
        // The return value of the selected statement muse be popped since all user defined function return a value.
        let selection = selection_as_pop_statement(&attributes.name, &attributes.param_types());
        case! { case [selector] { [selection] } }
    }
}

fn selector(name: &str, params: &[FixedSize]) -> yul::Literal {
    let params = params
        .iter()
        .map(|param| param.abi_selector_name())
        .collect::<Vec<String>>();

    literal! {(abi_utils::func_selector(name, params))}
}

fn selection(name: &str, params: &[FixedSize]) -> yul::Expression {
    let decoded_params = abi_operations::decode(
        params.to_owned(),
        literal_expression! { 4 },
        AbiDecodeLocation::Calldata,
    );

    let name = names::func_name(&name);

    expression! { [name]([decoded_params...]) }
}

fn selection_as_pop_statement(name: &str, params: &[FixedSize]) -> yul::Statement {
    yul::Statement::Expression(expression! { pop([selection(name, params)]) })
}

#[cfg(test)]
mod tests {
    use crate::runtime::abi_dispatcher::selector;
    use fe_analyzer::namespace::types::{FixedSize, U256};

    #[test]
    fn test_selector_literal_basic() {
        assert_eq!(selector("foo", &[]).to_string(), String::from("0xc2985578"),)
    }

    #[test]
    fn test_selector_literal() {
        assert_eq!(
            selector("bar", &[FixedSize::Base(U256)]).to_string(),
            String::from("0x0423a132"),
        )
    }
}
