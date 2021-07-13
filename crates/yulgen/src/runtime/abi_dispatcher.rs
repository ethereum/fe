use crate::names::abi as abi_names;
use crate::operations::abi as abi_operations;
use crate::types::{to_abi_selector_names, to_abi_types, AbiDecodeLocation, AbiType};
use fe_abi::utils as abi_utils;
use fe_analyzer::context::FunctionAttributes;
use yultsur::*;

/// Builds a switch statement that dispatches calls to the contract.
pub fn dispatcher(attributes: &[FunctionAttributes]) -> yul::Statement {
    let arms = attributes
        .iter()
        .map(|arm| dispatch_arm(arm.to_owned()))
        .collect::<Vec<_>>();

    if arms.is_empty() {
        statement! { return(0, 0) }
    } else {
        switch! {
            switch (cloadn(0, 4))
            [arms...]
            (default { (return(0, 0)) })
        }
    }
}

fn dispatch_arm(attributes: FunctionAttributes) -> yul::Case {
    let selector = selector(&attributes.name, &to_abi_types(&attributes.param_types()));

    let (param_idents, param_exprs) = abi_names::vals("call", attributes.params.len());

    // If there are no params, we create an empty vector.
    let maybe_decode_params = if attributes.params.is_empty() {
        statements! {}
    } else {
        let decode_expr = abi_operations::decode_data(
            &to_abi_types(&attributes.param_types()),
            expression! { 4 },
            expression! { calldatasize() },
            AbiDecodeLocation::Calldata,
        );
        statements! { (let [param_idents...] := [decode_expr]) }
    };

    // If the function returns a unit value, we call the function and return
    // nothing. Otherwise, we encode the value and return it.
    let call_and_maybe_encode_return = {
        let name = identifier! { (format!("$${}", attributes.name)) };
        let call = expression! { [name]([param_exprs...]) };
        if attributes.return_type.is_unit() {
            statements! {
                (pop([call]))
                (return(0, 0))
            }
        } else {
            let encode_expr = abi_operations::encode(
                &[AbiType::from(&attributes.return_type)],
                expressions! { return_val },
            );
            let encoding_size = abi_operations::encoding_size(
                &[AbiType::from(&attributes.return_type)],
                expressions! { return_val },
            );
            statements! {
                (let return_val := [call])
                (let encoding_start := [encode_expr])
                (let encoding_size := [encoding_size])
                (return(encoding_start, encoding_size))
            }
        }
    };

    case! {
        case [selector] {
            [maybe_decode_params...]
            [call_and_maybe_encode_return...]
        }
    }
}

fn selector(name: &str, params: &[AbiType]) -> yul::Literal {
    let params = to_abi_selector_names(params);
    literal! { (abi_utils::func_selector(name, &params)) }
}

#[cfg(test)]
mod tests {
    use crate::runtime::abi_dispatcher::selector;
    use crate::types::AbiType;

    #[test]
    fn test_selector_literal_basic() {
        assert_eq!(selector("foo", &[]).to_string(), String::from("0xc2985578"),)
    }

    #[test]
    fn test_selector_literal() {
        assert_eq!(
            selector("bar", &[AbiType::Uint { size: 32 }]).to_string(),
            String::from("0x0423a132"),
        )
    }
}
