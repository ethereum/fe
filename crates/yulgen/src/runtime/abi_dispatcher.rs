use crate::names::abi as abi_names;
use crate::operations::abi as abi_operations;
use crate::types::{to_abi_selector_names, AbiDecodeLocation, AbiType};
use fe_abi::utils as abi_utils;
use smol_str::SmolStr;
use yultsur::*;

/// Builds a switch statement that dispatches calls to the contract.
pub fn dispatcher(
    functions: &[(SmolStr, SmolStr, impl AsRef<[AbiType]>, Option<AbiType>)],
) -> yul::Statement {
    let arms = functions
        .iter()
        .map(|(name, qname, params, ret)| dispatch_arm(name, qname, params.as_ref(), ret))
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

fn dispatch_arm(
    bare_name: &str,
    qualified_name: &str,
    params: &[AbiType],
    return_type: &Option<AbiType>,
) -> yul::Case {
    let selector = selector(bare_name, params);

    let (param_idents, param_exprs) = abi_names::vals("call", params.len());

    // If there are no params, we create an empty vector.
    let maybe_decode_params = if params.is_empty() {
        statements! {}
    } else {
        let decode_expr = abi_operations::decode_data(
            params,
            expression! { 4 },
            expression! { calldatasize() },
            AbiDecodeLocation::Calldata,
        );
        statements! { (let [param_idents...] := [decode_expr]) }
    };

    // If the function returns a unit value, we call the function and return
    // nothing. Otherwise, we encode the value and return it.
    let call_and_maybe_encode_return = {
        let name = identifier! { (qualified_name) };
        let call = expression! { [name]([param_exprs...]) };
        if let Some(return_type) = return_type {
            let return_expr = expressions! { return_val };
            let encoding_size = abi_operations::encoding_size(&[return_type.clone()], &return_expr);
            let encode_expr = abi_operations::encode(&[return_type.clone()], return_expr);
            statements! {
                (let return_val := [call])
                (let encoding_start := [encode_expr])
                (let encoding_size := [encoding_size])
                (return(encoding_start, encoding_size))
            }
        } else {
            statements! {
                (pop([call]))
                (return(0, 0))
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
    literal! { (abi_utils::func_selector(name, &to_abi_selector_names(params))) }
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
