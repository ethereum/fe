use crate::names;
use crate::names::abi as abi_names;
use crate::operations::abi as abi_operations;
use crate::types::{to_abi_selector_names, to_abi_types, AbiDecodeLocation, AsAbiType};
use fe_abi::utils as abi_utils;
use fe_analyzer::namespace::items::ContractId;
use fe_analyzer::AnalyzerDb;
use yultsur::*;

/// Return all contacts runtime functions
pub fn all() -> Vec<yul::Statement> {
    vec![create2(), create()]
}

/// Builds a set of functions used to make calls to the given contract's public
/// functions.
pub fn calls(db: &dyn AnalyzerDb, contract: ContractId) -> Vec<yul::Statement> {
    let contract_name = contract.name(db);
    contract
        .functions(db)
        .iter()
        .map(|(name, function)| {
            let signature = function.signature(db);
            let return_type = signature.return_type.clone().expect("fn return type error");

            // get the name of the call function and its parameters
            let function_name = names::contract_call(&contract_name, name);
            let param_types = to_abi_types(db, &signature.param_types());

            // create a pair of identifiers and expressions for the parameters
            let (param_idents, param_exprs) = abi_names::vals("param", signature.params.len());
            // the function selector must be added to the first 4 bytes of the calldata
            let selector = {
                let selector = abi_utils::func_selector(name, &to_abi_selector_names(&param_types));
                literal_expression! { (selector) }
            };

            // the size of the encoded data
            let encoding_size = abi_operations::encoding_size(&param_types, &param_exprs);
            // the operations used to encode the parameters
            let encoding_operation = abi_operations::encode(&param_types, param_exprs);

            if return_type.is_unit() {
                // there is no return data to handle
                function_definition! {
                    function [function_name](addr, [param_idents...]) -> return_val {
                        (let instart := alloc_mstoren([selector], 4))
                        (let insize := add(4, [encoding_size]))
                        (pop([encoding_operation]))
                        (pop((call((gas()), addr, 0, instart, insize, 0, 0))))
                    }
                }
            } else {
                let decoding_operation = abi_operations::decode_data(
                    &[return_type.as_abi_type(db)],
                    expression! { outstart },
                    expression! { add(outstart, outsize) },
                    AbiDecodeLocation::Memory,
                );
                // return data must be captured and decoded
                function_definition! {
                    function [function_name](addr, [param_idents...]) -> return_val {
                        (let instart := alloc_mstoren([selector], 4))
                        (let insize := add(4, [encoding_size]))
                        (pop([encoding_operation]))
                        (pop((call((gas()), addr, 0, instart, insize, 0, 0))))
                        (let outsize := returndatasize())
                        (let outstart := alloc(outsize))
                        (returndatacopy(outstart, 0, outsize))
                        (return_val := [decoding_operation])
                    }
                }
            }
        })
        .collect()
}

/// Function that executes the `create2` operation.
pub fn create2() -> yul::Statement {
    function_definition! {
        function contract_create2(data_ptr, data_size, value, salt) -> return_address {
            (let mptr := alloc(data_size))
            (datacopy(mptr, data_ptr, data_size))
            (return_address := create2(value, mptr, data_size, salt))
        }
    }
}

/// Function that executes the `create` operation.
pub fn create() -> yul::Statement {
    function_definition! {
        function contract_create(data_ptr, data_size, value) -> return_address {
            (let mptr := alloc(data_size))
            (datacopy(mptr, data_ptr, data_size))
            (return_address := create(value, mptr, data_size))
        }
    }
}
