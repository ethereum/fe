mod abi_dispatcher;
pub mod functions;
use crate::yul::Context;
use fe_analyzer::context::FunctionAttributes;
use fe_analyzer::namespace::types::{AbiDecodeLocation, Contract, FixedSize};
use fe_parser::ast as fe;
use fe_parser::node::Node;
use yultsur::*;

/// Builds the set of function statements that are needed during runtime.
pub fn build(context: &Context, contract: &Node<fe::Contract>) -> Vec<yul::Statement> {
    if let Some(attributes) = context.analysis.get_contract(contract) {
        let std = functions::std();

        let external_functions =
            concat_contract_functions(attributes.external_contracts.to_owned());

        let encoding = {
            let public_functions_batch = attributes
                .public_functions
                .iter()
                .filter(|attributes| !attributes.return_type.is_unit())
                .map(|attributes| vec![attributes.return_type.clone()])
                .collect::<Vec<_>>();

            let events_batch = attributes
                .events
                .iter()
                .map(|event| event.non_indexed_field_types())
                .collect::<Vec<_>>();

            let contracts_batch = external_functions
                .clone()
                .into_iter()
                .map(|function| function.param_types())
                .collect();

            let structs_batch = attributes
                .structs
                .clone()
                .into_iter()
                .map(|struct_| vec![FixedSize::Struct(struct_)])
                .collect::<Vec<Vec<_>>>();

            let batch = [
                public_functions_batch,
                events_batch,
                contracts_batch,
                structs_batch,
            ]
            .concat();
            functions::abi::batch_encode(batch)
        };
        let decoding = {
            let public_functions_batch: Vec<(FixedSize, AbiDecodeLocation)> = attributes
                .public_functions
                .to_owned()
                .into_iter()
                .map(|attributes| attributes.param_types())
                .collect::<Vec<_>>()
                .concat()
                .into_iter()
                .map(|typ| (typ, AbiDecodeLocation::Calldata))
                .collect();

            let init_params_batch =
                if let Some(init_attributes) = attributes.init_function.to_owned() {
                    init_attributes
                        .param_types()
                        .into_iter()
                        .map(|typ| (typ, AbiDecodeLocation::Memory))
                        .collect::<Vec<_>>()
                } else {
                    vec![]
                };

            let contracts_batch = external_functions
                .into_iter()
                .filter(|function| !function.return_type.is_unit())
                .map(|function| (function.return_type, AbiDecodeLocation::Memory))
                .collect();

            let batch = [public_functions_batch, init_params_batch, contracts_batch].concat();
            functions::abi::batch_decode(batch)
        };
        let contract_calls = {
            attributes
                .external_contracts
                .iter()
                .map(|contract| functions::contracts::calls(contract.to_owned()))
                .collect::<Vec<_>>()
                .concat()
        };

        let struct_apis = attributes
            .structs
            .iter()
            .map(|val| functions::structs::struct_apis(val.to_owned()))
            .collect::<Vec<_>>()
            .concat();

        return [std, encoding, decoding, contract_calls, struct_apis].concat();
    }

    panic!("missing contract attributes")
}

/// Concatenates the functions inside of each contracts.
fn concat_contract_functions(contracts: Vec<Contract>) -> Vec<FunctionAttributes> {
    contracts
        .into_iter()
        .map(|contract| contract.functions)
        .collect::<Vec<_>>()
        .concat()
}

/// Builds the set of function statements that are needed during as well as an
/// ABI dispatcher statement.
pub fn build_with_abi_dispatcher(
    context: &Context,
    contract: &Node<fe::Contract>,
) -> Vec<yul::Statement> {
    if let Some(attributes) = context.analysis.get_contract(contract) {
        let mut runtime = build(context, contract);
        runtime.push(abi_dispatcher::dispatcher(
            attributes.public_functions.to_owned(),
        ));

        return runtime;
    }

    panic!("missing contract attributes")
}
