mod abi_dispatcher;
mod functions;

use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use fe_semantics::namespace::types::{
    AbiDecodeLocation,
    FixedSize,
};
use fe_semantics::Context;
use yultsur::*;

/// Builds the set of function statements that are needed during runtime.
pub fn build(context: &Context, contract: &Spanned<fe::ModuleStmt>) -> Vec<yul::Statement> {
    if let Some(attributes) = context.get_contract(contract) {
        let std = functions::std();
        let encoding = {
            let public_functions_batch = attributes
                .public_functions
                .iter()
                .filter(|attributes| !attributes.return_type.is_empty_tuple())
                .map(|attributes| vec![attributes.return_type.clone()])
                .collect::<Vec<_>>();

            let events_batch = attributes
                .events
                .iter()
                .map(|event| event.field_types())
                .collect::<Vec<_>>();

            let batch = [public_functions_batch, events_batch].concat();
            functions::abi::batch_encode(batch)
        };
        let decoding = {
            let public_functions_batch: Vec<(FixedSize, AbiDecodeLocation)> = attributes
                .public_functions
                .to_owned()
                .into_iter()
                .map(|attributes| attributes.param_types)
                .collect::<Vec<_>>()
                .concat()
                .into_iter()
                .map(|typ| (typ, AbiDecodeLocation::Calldata))
                .collect();

            let init_params_batch =
                if let Some(init_attributes) = attributes.init_function.to_owned() {
                    init_attributes
                        .param_types
                        .into_iter()
                        .map(|typ| (typ, AbiDecodeLocation::Memory))
                        .collect::<Vec<_>>()
                } else {
                    vec![]
                };

            let batch = [public_functions_batch, init_params_batch].concat();
            functions::abi::batch_decode(batch)
        };

        return [std, encoding, decoding].concat();
    }

    panic!("missing contract attributes")
}

/// Builds the set of function statements that are needed during as well as an
/// ABI dispatcher statement.
pub fn build_with_abi_dispatcher(
    context: &Context,
    contract: &Spanned<fe::ModuleStmt>,
) -> Vec<yul::Statement> {
    if let Some(attributes) = context.get_contract(contract) {
        let mut runtime = build(context, contract);
        runtime.push(abi_dispatcher::dispatcher(
            attributes.public_functions.to_owned(),
        ));

        return runtime;
    }

    panic!("missing contract attributes")
}
