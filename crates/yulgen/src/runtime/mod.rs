pub mod abi_dispatcher;
pub mod functions;
use crate::context::ContractContext;
use crate::types::{to_abi_types, AbiDecodeLocation, AbiType, AsAbiType};
use fe_analyzer::namespace::items::ContractId;
use fe_analyzer::namespace::types::FunctionSignature;
use fe_analyzer::AnalyzerDb;
use std::rc::Rc;
use yultsur::*;

/// Builds the set of function statements that are needed during runtime.
pub fn build(
    db: &dyn AnalyzerDb,
    context: &ContractContext,
    contract: ContractId,
) -> Vec<yul::Statement> {
    // TODO: db should provide easier access to these things.
    // Eg. contract.public_functions(db)

    let std = functions::std();

    let contract_name = contract.name(db);
    let external_contracts: Vec<ContractId> = contract
        .module(db)
        .all_contracts(db)
        .iter()
        .filter_map(|id| (id.name(db) != contract_name).then(|| *id))
        .collect();
    let external_functions = external_contracts
        .iter()
        .map(|id| {
            id.all_functions(db)
                .iter()
                .map(|func| func.signature(db))
                .collect::<Vec<Rc<FunctionSignature>>>()
        })
        .collect::<Vec<_>>()
        .concat();

    let public_functions: Vec<Rc<FunctionSignature>> = contract
        .public_functions(db)
        .values()
        .map(|func| func.signature(db))
        .collect();

    let encoding = {
        let public_functions_batch = public_functions
            .iter()
            .filter_map(|sig| {
                let typ = sig.return_type.clone().expect("return type error");
                (!typ.is_unit()).then(|| vec![typ.as_abi_type(db)])
            })
            .collect::<Vec<_>>();

        let events_batch: Vec<Vec<AbiType>> = contract
            .all_events(db)
            .iter()
            .map(|event| {
                event
                    .typ(db)
                    .fields
                    .iter()
                    .filter_map(|field| {
                        (!field.is_indexed).then(|| {
                            field
                                .typ
                                .clone()
                                .expect("event field type error")
                                .as_abi_type(db)
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<Vec<_>>>();

        let contracts_batch: Vec<Vec<_>> = external_functions
            .iter()
            .map(|fn_sig| to_abi_types(db, &fn_sig.param_types()))
            .collect();

        let assert_strings_batch = context
            .assert_strings
            .iter()
            .map(|val| vec![val.as_abi_type(db)])
            .collect::<Vec<_>>();

        let revert_errors_batch = context
            .revert_errors
            .iter()
            .map(|struct_| {
                struct_
                    .id
                    .all_fields(db)
                    .iter()
                    .map(|field| {
                        field
                            .typ(db)
                            .expect("struct field type error")
                            .as_abi_type(db)
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        let revert_panic_batch = vec![vec![AbiType::Uint { size: 32 }]];

        let structs_batch: Vec<Vec<AbiType>> = contract
            .module(db)
            .all_structs(db)
            .iter()
            .map(|struc| vec![struc.typ(db).as_abi_type(db)])
            .collect::<Vec<Vec<_>>>();

        let batch = [
            public_functions_batch,
            events_batch,
            contracts_batch,
            assert_strings_batch,
            revert_errors_batch,
            revert_panic_batch,
            structs_batch,
        ]
        .concat();
        functions::abi::batch_encode(batch)
    };
    let decoding = {
        let public_functions_batch = public_functions
            .iter()
            .map(|sig| {
                (
                    to_abi_types(db, &sig.param_types()),
                    AbiDecodeLocation::Calldata,
                )
            })
            .collect();

        let init_params_batch = if let Some(init_fn) = contract.init_function(db) {
            let sig = init_fn.signature(db);
            vec![(
                to_abi_types(db, &sig.param_types()),
                AbiDecodeLocation::Memory,
            )]
        } else {
            vec![]
        };

        let contracts_batch = external_functions
            .iter()
            .filter_map(|function| {
                let return_type = function.expect_return_type();
                (!return_type.is_unit())
                    .then(|| (vec![return_type.as_abi_type(db)], AbiDecodeLocation::Memory))
            })
            .collect();

        let batch = [public_functions_batch, init_params_batch, contracts_batch].concat();
        functions::abi::batch_decode(batch)
    };
    let contract_calls = {
        external_contracts
            .iter()
            .map(|contract| functions::contracts::calls(db, contract.to_owned()))
            .collect::<Vec<_>>()
            .concat()
    };

    let struct_apis = contract
        .module(db)
        .all_structs(db)
        .iter()
        .map(|struct_| {
            let fields = struct_
                .fields(db)
                .iter()
                .map(|(name, id)| {
                    (
                        name.to_string(),
                        id.typ(db).expect("struct field type error").as_abi_type(db),
                    )
                })
                .collect::<Vec<_>>();
            functions::structs::struct_apis(&struct_.name(db), &fields)
        })
        .collect::<Vec<_>>()
        .concat();

    let revert_calls_from_assert = context
        .assert_strings
        .iter()
        .map(|string| functions::revert::error_revert(&string.as_abi_type(db)))
        .collect::<Vec<_>>();

    let revert_calls = context
        .revert_errors
        .iter()
        .map(|_struct| functions::revert::revert(&_struct.name, &_struct.as_abi_type(db)))
        .collect::<Vec<_>>();

    let mut funcs = [
        std,
        encoding,
        decoding,
        contract_calls,
        revert_calls_from_assert,
        revert_calls,
        struct_apis,
    ]
    .concat();
    funcs.sort();
    funcs.dedup();
    funcs
}

/// Builds the set of function statements that are needed during runtime as well as an ABI dispatcher statement.
pub fn build_with_abi_dispatcher(
    db: &dyn AnalyzerDb,
    context: &ContractContext,
    contract: ContractId,
) -> Vec<yul::Statement> {
    let public_functions = contract
        .public_functions(db)
        .iter()
        .map(|(name, id)| {
            let sig = id.signature(db);
            let return_type = sig.return_type.clone().expect("fn return type error");
            let return_type = (!return_type.is_unit()).then(|| return_type.as_abi_type(db));
            let param_types = sig
                .params
                .iter()
                .map(|param| {
                    param
                        .typ
                        .clone()
                        .expect("fn param type error")
                        .as_abi_type(db)
                })
                .collect::<Vec<_>>();

            (name.to_string(), param_types, return_type)
        })
        .collect::<Vec<_>>();

    let mut runtime = build(db, context, contract);
    runtime.push(abi_dispatcher::dispatcher(public_functions));
    runtime
}
