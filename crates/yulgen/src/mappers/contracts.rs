use crate::constructor;
use crate::context::ContractContext;
use crate::mappers::functions;
use crate::runtime;
use fe_analyzer::namespace::items::ContractId;
use fe_analyzer::AnalyzerDb;
use fe_common::utils::keccak;
use std::collections::HashMap;
use yultsur::*;

/// Builds a Yul object from a Fe contract.
pub fn contract_def(
    db: &dyn AnalyzerDb,
    contract: ContractId,
    contracts: &HashMap<String, yul::Object>,
) -> yul::Object {
    let contract_name = contract.name(db);
    let mut context = ContractContext::default();

    let init_function = contract.functions(db).get("__init__").map(|id| {
        (
            functions::func_def(db, &mut context, *id),
            id.signature(db)
                .params
                .iter()
                .map(|param| param.typ.clone().expect("fn param type error"))
                .collect(),
        )
    });

    let user_functions = contract
        .functions(db)
        .iter()
        .filter_map(|(name, function)| match name.as_str() {
            "__init__" => None,
            _ => Some(functions::func_def(db, &mut context, *function)),
        })
        .collect::<Vec<_>>();

    // build the set of functions needed during runtime
    let runtime_functions = runtime::build_with_abi_dispatcher(db, &context, contract);

    // build data objects for static strings (also for constants in the future)
    let data = context
        .string_literals
        .iter()
        .map(|val| yul::Data {
            name: keccak::full(val.as_bytes()),
            value: val.clone(),
        })
        .collect::<Vec<_>>();

    // Map the set of created contract names to their Yul objects so they can be
    // included in the Yul contract that deploys them.
    let created_contracts = context
        .created_contracts
        .iter()
        .map(|contract_name| contracts[contract_name].clone())
        .collect::<Vec<_>>();

    // create the runtime object
    let runtime_object = yul::Object {
        name: identifier! { runtime },
        code: yul::Code {
            block: yul::Block {
                statements: statements! {
                    [user_functions...]
                    [runtime_functions...]
                },
            },
        },
        objects: created_contracts.clone(),
        // We can't reach to data objects in the "contract" hierachy so in order to have
        // the data objects available in both places we have to put them in both places.
        data: data.clone(),
    };

    // Build the code and and objects fields for the constructor object.
    //
    // If there is an `__init__` function defined, we must include everything that
    // is in the runtime object in the constructor object too. This is so
    // user-defined functions can be called from `__init__`.
    let (constructor_code, constructor_objects) =
        if let Some((init_func, init_params)) = init_function {
            let init_runtime_functions =
                [runtime::build(db, &context, contract), user_functions].concat();
            let constructor_code = constructor::build_with_init(
                db,
                &contract_name,
                init_func,
                init_params,
                init_runtime_functions,
            );

            (
                constructor_code,
                [vec![runtime_object], created_contracts].concat(),
            )
        } else {
            let constructor_code = constructor::build();

            (constructor_code, vec![runtime_object])
        };

    // We return the contract initialization object.
    yul::Object {
        name: identifier! { (contract_name) },
        code: constructor_code,
        objects: constructor_objects,
        data,
    }
}
