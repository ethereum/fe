use crate::constructor;
use crate::context::FnContext;
use crate::db::YulgenDb;
use crate::mappers::functions::multiple_func_stmt;
use crate::runtime::{abi_dispatcher, functions};
use crate::types::{AbiDecodeLocation, AsAbiType};
use fe_analyzer::builtins::ValueMethod;
use fe_analyzer::context::CallType;
use fe_analyzer::namespace::items::{walk_local_dependencies, ContractId, DepGraph, Item, TypeDef};
use fe_analyzer::namespace::types::FixedSize;
use fe_common::utils::keccak;
use indexmap::IndexSet;
use smol_str::SmolStr;
use yultsur::*;

pub fn contract_object(db: &dyn YulgenDb, contract: ContractId) -> yul::Object {
    let adb = db.upcast();

    let runtime_object = db.contract_runtime_object(contract);

    let contract_name = contract.name(adb);
    if let Some(init_fn) = contract.init_function(adb) {
        let (functions, data, objects) =
            build_dependency_graph(db, &init_fn.dependency_graph(adb), Item::Function(init_fn));

        let (params, _) = db.function_sig_abi_types(init_fn);
        let code = constructor::build_with_init(
            &contract_name,
            &db.function_yul_name(init_fn),
            &params,
            functions,
            init_fn.signature(adb).ctx_decl.is_some(),
        );

        // Return constructor object
        yul::Object {
            name: identifier! { (contract_name) },
            code,
            objects: [vec![runtime_object], objects].concat(),
            data,
        }
    } else {
        yul::Object {
            name: identifier! { (contract_name) },
            code: constructor::build(),
            objects: vec![runtime_object],
            data: vec![],
        }
    }
}

pub fn contract_runtime_object(db: &dyn YulgenDb, contract: ContractId) -> yul::Object {
    let adb = db.upcast();

    let (mut functions, data, objects) = build_dependency_graph(
        db,
        &contract.runtime_dependency_graph(adb),
        Item::Type(TypeDef::Contract(contract)),
    );

    // This can all be replaced with a call to the contract's `__call__` function once the
    // dispatching code has been moved into lowering.
    //
    // For now, we must do the following:
    // - check if a `__call__` function has been defined in the contract and, if so, we map its
    // body into a Yul function named `$$__call__`
    // - if a `__call__` function is not defined in the contract, we generate a `$$__call__`
    // function that dispatches calls using the Solidity ABI
    // - call the `$$__call__` function
    let call_fn_ident = identifier! { ("$$__call__") };
    if let Some(call_fn) = contract.call_function(adb) {
        let mut fn_context = FnContext::new(db, call_fn.body(adb));
        let function_statements =
            multiple_func_stmt(&mut fn_context, &call_fn.data(adb).ast.kind.body);
        let call_fn_yul = function_definition! {
            function [call_fn_ident.clone()]() {
                // the function body will contain one or more `return_val` assignments
                (let return_val := 0)
                [function_statements...]
            }
        };
        functions.push(call_fn_yul);
    } else {
        functions.extend(db.contract_abi_dispatcher(contract));
    }
    functions.sort();
    functions.dedup();

    yul::Object {
        name: identifier! { runtime },
        code: yul::Code {
            block: yul::Block {
                statements: statements! {
                    [functions...]
                    ([call_fn_ident]())
                },
            },
        },
        objects,
        data,
    }
}

/// Dispatch function and required encode/decode functions.
pub fn contract_abi_dispatcher(db: &dyn YulgenDb, contract: ContractId) -> Vec<yul::Statement> {
    let adb = db.upcast();
    let public_functions = contract
        .public_functions(adb)
        .values()
        .map(|id| {
            let bare_name = id.name(adb);
            let qualified_name = db.function_yul_name(*id);
            let expects_ctx = id.signature(db.upcast()).ctx_decl.is_some();
            let (param_types, return_type) = db.function_sig_abi_types(*id);
            (
                bare_name,
                qualified_name,
                param_types,
                return_type,
                expects_ctx,
            )
        })
        .collect::<Vec<_>>();

    let mut fns =
        public_functions
            .iter()
            .fold(vec![], |mut fns, (_, _, param_types, return_type, _)| {
                fns.extend(functions::abi::decode_functions(
                    param_types,
                    AbiDecodeLocation::Calldata,
                ));
                if let Some(return_type) = return_type {
                    fns.push(functions::abi::encode(&[return_type.clone()]));
                }
                fns
            });
    fns.push(abi_dispatcher::dispatcher(&public_functions));
    fns.sort();
    fns.dedup();
    fns
}

fn build_dependency_graph(
    db: &dyn YulgenDb,
    graph: &DepGraph,
    root: Item,
) -> (Vec<yul::Statement>, Vec<yul::Data>, Vec<yul::Object>) {
    let adb = db.upcast(); // AnalyzerDb

    let mut string_literals = IndexSet::<SmolStr>::new();
    let mut created_contracts = IndexSet::<ContractId>::new();

    // We need all of the "std" yul functions,
    // because we can't yet track which ones are needed.
    let mut yulfns = functions::std();

    walk_local_dependencies(graph, root, |item| {
        match item {
            Item::Function(function) => {
                yulfns.push(db.function_def(function));

                let body = function.body(adb);
                for calltype in body.calls.values() {
                    match calltype {
                        CallType::External { function: fun, .. } => {
                            yulfns.extend(db.function_external_call_fn(*fun));
                        }
                        CallType::BuiltinValueMethod {
                            method: ValueMethod::AbiEncode,
                            typ,
                        } => {
                            let typ: FixedSize = typ
                                .clone()
                                .try_into()
                                .expect("abi_encode non-fixedsize type");
                            yulfns.push(functions::abi::encode(&[typ.as_abi_type(adb)]));
                        }
                        CallType::BuiltinAssociatedFunction { contract, .. } => {
                            created_contracts.insert(*contract);
                        }
                        _ => {}
                    }
                }

                for struct_ in db.function_revert_errors(function).iter() {
                    yulfns.push(functions::abi::encode(&[db.struct_abi_type(*struct_)]));
                    yulfns.push(functions::revert::revert(
                        &struct_.name(adb),
                        &db.struct_abi_type(*struct_),
                    ));
                }
                for string_type in db.function_assert_string_types(function).iter() {
                    yulfns.push(functions::revert::error_revert(string_type));
                    yulfns.push(functions::abi::encode(&[string_type.clone()]));
                }
                string_literals.extend(body.string_literals.iter().cloned());
            }
            Item::Type(TypeDef::Struct(struct_)) => {
                // We don't know which struct fields are actually accessed, so we need
                // accessor functions for all of them.
                yulfns.extend(db.struct_api_fns(struct_));
            }
            Item::Event(event) => {
                yulfns.push(functions::abi::encode(&db.event_idx_abi_types(event)));
            }
            _ => {}
        }
    });

    yulfns.sort();
    yulfns.dedup();

    let data = string_literals
        .into_iter()
        .map(|string| yul::Data {
            name: keccak::full(string.as_bytes()),
            value: string.to_string(),
        })
        .collect();

    let objects = created_contracts
        .iter()
        .map(|contract| db.contract_object(*contract))
        .collect();

    (yulfns, data, objects)
}
