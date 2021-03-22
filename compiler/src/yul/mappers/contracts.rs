use crate::yul::constructor;
use crate::yul::mappers::functions;
use crate::yul::runtime;
use fe_analyzer::Context;
use fe_common::utils::keccak;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use yultsur::*;

/// Builds a Yul object from a Fe contract.
pub fn contract_def(
    context: &Context,
    stmt: &Node<fe::ModuleStmt>,
    created_contracts: Vec<yul::Object>,
) -> yul::Object {
    if let fe::ModuleStmt::ContractDef { name, body } = &stmt.kind {
        let contract_name = name.kind;
        let mut init_function = None;
        let mut user_functions = vec![];

        // map user defined functions
        for stmt in body.iter() {
            if let (Some(attributes), fe::ContractStmt::FuncDef { name, .. }) =
                (context.get_function(stmt), &stmt.kind)
            {
                if name.kind == "__init__" {
                    init_function =
                        Some((functions::func_def(context, stmt), attributes.param_types()))
                } else {
                    user_functions.push(functions::func_def(context, stmt))
                }
            }
        }

        // build the set of functions needed during runtime
        let runtime_functions = runtime::build_with_abi_dispatcher(context, stmt);

        // build data objects for static strings (also for constants in the future)
        let data = if let Some(attributes) = context.get_contract(stmt) {
            attributes
                .string_literals
                .clone()
                .into_iter()
                .map(|val| yul::Data {
                    name: keccak::full(val.as_bytes()),
                    value: val,
                })
                .collect::<Vec<_>>()
        } else {
            vec![]
        };

        // create the runtime object
        let runtime_object = yul::Object {
            name: identifier! { runtime },
            code: yul::Code {
                block: yul::Block {
                    statements: statements! {
                        [user_functions...]
                        [runtime_functions...]
                        // we must return, otherwise we'll enter into other objects
                        (return(0, 0))
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
        let (constructor_code, constructor_objects) = if let Some((init_func, init_params)) =
            init_function
        {
            let init_runtime_functions = [runtime::build(context, stmt), user_functions].concat();
            let constructor_code = constructor::build_with_init(
                contract_name,
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
        return yul::Object {
            name: identifier! { (contract_name) },
            code: constructor_code,
            objects: constructor_objects,
            data,
        };
    }

    unreachable!()
}
