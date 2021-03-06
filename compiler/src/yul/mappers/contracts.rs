use crate::errors::CompileError;
use crate::yul::constructor;
use crate::yul::mappers::functions;
use crate::yul::runtime;
use fe_analyzer::Context;
use fe_common::utils::keccak;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use yultsur::*;

/// Builds a Yul object from a Fe contract.
pub fn contract_def(
    context: &Context,
    stmt: &Spanned<fe::ModuleStmt>,
    created_contracts: Vec<yul::Object>,
) -> Result<yul::Object, CompileError> {
    if let fe::ModuleStmt::ContractDef { name, body } = &stmt.node {
        let mut init = None;
        let mut user_functions = vec![];
        let contract_name = name.node;

        // map user defined functions
        for stmt in body.iter() {
            if let (Some(attributes), fe::ContractStmt::FuncDef { name, .. }) =
                (context.get_function(stmt), &stmt.node)
            {
                if name.node == "__init__" {
                    init = Some((
                        functions::func_def(context, stmt)?,
                        attributes.param_types(),
                    ))
                } else {
                    user_functions.push(functions::func_def(context, stmt)?)
                }
            }
        }

        // build the contract's constructor
        let constructor = if let Some((init_func, init_params)) = init {
            let init_runtime = [runtime::build(context, stmt), user_functions.clone()].concat();
            constructor::build_with_init(contract_name, init_func, init_params, init_runtime)
        } else {
            constructor::build()
        };

        // build the contract's runtime
        let runtime = runtime::build_with_abi_dispatcher(context, stmt);

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

        let runtime = yul::Object {
            name: identifier! { runtime },
            code: yul::Code {
                block: yul::Block {
                    statements: statements! {
                        [user_functions...]
                        [runtime...]
                        // we must return, otherwise we'll enter into other objects
                        (return(0, 0))
                    },
                },
            },
            objects: created_contracts,
            // We can't reach to data objects in the "contract" hierachy so in order to have
            // the data objects available in both places we have to put them in both places.
            data: data.clone(),
        };

        return Ok(yul::Object {
            name: identifier! { (contract_name) },
            code: constructor,
            objects: vec![runtime],
            data,
        });
    }

    unreachable!()
}
