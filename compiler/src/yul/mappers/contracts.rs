use crate::errors::CompileError;
use crate::yul::constructor;
use crate::yul::mappers::functions;
use crate::yul::runtime;
use fe_common::utils::keccak::get_full_signature;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use fe_semantics::Context;
use yultsur::*;

/// Builds a Yul object from a Fe contract.
pub fn contract_def(
    context: &Context,
    stmt: &Spanned<fe::ModuleStmt>,
) -> Result<yul::Object, CompileError> {
    if let fe::ModuleStmt::ContractDef { name: _, body } = &stmt.node {
        let mut init = None;
        let mut user_functions = vec![];

        for stmt in body.iter() {
            if let (Some(attributes), fe::ContractStmt::FuncDef { name, .. }) =
                (context.get_function(stmt), &stmt.node)
            {
                if name.node == "__init__" {
                    init = Some((
                        functions::func_def(context, stmt)?,
                        attributes.param_types.clone(),
                    ))
                } else {
                    user_functions.push(functions::func_def(context, stmt)?)
                }
            }
        }

        let constructor = if let Some((init_func, init_params)) = init {
            let init_runtime = [runtime::build(context, stmt), user_functions.clone()].concat();
            constructor::build_with_init(init_func, init_params, init_runtime)
        } else {
            constructor::build()
        };

        let runtime = runtime::build_with_abi_dispatcher(context, stmt);

        let data = if let Some(attributes) = context.get_contract(stmt) {
            attributes
                .string_literals
                .clone()
                .into_iter()
                .map(|val| yul::Data {
                    name: get_full_signature(val.as_bytes()),
                    value: val,
                })
                .collect::<Vec<_>>()
        } else {
            vec![]
        };

        return Ok(yul::Object {
            name: identifier! { Contract },
            code: constructor,
            objects: vec![yul::Object {
                name: identifier! { runtime },
                code: yul::Code {
                    block: yul::Block {
                        statements: statements! {
                            [user_functions...]
                            [runtime...]
                        },
                    },
                },
                objects: vec![],
                // We can't reach to data objects in the "contract" hierachy so in order to have
                // the data objects available in both places we have to put them in both places.
                data: data.clone(),
            }],
            data,
        });
    }

    unreachable!()
}
