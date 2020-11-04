use crate::errors::CompileError;
use crate::yul::constructor;
use crate::yul::mappers::functions;
use crate::yul::runtime::abi as runtime_abi;
use crate::yul::runtime::functions as runtime_functions;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use fe_semantics::{
    Context,
    RuntimeOperations,
};
use yultsur::*;

/// Builds a Yul object from a Fe contract.
pub fn contract_def(
    context: &Context,
    stmt: &Spanned<fe::ModuleStmt>,
) -> Result<yul::Object, CompileError> {
    if let (Some(attributes), fe::ModuleStmt::ContractDef { name: _, body }) =
        (context.get_contract(stmt), &stmt.node)
    {
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

        let mut statements = vec![];
        statements.append(&mut user_functions);
        statements.append(&mut runtime_functions::std());
        statements.append(&mut build_runtime_functions(
            attributes.runtime_operations.to_owned(),
        ));
        statements.push(runtime_abi::dispatcher(
            attributes.public_functions.to_owned(),
        )?);

        return Ok(yul::Object {
            name: identifier! { Contract },
            code: constructor::build(init),
            objects: vec![yul::Object {
                name: identifier! { runtime },
                code: yul::Code {
                    block: yul::Block { statements },
                },
                objects: vec![],
            }],
        });
    }

    unreachable!()
}

fn build_runtime_functions(functions: Vec<RuntimeOperations>) -> Vec<yul::Statement> {
    functions
        .iter()
        .map(|function| match function {
            RuntimeOperations::AbiEncode { params } => {
                runtime_functions::abi_encode(params.clone())
            }
        })
        .collect()
}
