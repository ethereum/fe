use crate::errors::CompileError;
use crate::yul::mappers::{
    constructor,
    functions,
};
use crate::yul::runtime::abi as runtime_abi;
use crate::yul::runtime::functions as runtime_functions;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use fe_semantics::Context;
use yultsur::*;

/// Builds a Yul object from a Fe contract.
pub fn contract_def(
    context: &Context,
    stmt: &Spanned<fe::ModuleStmt>,
) -> Result<yul::Object, CompileError> {
    if let (Some(functions), fe::ModuleStmt::ContractDef { name: _, body }) =
        (context.get_contract(stmt), &stmt.node)
    {
        let mut statements = body.iter().try_fold::<_, _, Result<_, CompileError>>(
            vec![],
            |mut statements, stmt| {
                match &stmt.node {
                    fe::ContractStmt::ContractField { .. } => {}
                    fe::ContractStmt::EventDef { .. } => {}
                    fe::ContractStmt::FuncDef { .. } => {
                        statements.push(functions::func_def(context, stmt)?)
                    }
                };

                Ok(statements)
            },
        )?;

        statements.append(&mut runtime_functions::all());
        statements.push(runtime_abi::dispatcher(functions.to_owned())?);

        return Ok(yul::Object {
            name: identifier! { Contract },
            code: constructor::runtime(),
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
