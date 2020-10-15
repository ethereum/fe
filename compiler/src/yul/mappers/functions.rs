use crate::errors::CompileError;
use crate::yul::mappers::expressions::spanned_expression;
use crate::yul::mappers::{
    assignments,
    declarations,
    expressions,
};
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use fe_semantics::namespace::types::{
    FeSized,
    FixedSize,
};
use fe_semantics::Context;
use yultsur::*;

/// Builds a Yul function definition from a Fe function definition.
pub fn func_def(
    context: &Context,
    def: &Spanned<fe::ContractStmt>,
) -> Result<yul::Statement, CompileError> {
    if let (
        Some(attributes),
        fe::ContractStmt::FuncDef {
            qual: _,
            name,
            args,
            return_type: _,
            body,
        },
    ) = (context.get_function(def).to_owned(), &def.node)
    {
        let function_name = identifier! {(name.node)};
        let param_names = args
            .iter()
            .map(|arg| func_def_arg(arg))
            .collect::<Result<Vec<_>, _>>()?;
        let function_statements = body
            .iter()
            .map(|statement| func_stmt(context, statement))
            .collect::<Result<Vec<_>, _>>()?;

        // Different return types require slightly different functions.
        if let Some(return_type) = attributes.return_type.to_owned() {
            return match return_type {
                // Base types are returned by value. All memory used by the function is cleared.
                FixedSize::Base(_) => Ok(function_definition! {
                    function [function_name]([param_names...]) -> return_val {
                        (let ptr := avail())
                        [function_statements...]
                        (free(ptr))
                    }
                }),
                // Arrays need to keep memory allocated after completing.
                // FIXME: Copy arrays to the lowest available pointer to save memory.
                FixedSize::Array(array) => {
                    let size = literal_expression! {(array.size())};

                    Ok(function_definition! {
                        function [function_name]([param_names...]) -> return_val {
                            [function_statements...]
                            (free((add(return_val, [size]))))
                        }
                    })
                }
            };
        }

        // Nothing is returned. All memory used by the function is freed.
        return Ok(function_definition! {
            function [function_name]([param_names...]) {
                (let ptr := avail())
                [function_statements...]
                (free(ptr))
            }
        });
    }

    unreachable!()
}

fn func_def_arg(arg: &Spanned<fe::FuncDefArg>) -> Result<yul::Identifier, CompileError> {
    let name = arg.node.name.node.to_string();
    Ok(identifier! {(name)})
}

fn func_stmt(
    context: &Context,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    match &stmt.node {
        fe::FuncStmt::Return { .. } => func_return(context, stmt),
        fe::FuncStmt::VarDecl { .. } => declarations::var_decl(context, stmt),
        fe::FuncStmt::Assign { .. } => assignments::assign(context, stmt),
        fe::FuncStmt::Emit { .. } => emit(context, stmt),
        fe::FuncStmt::AugAssign { .. } => unimplemented!(),
        fe::FuncStmt::For { .. } => unimplemented!(),
        fe::FuncStmt::While { .. } => unimplemented!(),
        fe::FuncStmt::If { .. } => unimplemented!(),
        fe::FuncStmt::Assert { .. } => unimplemented!(),
        fe::FuncStmt::Expr { .. } => unimplemented!(),
        fe::FuncStmt::Pass => unimplemented!(),
        fe::FuncStmt::Break => unimplemented!(),
        fe::FuncStmt::Continue => unimplemented!(),
        fe::FuncStmt::Revert => revert(stmt),
    }
}

fn revert(stmt: &Spanned<fe::FuncStmt>) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::Revert = &stmt.node {
        return Ok(statement! { revert(0, 0) });
    }

    unreachable!()
}

fn emit(context: &Context, stmt: &Spanned<fe::FuncStmt>) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::Emit { value } = &stmt.node {
        if let fe::Expr::Call { func: _, args } = &value.node {
            let event_values = args
                .node
                .iter()
                .map(|arg| call_arg(context, arg))
                .collect::<Result<_, _>>()?;

            if let Some(event) = context.get_emit(stmt) {
                return Ok(event.emit(event_values));
            }

            return Err(CompileError::static_str("missing event definition"));
        }

        return Err(CompileError::static_str(
            "emit statements must contain a call expression",
        ));
    }

    unreachable!()
}

fn call_arg(
    context: &Context,
    arg: &Spanned<fe::CallArg>,
) -> Result<yul::Expression, CompileError> {
    match &arg.node {
        fe::CallArg::Arg(value) => {
            let spanned = spanned_expression(&arg.span, value);
            expressions::expr(context, &spanned)
        }
        fe::CallArg::Kwarg(fe::Kwarg { name: _, value }) => expressions::expr(context, value),
    }
}

fn func_return(
    context: &Context,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::Return { value } = &stmt.node {
        match value {
            Some(value) => {
                let value = expressions::expr(context, value)?;

                return Ok(yul::Statement::Block(block! {
                    (return_val := [value])
                    (leave)
                }));
            }
            None => return Ok(statement! { leave }),
        }
    }

    unreachable!()
}
