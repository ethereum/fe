use crate::errors::CompileError;
use crate::yul::mappers::{
    assignments,
    declarations,
    expressions,
};
use crate::yul::names;
use crate::yul::operations::data as data_operations;
use crate::yul::utils;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use fe_semantics::namespace::types::{
    FeSized,
    Type,
};
use fe_semantics::{
    Context,
    ExpressionAttributes,
};
use yultsur::*;

pub fn multiple_func_stmt(
    context: &Context,
    statements: &[Spanned<fe::FuncStmt>],
) -> Result<Vec<yul::Statement>, CompileError> {
    statements
        .iter()
        .map(|statement| func_stmt(context, statement))
        .collect::<Result<Vec<_>, _>>()
}

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
        let function_name = names::func_name(name.node);
        let param_names = args.iter().map(|arg| func_def_arg(arg)).collect::<Vec<_>>();
        let function_statements = multiple_func_stmt(context, body)?;

        return if attributes.return_type.is_empty_tuple() {
            Ok(function_definition! {
                function [function_name]([param_names...]) {
                    [function_statements...]
                }
            })
        } else {
            Ok(function_definition! {
                function [function_name]([param_names...]) -> return_val {
                    [function_statements...]
                }
            })
        };
    }

    unreachable!()
}

fn func_def_arg(arg: &Spanned<fe::FuncDefArg>) -> yul::Identifier {
    let name = arg.node.name.node;

    names::var_name(name)
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
        fe::FuncStmt::For { .. } => for_loop(context, stmt),
        fe::FuncStmt::While { .. } => while_loop(context, stmt),
        fe::FuncStmt::If { .. } => if_statement(context, stmt),
        fe::FuncStmt::Assert { .. } => assert(context, stmt),
        fe::FuncStmt::Expr { .. } => expr(context, stmt),
        fe::FuncStmt::Pass => Ok(statement! { pop(0) }),
        fe::FuncStmt::Break => break_statement(context, stmt),
        fe::FuncStmt::Continue => continue_statement(context, stmt),
        fe::FuncStmt::Revert => revert(stmt),
    }
}

fn for_loop(
    context: &Context,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::For {
        target,
        iter,
        body,
        or_else: _,
    } = &stmt.node
    {
        let iterator = expressions::expr(context, iter)?;
        let target_var = names::var_name(expressions::expr_name_str(target)?);
        let yul_body = multiple_func_stmt(context, body)?;
        if let Some(ExpressionAttributes {
            typ: Type::Array(array),
            ..
        }) = context.get_expression(iter.span)
        {
            let size = literal_expression! { (array.dimension) };
            let inner_size = literal_expression! { (array.inner.size()) };
            return Ok(block_statement! {
                (for {(let i := 0)} (lt(i, [size])) {(i := add(i, 1))}
                {
                    // Below yul statement to load values from memory to `target_var`.
                    (let [target_var] := [expression! { mloadn([expression! { add([iterator], (mul(i, [inner_size.clone()]))) }], [inner_size]) }])
                    [yul_body...]
                })
            });
        } else {
            return Err(CompileError::static_str("missing iter expression"));
        }
    }
    unreachable!()
}

fn if_statement(
    context: &Context,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::If {
        test,
        body,
        or_else,
    } = &stmt.node
    {
        let yul_test = expressions::expr(context, &test)?;
        let yul_body = multiple_func_stmt(context, body)?;
        let yul_or_else = multiple_func_stmt(context, or_else)?;

        return Ok(switch! {
            switch ([yul_test])
            (case 1 {[yul_body...]})
            (case 0 {[yul_or_else...]})
        });
    }

    unreachable!()
}

fn expr(context: &Context, stmt: &Spanned<fe::FuncStmt>) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::Expr { value } = &stmt.node {
        let spanned = utils::spanned_expression(&stmt.span, value);
        let expr = expressions::expr(context, &spanned)?;
        if let Some(attributes) = context.get_expression(stmt.span) {
            if attributes.typ.is_empty_tuple() {
                return Ok(yul::Statement::Expression(expr));
            } else {
                return Ok(statement! { pop([expr])});
            }
        }
    }

    unreachable!()
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
                .map(|arg| expressions::call_arg(context, arg))
                .collect::<Result<_, _>>()?;

            if let Some(event) = context.get_emit(stmt) {
                return Ok(data_operations::emit_event(event.to_owned(), event_values));
            }

            return Err(CompileError::static_str("missing event definition"));
        }

        return Err(CompileError::static_str(
            "emit statements must contain a call expression",
        ));
    }

    unreachable!()
}

fn assert(context: &Context, stmt: &Spanned<fe::FuncStmt>) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::Assert { test, msg: _ } = &stmt.node {
        let test = expressions::expr(context, test)?;

        return Ok(statement! { if (iszero([test])) { (revert(0, 0)) } });
    }

    unreachable!()
}

fn break_statement(
    _context: &Context,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::Break {} = &stmt.node {
        return Ok(statement! { break });
    }

    unreachable!()
}

fn continue_statement(
    _context: &Context,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::Continue {} = &stmt.node {
        return Ok(statement! { continue });
    }

    unreachable!()
}

fn func_return(
    context: &Context,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::Return { value } = &stmt.node {
        return match value {
            Some(value) => {
                let value = expressions::expr(context, value)?;

                return Ok(block_statement! {
                    (return_val := [value])
                    (leave)
                });
            }
            None => Ok(statement! { leave }),
        };
    }

    unreachable!()
}

fn while_loop(
    context: &Context,
    stmt: &Spanned<fe::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let fe::FuncStmt::While {
        test,
        body,
        or_else: _,
    } = &stmt.node
    {
        let test = expressions::expr(context, test)?;
        let yul_body = multiple_func_stmt(context, body)?;

        return Ok(block_statement! {
            (for {} ([test]) {}
            {
                [yul_body...]
            })
        });
    }

    unreachable!()
}
