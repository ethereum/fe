use crate::mappers::{assignments, declarations, expressions};
use crate::names;
use crate::operations::data as data_operations;
use crate::Context;
use fe_analyzer::context::ExpressionAttributes;
use fe_analyzer::namespace::types::{FeSized, Type};
use fe_parser::ast as fe;
use fe_parser::node::Node;
use yultsur::*;

pub fn multiple_func_stmt(
    context: &mut Context,
    statements: &[Node<fe::FuncStmt>],
) -> Vec<yul::Statement> {
    statements
        .iter()
        .map(|statement| func_stmt(context, statement))
        .collect()
}

/// Builds a Yul function definition from a Fe function definition.
pub fn func_def(context: &mut Context, def: &Node<fe::Function>) -> yul::Statement {
    let fe::Function {
        name, args, body, ..
    } = &def.kind;
    let function_name = names::func_name(&name.kind);
    let param_names = args.iter().map(|arg| func_def_arg(arg)).collect::<Vec<_>>();
    let function_statements = multiple_func_stmt(context, body);

    // all user-defined functions are given a return value during lowering
    function_definition! {
        function [function_name]([param_names...]) -> return_val {
            [function_statements...]
        }
    }
}

fn func_def_arg(arg: &Node<fe::FunctionArg>) -> yul::Identifier {
    let name = &arg.kind.name.kind;

    names::var_name(name)
}

fn func_stmt(context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    match &stmt.kind {
        fe::FuncStmt::Return { .. } => func_return(context, stmt),
        fe::FuncStmt::VarDecl { .. } => declarations::var_decl(context, stmt),
        fe::FuncStmt::Assign { .. } => assignments::assign(context, stmt),
        fe::FuncStmt::Emit { .. } => emit(context, stmt),
        fe::FuncStmt::AugAssign { .. } => panic!("AugAssign should be lowered"),
        fe::FuncStmt::For { .. } => for_loop(context, stmt),
        fe::FuncStmt::While { .. } => while_loop(context, stmt),
        fe::FuncStmt::If { .. } => if_statement(context, stmt),
        fe::FuncStmt::Assert { .. } => assert(context, stmt),
        fe::FuncStmt::Expr { .. } => expr(context, stmt),
        fe::FuncStmt::Pass => statement! { pop(0) },
        fe::FuncStmt::Break => break_statement(context, stmt),
        fe::FuncStmt::Continue => continue_statement(context, stmt),
        fe::FuncStmt::Revert => revert(stmt),
    }
}

fn for_loop(context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::For { target, iter, body } = &stmt.kind {
        let iterator = expressions::expr(context, iter);
        let target_var = names::var_name(&target.kind);
        let yul_body = multiple_func_stmt(context, body);
        return if let Some(ExpressionAttributes {
            typ: Type::Array(array),
            ..
        }) = context.analysis.get_expression(iter)
        {
            let size = literal_expression! { (array.size) };
            let inner_size = literal_expression! { (array.inner.size()) };
            block_statement! {
                (for {(let i := 0)} (lt(i, [size])) {(i := add(i, 1))}
                {
                    // Below yul statement to load values from memory to `target_var`.
                    (let [target_var] := [expression! { mloadn([expression! { add([iterator], (mul(i, [inner_size.clone()]))) }], [inner_size]) }])
                    [yul_body...]
                })
            }
        } else {
            panic!("missing iter expression")
        };
    }
    unreachable!()
}

fn if_statement(context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::If {
        test,
        body,
        or_else,
    } = &stmt.kind
    {
        let yul_test = expressions::expr(context, &test);
        let yul_body = multiple_func_stmt(context, body);
        let yul_or_else = multiple_func_stmt(context, or_else);

        return switch! {
            switch ([yul_test])
            (case 1 {[yul_body...]})
            (case 0 {[yul_or_else...]})
        };
    }

    unreachable!()
}

fn expr(context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Expr { value } = &stmt.kind {
        let expr = expressions::expr(context, value);

        statement! { pop([expr])}
    } else {
        unreachable!()
    }
}

fn revert(stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Revert = &stmt.kind {
        return statement! { revert(0, 0) };
    }

    unreachable!()
}

fn emit(context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Emit { args, .. } = &stmt.kind {
        let event_values = args
            .kind
            .iter()
            .map(|arg| expressions::expr(context, &arg.kind.value))
            .collect();

        if let Some(event) = context.analysis.get_emit(stmt) {
            return data_operations::emit_event(event.to_owned(), event_values);
        }

        panic!("missing event definition");
    }

    unreachable!()
}

fn assert(context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Assert { test, msg } = &stmt.kind {
        let test = expressions::expr(context, test);
        return match msg {
            Some(val) => {
                let msg = expressions::expr(context, val);
                statement! { if (iszero([test])) { (revert_with_reason_string([msg])) } }
            }
            None => statement! { if (iszero([test])) { (revert(0, 0)) } },
        };
    }

    unreachable!()
}

fn break_statement(_context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Break {} = &stmt.kind {
        return statement! { break };
    }

    unreachable!()
}

fn continue_statement(_context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Continue {} = &stmt.kind {
        return statement! { continue };
    }

    unreachable!()
}

fn func_return(context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Return { value } = &stmt.kind {
        let value = value
            .as_ref()
            .expect("valueless return made it to Yul codegen");
        let value = expressions::expr(context, value);

        block_statement! {
            (return_val := [value])
            (leave)
        }
    } else {
        unreachable!()
    }
}

fn while_loop(context: &mut Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::While { test, body } = &stmt.kind {
        let test = expressions::expr(context, test);
        let yul_body = multiple_func_stmt(context, body);

        return block_statement! {
            (for {} ([test]) {}
            {
                [yul_body...]
            })
        };
    }

    unreachable!()
}
