use crate::yul::mappers::{
    assignments,
    declarations,
    expressions,
};
use crate::yul::names;
use crate::yul::operations::data as data_operations;
use fe_analyzer::namespace::types::{
    FeSized,
    Type,
};
use fe_analyzer::{
    Context,
    ExpressionAttributes,
};
use fe_parser::ast as fe;
use fe_parser::node::Node;
use yultsur::*;

pub fn multiple_func_stmt(
    context: &Context,
    statements: &[Node<fe::FuncStmt>],
) -> Vec<yul::Statement> {
    statements
        .iter()
        .map(|statement| func_stmt(context, statement))
        .collect()
}

/// Builds a Yul function definition from a Fe function definition.
pub fn func_def(context: &Context, def: &Node<fe::ContractStmt>) -> yul::Statement {
    if let (
        Some(attributes),
        fe::ContractStmt::FuncDef {
            qual: _,
            name,
            args,
            return_type: _,
            body,
        },
    ) = (context.get_function(def).to_owned(), &def.kind)
    {
        let function_name = names::func_name(&name.kind);
        let param_names = args.iter().map(|arg| func_def_arg(arg)).collect::<Vec<_>>();
        let function_statements = multiple_func_stmt(context, body);

        return if attributes.return_type.is_empty_tuple() {
            function_definition! {
                function [function_name]([param_names...]) {
                    [function_statements...]
                }
            }
        } else {
            function_definition! {
                function [function_name]([param_names...]) -> return_val {
                    [function_statements...]
                }
            }
        };
    }

    unreachable!()
}

fn func_def_arg(arg: &Node<fe::FuncDefArg>) -> yul::Identifier {
    let name = &arg.kind.name.kind;

    names::var_name(name)
}

fn func_stmt(context: &Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    match &stmt.kind {
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
        fe::FuncStmt::Pass => statement! { pop(0) },
        fe::FuncStmt::Break => break_statement(context, stmt),
        fe::FuncStmt::Continue => continue_statement(context, stmt),
        fe::FuncStmt::Revert => revert(stmt),
    }
}

fn for_loop(context: &Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::For {
        target,
        iter,
        body,
        or_else: _,
    } = &stmt.kind
    {
        let iterator = expressions::expr(context, iter);
        let target_var = names::var_name(&expressions::expr_name_string(target));
        let yul_body = multiple_func_stmt(context, body);
        return if let Some(ExpressionAttributes {
            typ: Type::Array(array),
            ..
        }) = context.get_expression(iter)
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

fn if_statement(context: &Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
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

fn expr(context: &Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Expr { value } = &stmt.kind {
        let expr = expressions::expr(context, value);

        let attributes = context.get_expression(value).expect("missing attributes");

        return if attributes.typ.is_empty_tuple() {
            yul::Statement::Expression(expr)
        } else {
            statement! { pop([expr])}
        };
    }

    unreachable!()
}

fn revert(stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Revert = &stmt.kind {
        return statement! { revert(0, 0) };
    }

    unreachable!()
}

fn emit(context: &Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Emit { value } = &stmt.kind {
        if let fe::Expr::Call { func: _, args } = &value.kind {
            let event_values = args
                .kind
                .iter()
                .map(|arg| expressions::call_arg(context, arg))
                .collect();

            if let Some(event) = context.get_emit(stmt) {
                return data_operations::emit_event(event.to_owned(), event_values);
            }

            panic!("missing event definition");
        }

        panic!("emit statements must contain a call expression",);
    }

    unreachable!()
}

fn assert(context: &Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Assert { test, msg: _ } = &stmt.kind {
        let test = expressions::expr(context, test);

        return statement! { if (iszero([test])) { (revert(0, 0)) } };
    }

    unreachable!()
}

fn break_statement(_context: &Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Break {} = &stmt.kind {
        return statement! { break };
    }

    unreachable!()
}

fn continue_statement(_context: &Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Continue {} = &stmt.kind {
        return statement! { continue };
    }

    unreachable!()
}

fn func_return(context: &Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Return { value } = &stmt.kind {
        return match value {
            Some(value) => {
                // Ensure `return ()` is handled as if the function does not return
                let attributes = context.get_expression(value).expect("Missing attributes");
                if attributes.is_empty_tuple() {
                    return statement! { leave };
                }

                let value = expressions::expr(context, value);
                return block_statement! {
                    (return_val := [value])
                    (leave)
                };
            }
            None => statement! { leave },
        };
    }

    unreachable!()
}

fn while_loop(context: &Context, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::While {
        test,
        body,
        or_else: _,
    } = &stmt.kind
    {
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
