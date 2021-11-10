use crate::constants::PANIC_FAILED_ASSERTION;
use crate::context::{ContractContext, FnContext};
use crate::mappers::{assignments, declarations, expressions};
use crate::names;
use crate::operations::data as data_operations;
use crate::operations::revert as revert_operations;
use crate::types::{AbiType, AsAbiType, EvmSized};
use fe_analyzer::context::ExpressionAttributes;
use fe_analyzer::namespace::items::{self, FunctionId};
use fe_analyzer::namespace::types::Type;
use fe_analyzer::AnalyzerDb;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use yultsur::*;

pub fn multiple_func_stmt(
    context: &mut FnContext,
    statements: &[Node<fe::FuncStmt>],
) -> Vec<yul::Statement> {
    statements
        .iter()
        .map(|statement| func_stmt(context, statement))
        .collect()
}

/// Builds a Yul function definition from a Fe function definition.
pub fn func_def(
    db: &dyn AnalyzerDb,
    context: &mut ContractContext,
    function_name: yul::Identifier,
    function: FunctionId,
) -> yul::Statement {
    // let function_name = names::func_name(&function.name(db));
    let sig = function.signature(db);
    let mut param_names = sig
        .params
        .iter()
        .map(|param| names::var_name(&param.name))
        .collect::<Vec<_>>();

    if sig.self_decl.is_some() && matches!(function.parent(db), Some(items::Class::Struct(_))) {
        // struct member functions take `$self` in yul
        param_names.insert(0, names::var_name("self"));
    }

    let mut fn_context = FnContext::new(db, context, function.body(db));
    let function_statements = multiple_func_stmt(&mut fn_context, &function.data(db).ast.kind.body);

    // all user-defined functions are given a return value during lowering
    function_definition! {
        function [function_name]([param_names...]) -> return_val {
            [function_statements...]
        }
    }
}

fn func_stmt(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    match &stmt.kind {
        fe::FuncStmt::Return { .. } => func_return(context, stmt),
        fe::FuncStmt::VarDecl { .. } => declarations::var_decl(context, stmt),
        fe::FuncStmt::Assign { .. } => assignments::assign(context, stmt),
        fe::FuncStmt::Emit { .. } => emit(context, stmt),
        fe::FuncStmt::AugAssign { .. } => panic!("AugAssign should be lowered"),
        fe::FuncStmt::For { .. } => for_loop(context, stmt),
        fe::FuncStmt::While { .. } => while_loop(context, stmt),
        fe::FuncStmt::If { .. } => if_statement(context, stmt),
        fe::FuncStmt::Unsafe(body) => {
            let yul_body = multiple_func_stmt(context, body);
            block_statement! {
                [yul_body...]
            }
        }
        fe::FuncStmt::Assert { .. } => assert(context, stmt),
        fe::FuncStmt::Expr { .. } => expr(context, stmt),
        fe::FuncStmt::Pass => statement! { pop(0) },
        fe::FuncStmt::Break => break_statement(context, stmt),
        fe::FuncStmt::Continue => continue_statement(context, stmt),
        fe::FuncStmt::Revert { .. } => revert(context, stmt),
    }
}

fn for_loop(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::For { target, iter, body } = &stmt.kind {
        let iterator = expressions::expr(context, iter);
        let target_var = names::var_name(&target.kind);
        let yul_body = multiple_func_stmt(context, body);
        return if let Some(ExpressionAttributes {
            typ: Type::Array(array),
            ..
        }) = context.expression_attributes(iter)
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

fn if_statement(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::If {
        test,
        body,
        or_else,
    } = &stmt.kind
    {
        let yul_test = expressions::expr(context, test);
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

fn expr(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Expr { value } = &stmt.kind {
        let expr = expressions::expr(context, value);

        statement! { pop([expr])}
    } else {
        unreachable!()
    }
}

fn revert(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Revert { error } = &stmt.kind {
        if let Some(error_expr) = error {
            let error_attributes = context
                .expression_attributes(error_expr)
                .expect("missing expression")
                .clone();

            if let Type::Struct(struct_) = &error_attributes.typ {
                context.contract.revert_errors.insert(struct_.clone());

                revert_operations::revert(
                    &struct_.name,
                    &struct_.as_abi_type(context.db),
                    expressions::expr(context, error_expr),
                )
            } else {
                panic!("trying to revert with non-struct expression")
            }
        } else {
            statement! { revert(0, 0) }
        }
    } else {
        unreachable!()
    }
}

fn emit(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Emit { args, .. } = &stmt.kind {
        let event_values = args
            .kind
            .iter()
            .map(|arg| expressions::expr(context, &arg.kind.value))
            .collect();

        if let Some(event) = context.emitted_event(stmt) {
            let event_fields: Vec<(AbiType, bool)> = event
                .fields
                .iter()
                .map(|field| {
                    (
                        field
                            .typ
                            .clone()
                            .expect("event field type error")
                            .as_abi_type(context.db),
                        field.is_indexed,
                    )
                })
                .collect();

            return data_operations::emit_event(&event.name, &event_fields, event_values);
        }

        panic!("missing event definition");
    }

    unreachable!()
}

fn assert(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Assert { test, msg } = &stmt.kind {
        let test = expressions::expr(context, test);
        match msg {
            Some(val) => {
                let msg = expressions::expr(context, val);
                let msg_attributes = context
                    .expression_attributes(val)
                    .expect("missing expression")
                    .clone();

                if let Type::String(string) = msg_attributes.typ {
                    let abi_type = string.as_abi_type(context.db);
                    context.contract.assert_strings.insert(string);

                    statement! {
                        if (iszero([test])) {
                            [revert_operations::error_revert(&abi_type, msg)]
                        }
                    }
                } else {
                    unreachable!()
                }
            }
            None => {
                statement! {
                    if (iszero([test])) {
                        [revert_operations::panic_revert(PANIC_FAILED_ASSERTION)]
                    }
                }
            }
        }
    } else {
        unreachable!()
    }
}

fn break_statement(_context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Break {} = &stmt.kind {
        return statement! { break };
    }

    unreachable!()
}

fn continue_statement(_context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
    if let fe::FuncStmt::Continue {} = &stmt.kind {
        return statement! { continue };
    }

    unreachable!()
}

fn func_return(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
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

fn while_loop(context: &mut FnContext, stmt: &Node<fe::FuncStmt>) -> yul::Statement {
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
