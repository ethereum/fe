use crate::context::{AnalyzerContext, ExpressionAttributes, Location, NamedThing};
use crate::errors::FatalError;
use crate::namespace::items::Item;
use crate::namespace::scopes::{BlockScope, BlockScopeType};
use crate::namespace::types::{Base, EventField, FixedSize, Type};
use crate::traversal::call_args::LabelPolicy;
use crate::traversal::{assignments, call_args, declarations, expressions};
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;

pub fn traverse_statements(
    scope: &mut BlockScope,
    body: &[Node<fe::FuncStmt>],
) -> Result<(), FatalError> {
    for stmt in body.iter() {
        func_stmt(scope, stmt)?
    }
    Ok(())
}

fn func_stmt(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    use fe::FuncStmt::*;
    match &stmt.kind {
        Return { .. } => func_return(scope, stmt),
        VarDecl { .. } => declarations::var_decl(scope, stmt),
        ConstantDecl { .. } => declarations::const_decl(scope, stmt),
        Assign { .. } => assignments::assign(scope, stmt),
        Emit { .. } => emit(scope, stmt),
        AugAssign { .. } => assignments::aug_assign(scope, stmt),
        For { .. } => for_loop(scope, stmt),
        While { .. } => while_loop(scope, stmt),
        If { .. } => if_statement(scope, stmt),
        Unsafe { .. } => unsafe_block(scope, stmt),
        Assert { .. } => assert(scope, stmt),
        Expr { value } => expressions::expr(scope, value, None).map(|_| ()),
        Pass => Ok(()),
        Revert { .. } => revert(scope, stmt),
        Break | Continue => {
            loop_flow_statement(scope, stmt);
            Ok(())
        }
    }
}

fn for_loop(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    match &stmt.kind {
        fe::FuncStmt::For { target, iter, body } => {
            // Make sure iter is in the function scope & it should be an array.
            let iter_type = expressions::assignable_expr(scope, iter, None)?.typ;
            let target_type = if let Type::Array(array) = iter_type {
                FixedSize::Base(array.inner)
            } else {
                return Err(FatalError::new(scope.type_error(
                    "invalid `for` loop iterator type",
                    iter.span,
                    &"array",
                    &iter_type,
                )));
            };

            scope
                .root
                .map_variable_type(target, target_type.clone().into());

            let mut body_scope = scope.new_child(BlockScopeType::Loop);
            // add_var emits a msg on err; we can ignore the Result.
            let _ = body_scope.add_var(&target.kind, target_type, false, target.span);

            // Traverse the statements within the `for loop` body scope.
            traverse_statements(&mut body_scope, body)
        }
        _ => unreachable!(),
    }
}

fn loop_flow_statement(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) {
    if !scope.inherits_type(BlockScopeType::Loop) {
        let stmt_name = match stmt.kind {
            fe::FuncStmt::Continue => "continue",
            fe::FuncStmt::Break => "break",
            _ => unreachable!(),
        };
        scope.error(
            &format!("`{}` outside of a loop", stmt_name),
            stmt.span,
            &format!(
                "`{}` can only be used inside of a `for` or `while` loop",
                stmt_name
            ),
        );
    }
}

fn if_statement(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    match &stmt.kind {
        fe::FuncStmt::If {
            test,
            body,
            or_else,
        } => {
            let test_type = expressions::value_expr(scope, test, None)?.typ;
            if test_type != Type::Base(Base::Bool) {
                scope.type_error(
                    "`if` statement condition is not bool",
                    test.span,
                    &Base::Bool,
                    &test_type,
                );
            }

            traverse_statements(&mut scope.new_child(BlockScopeType::IfElse), body)?;
            traverse_statements(&mut scope.new_child(BlockScopeType::IfElse), or_else)?;
            Ok(())
        }
        _ => unreachable!(),
    }
}

fn unsafe_block(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    match &stmt.kind {
        fe::FuncStmt::Unsafe(body) => {
            if scope.inherits_type(BlockScopeType::Unsafe) {
                scope.error(
                    "unnecessary `unsafe` block",
                    stmt.span,
                    "this `unsafe` block is nested inside another `unsafe` context",
                );
            }
            traverse_statements(&mut scope.new_child(BlockScopeType::Unsafe), body)
        }
        _ => unreachable!(),
    }
}

fn while_loop(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    match &stmt.kind {
        fe::FuncStmt::While { test, body } => {
            let test_type = expressions::value_expr(scope, test, None)?.typ;
            if test_type != Type::Base(Base::Bool) {
                scope.type_error(
                    "`while` loop condition is not bool",
                    test.span,
                    &Base::Bool,
                    &test_type,
                );
            }

            traverse_statements(&mut scope.new_child(BlockScopeType::Loop), body)?;
            Ok(())
        }
        _ => unreachable!(),
    }
}

fn emit(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Emit { name, args } = &stmt.kind {
        match scope.resolve_name(&name.kind, name.span)? {
            None => {
                scope.error(
                    &format!("undefined event: `{}`", name.kind),
                    name.span,
                    "undefined event",
                );
            }
            Some(NamedThing::Item(Item::Event(event))) => {
                scope.root.add_emit(stmt, event);
                // Check visibility of event.
                if !event.is_public(scope.db()) && event.module(scope.db()) != scope.module() {
                    let module_name = event.module(scope.db()).name(scope.db());
                    scope.fancy_error(
                             &format!(
                                 "the event `{}` is private",
                                 name.kind,
                             ),
                             vec![
                                 Label::primary(name.span, "this event is not `pub`"),
                                 Label::secondary(
                                     event.data(scope.db()).ast.span,
                                     format!("`{}` is defined here", name.kind)
                                 ),
                             ],
                             vec![
                                 format!("`{}` can only be used within `{}`", name.kind, module_name),
                                 format!("Hint: use `pub event {event}` to make `{event}` visible from outside of `{module}`", event=name.kind, module=module_name),
                             ],
                         );
                }
                if let Some(context_type) = scope.get_context_type() {
                    // we add ctx to the list of expected params
                    let params_with_ctx = [
                        vec![EventField {
                            name: "ctx".into(),
                            typ: Ok(FixedSize::Struct(context_type)),
                            is_indexed: false,
                        }],
                        event.typ(scope.db()).fields.clone(),
                    ]
                    .concat();
                    call_args::validate_named_args(
                        scope,
                        &name.kind,
                        name.span,
                        args,
                        &params_with_ctx,
                        LabelPolicy::AllowUnlabeledIfNameEqual,
                    )?;
                } else {
                    scope.fancy_error(
                        "`Context` is not defined",
                        vec![
                            Label::primary(
                                stmt.span,
                                "`ctx` must be defined and passed into the event",
                            ),
                            Label::secondary(
                                scope.parent_function().name_span(scope.db()),
                                "Note: declare `ctx` in this function signature",
                            ),
                            Label::secondary(
                                scope.parent_function().name_span(scope.db()),
                                "Example: `pub fn foo(ctx: Context, ...)`",
                            ),
                        ],
                        vec![
                            "Note: import context with `use std::context::Context`".into(),
                            "Example: emit MyEvent(ctx, ...)".into(),
                        ],
                    );
                }
            }
            Some(named_thing) => {
                scope.error(
                    "`emit` expects an event",
                    name.span,
                    &format!(
                        "`{}` is a {} name; expected an event",
                        &name.kind,
                        named_thing.item_kind_display_name(),
                    ),
                );
            }
        }
        return Ok(());
    }
    unreachable!()
}

fn assert(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Assert { test, msg } = &stmt.kind {
        let test_type = expressions::value_expr(scope, test, None)?.typ;
        if test_type != Type::Base(Base::Bool) {
            scope.type_error(
                "`assert` condition is not bool",
                test.span,
                &Base::Bool,
                &test_type,
            );
        }

        if let Some(msg) = msg {
            let msg_attributes = expressions::assignable_expr(scope, msg, None)?;
            if !matches!(msg_attributes.typ, Type::String(_)) {
                scope.error(
                    "`assert` reason must be a string",
                    msg.span,
                    &format!("this has type `{}`; expected a string", msg_attributes.typ),
                );
            }
        }

        return Ok(());
    }

    unreachable!()
}

fn revert(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Revert { error } = &stmt.kind {
        if let Some(error_expr) = error {
            let error_attributes = expressions::assignable_expr(scope, error_expr, None)?;
            if !matches!(error_attributes.typ, Type::Struct(_)) {
                scope.error(
                    "`revert` error must be a struct",
                    error_expr.span,
                    &format!(
                        "this has type `{}`; expected a struct",
                        error_attributes.typ
                    ),
                );
            }
        }

        return Ok(());
    }

    unreachable!()
}

fn func_return(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Return { value } = &stmt.kind {
        let expected_type = scope.root.function_return_type()?.into();

        let attributes = match value {
            Some(val) => expressions::assignable_expr(scope, val, Some(&expected_type))?,
            None => ExpressionAttributes::new(Type::unit(), Location::Value),
        };

        if attributes.typ != expected_type {
            scope.error(
                &format!(
                    "expected function to return `{}` but was `{}`",
                    expected_type, attributes.typ
                ),
                stmt.span,
                "",
            );
        }

        return Ok(());
    }

    unreachable!()
}
