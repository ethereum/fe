use crate::context::{AnalyzerContext, ExpressionAttributes, NamedThing};
use crate::display::Displayable;
use crate::errors::{self, FatalError, TypeCoercionError};
use crate::namespace::items::Item;
use crate::namespace::scopes::{BlockScope, BlockScopeType};
use crate::namespace::types::{EventField, Type, TypeDowncast, TypeId};
use crate::traversal::types;
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
            let iter_type = expressions::expr(scope, iter, None)?.typ;
            let target_type = match iter_type.typ(scope.db()) {
                Type::Array(array) => array.inner,
                Type::SPtr(inner) if inner.as_array(scope.db()).is_some() => {
                    scope.add_diagnostic(errors::to_mem_error(iter.span));

                    // `if let` guards are experimental (rust issue #51114)
                    inner.as_array(scope.db()).unwrap().inner
                }
                _ => {
                    return Err(FatalError::new(scope.register_diag(errors::type_error(
                        "invalid `for` loop iterator type",
                        iter.span,
                        &"array",
                        &iter_type.display(scope.db()),
                    ))))
                }
            };
            scope.root.map_variable_type(target, target_type);

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
            let test_type = expressions::expr(scope, test, None)?.typ;
            error_if_not_bool(
                scope,
                test,
                test_type,
                "`if` statement condition is not bool",
            );
            traverse_statements(&mut scope.new_child(BlockScopeType::IfElse), body)?;
            traverse_statements(&mut scope.new_child(BlockScopeType::IfElse), or_else)?;
            Ok(())
        }
        _ => unreachable!(),
    }
}

fn error_if_not_bool(scope: &mut BlockScope, expr: &Node<fe::Expr>, typ: TypeId, msg: &str) {
    let bool_type = TypeId::bool(scope.db());
    if types::try_coerce_type(scope, Some(expr), typ, bool_type).is_err() {
        scope.type_error(msg, expr.span, bool_type, typ);
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
            let test_type = expressions::expr(scope, test, None)?.typ;
            error_if_not_bool(scope, test, test_type, "`while` loop condition is not bool");
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
                            typ: Ok(context_type),
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
                        vec!["Example: emit MyEvent(ctx, ...)".into()],
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
        let test_type = expressions::expr(scope, test, None)?.typ;
        error_if_not_bool(scope, test, test_type, "`assert` condition is not bool");

        if let Some(msg) = msg {
            let msg_attributes = expressions::expr(scope, msg, None)?;
            match msg_attributes.typ.typ(scope.db()) {
                Type::String(_) => {}
                Type::SPtr(inner) if matches!(inner.typ(scope.db()), Type::String(_)) => {
                    scope.add_diagnostic(errors::to_mem_error(msg.span));
                }
                _ => {
                    scope.error(
                        "`assert` reason must be a string",
                        msg.span,
                        &format!(
                            "this has type `{}`; expected a string",
                            msg_attributes.typ.display(scope.db())
                        ),
                    );
                }
            }
        }

        return Ok(());
    }

    unreachable!()
}

fn revert(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Revert { error } = &stmt.kind {
        if let Some(error_expr) = error {
            let error_attr = expressions::expr(scope, error_expr, None)?;
            if !error_attr.typ.deref(scope.db()).is_struct(scope.db()) {
                scope.error(
                    "`revert` error must be a struct",
                    error_expr.span,
                    &format!(
                        "this has type `{}`; expected a struct",
                        error_attr.typ.deref(scope.db()).display(scope.db())
                    ),
                );
            } else if error_attr.typ.is_sptr(scope.db()) {
                scope.fancy_error(
                    "`revert` value must be copied to memory",
                    vec![Label::primary(error_expr.span, "this value is in storage")],
                    vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                         format!("Example: `{}.to_mem()`", error_expr.kind),
                    ],
                );
            }
        }

        return Ok(());
    }

    unreachable!()
}

fn func_return(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Return { value } = &stmt.kind {
        let expected_type = scope.root.function_return_type()?;

        let value_attr = match value {
            Some(val) => expressions::expr(scope, val, Some(expected_type))?,
            None => ExpressionAttributes::new(TypeId::unit(scope.db())),
        };

        match types::try_coerce_type(scope, value.as_ref(), value_attr.typ, expected_type) {
            Err(TypeCoercionError::RequiresToMem) => {
                let value = value.clone().expect("to_mem required on unit type?");
                scope.add_diagnostic(errors::to_mem_error(value.span));
            }
            Err(TypeCoercionError::Incompatible) => {
                scope.error(
                    &format!(
                        "expected function to return `{}` but was `{}`",
                        expected_type.display(scope.db()),
                        value_attr.typ.display(scope.db())
                    ),
                    stmt.span,
                    "",
                );
            }
            Err(TypeCoercionError::SelfContractType) => {
                scope.add_diagnostic(errors::self_contract_type_error(
                    value.as_ref().unwrap().span,
                    &expected_type.display(scope.db()),
                ));
            }
            Ok(_) => {}
        }

        return Ok(());
    }

    unreachable!()
}
