use crate::context::{Context, ExpressionAttributes, FunctionAttributes, Label, Location};
use crate::errors::SemanticError;
use crate::namespace::scopes::{BlockScope, BlockScopeType, ContractScope, Scope, Shared};
use crate::namespace::types::{Base, FixedSize, Type};
use crate::traversal::{assignments, declarations, expressions, types};
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::rc::Rc;

/// Gather context information for a function definition and check for type
/// errors. Does not inspect the function body.
pub fn func_def(
    contract_scope: Shared<ContractScope>,
    context: &mut Context,
    def: &Node<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::FuncDef {
        is_pub,
        name,
        args,
        return_type: return_type_node,
        ..
    } = &def.kind
    {
        let name = &name.kind;
        let function_scope = BlockScope::from_contract_scope(name, Rc::clone(&contract_scope));

        let params = args
            .iter()
            .map(|arg| func_def_arg(Rc::clone(&function_scope), context, arg))
            .collect::<Result<Vec<_>, _>>()?;

        let mut return_type = return_type_node
            .as_ref()
            .map(|typ| {
                types::type_desc_fixed_size(
                    &Scope::Block(Rc::clone(&function_scope)),
                    context,
                    &typ,
                )
            })
            .transpose()?
            .unwrap_or_else(FixedSize::unit);

        // `__init__` must not return any type other than `()`.
        if name == "__init__" && !return_type.is_unit() {
            context.fancy_error(
                "`__init__` function has incorrect return type",
                vec![Label::primary(
                    return_type_node.as_ref().unwrap().span,
                    "return type should be `()`",
                )],
                vec![
                    "Hint: Remove the return type specification.".to_string(),
                    format!(
                        "Example: `{}def __init__():`",
                        if *is_pub { "pub " } else { "" }
                    ),
                ],
            );
            return_type = FixedSize::unit();
        }

        let attributes: FunctionAttributes = contract_scope
            .borrow_mut()
            .add_function(
                name,
                *is_pub,
                params,
                return_type,
                Rc::clone(&function_scope),
            )?
            .to_owned()
            .into();

        context.add_function(def, attributes);

        Ok(())
    } else {
        unreachable!()
    }
}

/// Gather context information for a function body and check for type errors.
pub fn func_body(
    contract_scope: Shared<ContractScope>,
    context: &mut Context,
    def: &Node<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::FuncDef {
        is_pub: _,
        name,
        body,
        return_type,
        args: _,
    } = &def.kind
    {
        let host_func_def = contract_scope
            .borrow()
            .function_def(&name.kind)
            .unwrap_or_else(|| panic!("Failed to lookup function definition for {}", &name.kind));

        // If the return type is unit we do not have to validate any further at this point because
        // both returning (explicit) or not returning (implicit return) are valid syntax.
        // If the return type is anything else, we do need to ensure that all code paths
        // return or revert.
        if !host_func_def.return_type.is_unit() && !all_paths_return_or_revert(&body) {
            context.fancy_error(
                "function body is missing a return or revert statement",
                vec![
                    Label::primary(
                        name.span,
                        "all paths of this function must `return` or `revert`",
                    ),
                    Label::secondary(
                        return_type.as_ref().unwrap().span,
                        format!(
                            "expected function to return `{}`",
                            host_func_def.return_type
                        ),
                    ),
                ],
                vec![],
            );
        }

        traverse_statements(Rc::clone(&host_func_def.scope), context, body)?;

        Ok(())
    } else {
        unreachable!()
    }
}

fn traverse_statements(
    scope: Shared<BlockScope>,
    context: &mut Context,
    body: &[Node<fe::FuncStmt>],
) -> Result<(), SemanticError> {
    for stmt in body.iter() {
        func_stmt(Rc::clone(&scope), context, stmt)?
    }
    Ok(())
}

fn all_paths_return_or_revert(block: &[Node<fe::FuncStmt>]) -> bool {
    for statement in block.iter().rev() {
        match &statement.kind {
            fe::FuncStmt::Return { .. } => return true,
            fe::FuncStmt::Revert { .. } => return true,
            fe::FuncStmt::If {
                test: _,
                body,
                or_else,
            } => {
                let body_returns = all_paths_return_or_revert(&body);
                let or_else_returns = or_else.is_empty() || all_paths_return_or_revert(&or_else);
                if body_returns && or_else_returns {
                    return true;
                }
            }
            _ => {}
        }
    }

    false
}

fn func_def_arg(
    scope: Shared<BlockScope>,
    context: &mut Context,
    arg: &Node<fe::FuncDefArg>,
) -> Result<(String, FixedSize), SemanticError> {
    let fe::FuncDefArg { name, typ } = &arg.kind;
    let typ = types::type_desc_fixed_size(&Scope::Block(Rc::clone(&scope)), context, &typ)?;
    scope.borrow_mut().add_var(&name.kind, typ.clone())?;
    Ok((name.kind.to_string(), typ))
}

fn func_stmt(
    scope: Shared<BlockScope>,
    context: &mut Context,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    use fe::FuncStmt::*;
    match &stmt.kind {
        Return { .. } => func_return(scope, context, stmt),
        VarDecl { .. } => declarations::var_decl(scope, context, stmt),
        Assign { .. } => assignments::assign(scope, context, stmt),
        Emit { .. } => emit(scope, context, stmt),
        AugAssign { .. } => assignments::aug_assign(scope, context, stmt),
        For { .. } => for_loop(scope, context, stmt),
        While { .. } => while_loop(scope, context, stmt),
        If { .. } => if_statement(scope, context, stmt),
        Assert { .. } => assert(scope, context, stmt),
        Expr { .. } => expr(scope, context, stmt),
        Pass => Ok(()),
        Revert => Ok(()),
        Break | Continue => {
            loop_flow_statement(scope, context, stmt);
            Ok(())
        }
    }
    .map_err(|error| error.with_context(stmt.span))
}

fn for_loop(
    scope: Shared<BlockScope>,
    context: &mut Context,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    match &stmt.kind {
        fe::FuncStmt::For { target, iter, body } => {
            // Create the for loop body scope.
            let body_scope = BlockScope::from_block_scope(BlockScopeType::Loop, Rc::clone(&scope));
            // Make sure iter is in the function scope & it should be an array.

            let iter_type = expressions::expr(Rc::clone(&scope), context, &iter)?.typ;
            let target_type = if let Type::Array(array) = iter_type {
                FixedSize::Base(array.inner)
            } else {
                context.type_error(
                    "invalid `for` loop iterator type",
                    iter.span,
                    "array",
                    iter_type,
                );
                return Err(SemanticError::fatal());
            };
            body_scope.borrow_mut().add_var(&target.kind, target_type)?;
            // Traverse the statements within the `for loop` body scope.
            traverse_statements(body_scope, context, body)
        }
        _ => unreachable!(),
    }
}

fn loop_flow_statement(
    scope: Shared<BlockScope>,
    context: &mut Context,
    stmt: &Node<fe::FuncStmt>,
) {
    if !scope.borrow().inherits_type(BlockScopeType::Loop) {
        let stmt_name = match stmt.kind {
            fe::FuncStmt::Continue => "continue",
            fe::FuncStmt::Break => "break",
            _ => panic!(),
        };
        context.error(
            format!("`{}` outside of a loop", stmt_name),
            stmt.span,
            format!("cannot `{}` outside of a `for` or `while` loop", stmt_name),
        );
    }
}

fn if_statement(
    scope: Shared<BlockScope>,
    context: &mut Context,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    match &stmt.kind {
        fe::FuncStmt::If {
            test,
            body,
            or_else,
        } => {
            let test_type = expressions::expr(Rc::clone(&scope), context, test)?.typ;
            if test_type != Type::Base(Base::Bool) {
                context.type_error(
                    "`if` statement condition is not bool",
                    test.span,
                    Base::Bool,
                    test_type,
                );
            }

            let body_scope =
                BlockScope::from_block_scope(BlockScopeType::IfElse, Rc::clone(&scope));
            traverse_statements(body_scope, context, body)?;
            let or_else_scope =
                BlockScope::from_block_scope(BlockScopeType::IfElse, Rc::clone(&scope));
            traverse_statements(or_else_scope, context, or_else)?;
            Ok(())
        }
        _ => unreachable!(),
    }
}

fn while_loop(
    scope: Shared<BlockScope>,
    context: &mut Context,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    match &stmt.kind {
        fe::FuncStmt::While { test, body } => {
            let test_type = expressions::expr(Rc::clone(&scope), context, &test)?.typ;
            if test_type != Type::Base(Base::Bool) {
                context.type_error(
                    "`while` loop condition is not bool",
                    test.span,
                    Base::Bool,
                    test_type,
                );
            }

            let body_scope = BlockScope::from_block_scope(BlockScopeType::Loop, Rc::clone(&scope));
            traverse_statements(body_scope, context, body)?;
            Ok(())
        }
        _ => unreachable!(),
    }
}

fn expr(
    scope: Shared<BlockScope>,
    context: &mut Context,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Expr { value } = &stmt.kind {
        let _attributes = expressions::expr(scope, context, value)?;
    }

    Ok(())
}

fn emit(
    scope: Shared<BlockScope>,
    context: &mut Context,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Emit { name, args } = &stmt.kind {
        if let Some(event) = scope.borrow().contract_event_def(&name.kind) {
            context.add_emit(stmt, event.clone());
            expressions::validate_named_args(
                Rc::clone(&scope),
                context,
                &name.kind,
                name.span,
                args,
                &event.fields,
            )?;
        } else {
            context.error(
                format!("undefined event: `{}`", name.kind),
                name.span,
                "undefined event",
            );
        }

        return Ok(());
    }

    unreachable!()
}

fn assert(
    scope: Shared<BlockScope>,
    context: &mut Context,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Assert { test, msg } = &stmt.kind {
        let test_type = expressions::expr(Rc::clone(&scope), context, &test)?.typ;
        if test_type != Type::Base(Base::Bool) {
            context.type_error(
                "`assert` condition is not bool",
                test.span,
                Base::Bool,
                test_type,
            );
        }

        if let Some(msg) = msg {
            let msg_attributes = expressions::expr(scope, context, msg)?;
            if !matches!(msg_attributes.typ, Type::String(_)) {
                context.error(
                    "`assert` reason must be a string",
                    msg.span,
                    format!("this has type `{}`; expected a string", msg_attributes.typ),
                );
            }
        }

        return Ok(());
    }

    unreachable!()
}

fn func_return(
    scope: Shared<BlockScope>,
    context: &mut Context,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Return { value } = &stmt.kind {
        let attributes = match value {
            Some(val) => expressions::assignable_expr(Rc::clone(&scope), context, val)?,
            None => ExpressionAttributes::new(Type::unit(), Location::Value),
        };

        let host_func_def = scope
            .borrow()
            .current_function_def()
            .expect("Failed to get function definition");
        if attributes.typ != host_func_def.return_type.into() {
            return Err(SemanticError::type_error());
        }

        return Ok(());
    }

    unreachable!()
}
