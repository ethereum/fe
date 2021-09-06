use crate::context::{AnalyzerContext, FunctionBody};
use crate::db::{Analysis, AnalyzerDb};
use crate::errors::TypeError;
use crate::namespace::items::FunctionId;
use crate::namespace::scopes::{BlockScope, BlockScopeType, FunctionScope, ItemScope};
use crate::namespace::types::{self, FixedSize, SelfDecl};
use crate::traversal::functions::traverse_statements;
use crate::traversal::types::type_desc;
use fe_common::diagnostics::Label;
use fe_parser::ast;
use fe_parser::node::Node;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

/// Gather context information for a function definition and check for type
/// errors. Does not inspect the function body.
pub fn function_signature(
    db: &dyn AnalyzerDb,
    function: FunctionId,
) -> Analysis<Rc<types::FunctionSignature>> {
    let node = &function.data(db).ast;
    let def = &node.kind;

    let mut scope = ItemScope::new(db, function.module(db));

    let self_decl = if matches!(
        def.args.get(0),
        Some(Node {
            kind: ast::FunctionArg::Zelf,
            ..
        })
    ) {
        SelfDecl::Mutable
    } else {
        SelfDecl::None
    };

    let mut names = HashMap::new();
    let params = def
        .args
        .iter()
        // skip analysis of a `self` param
        .skip(if self_decl == SelfDecl::None { 0 } else { 1 })
        .enumerate()
        .filter_map(|(index, arg)| match &arg.kind {
            ast::FunctionArg::Zelf => {
                scope.error(
                    "self is not the first parameter",
                    arg.span,
                    "self may only be used as the first parameter",
                );

                None
            }
            ast::FunctionArg::Regular(ast::RegularFunctionArg {
                name,
                typ: typ_node,
            }) => {
                let typ = type_desc(&mut scope, typ_node).and_then(|typ| match typ.try_into() {
                    Ok(typ) => Ok(typ),
                    Err(_) => Err(TypeError::new(scope.error(
                        "function parameter types must have fixed size",
                        typ_node.span,
                        "`Map` type can't be used as a function parameter",
                    ))),
                });

                if let Some(dup_idx) = names.get(&name.kind) {
                    let dup_arg: &Node<ast::FunctionArg> = &def.args[*dup_idx];
                    scope.duplicate_name_error(
                        &format!("duplicate parameter names in function `{}`", def.name.kind),
                        &name.kind,
                        dup_arg.span,
                        arg.span,
                    );
                    None
                } else {
                    names.insert(&name.kind, index);
                    Some(types::FunctionParam {
                        name: name.kind.clone(),
                        typ,
                    })
                }
            }
        })
        .collect();

    let return_type = def
        .return_type
        .as_ref()
        .map(|type_node| {
            if def.name.kind == "__init__" {
                // `__init__` must not return any type other than `()`.
                if type_node.kind != ast::TypeDesc::Unit {
                    scope.fancy_error(
                        "`__init__` function has incorrect return type",
                        vec![Label::primary(type_node.span, "return type should be `()`")],
                        vec![
                            "Hint: Remove the return type specification.".to_string(),
                            "Example: `pub fn __init__():`".to_string(),
                        ],
                    );
                }
                Ok(FixedSize::unit())
            } else {
                match type_desc(&mut scope, type_node)?.try_into() {
                    Ok(typ) => Ok(typ),
                    Err(_) => Err(TypeError::new(scope.error(
                        "function return type must have a fixed size",
                        type_node.span,
                        "this can't be returned from a function",
                    ))),
                }
            }
        })
        .unwrap_or_else(|| Ok(FixedSize::unit()));

    Analysis {
        value: Rc::new(types::FunctionSignature {
            self_decl,
            params,
            return_type,
        }),
        diagnostics: Rc::new(scope.diagnostics),
    }
}

/// Gather context information for a function body and check for type errors.
pub fn function_body(db: &dyn AnalyzerDb, function: FunctionId) -> Analysis<Rc<FunctionBody>> {
    let def = &function.data(db).ast.kind;
    let mut scope = FunctionScope::new(db, function);

    // If the return type is unit, explicit return or no return (implicit) is valid,
    // so no scanning is necessary.
    // If the return type is anything else, we need to ensure that all code paths
    // return or revert.
    if let Ok(return_type) = &function.signature(db).return_type {
        if !return_type.is_unit() && !all_paths_return_or_revert(&def.body) {
            scope.fancy_error(
                "function body is missing a return or revert statement",
                vec![
                    Label::primary(
                        def.name.span,
                        "all paths of this function must `return` or `revert`",
                    ),
                    Label::secondary(
                        def.return_type.as_ref().unwrap().span,
                        format!("expected function to return `{}`", return_type),
                    ),
                ],
                vec![],
            );
        }
    }

    // If `traverse_statements` fails, we can be confident that a diagnostic
    // has been emitted, either while analyzing this fn body or while analyzing
    // a type or fn used in this fn body, because of the `DiagnosticVoucher`
    // system. (See the definition of `FatalError`)
    let _ = traverse_statements(
        &mut BlockScope::new(&scope, BlockScopeType::Function),
        &def.body,
    );
    Analysis {
        value: Rc::new(scope.body.into_inner()),
        diagnostics: Rc::new(scope.diagnostics.into_inner()),
    }
}

fn all_paths_return_or_revert(block: &[Node<ast::FuncStmt>]) -> bool {
    for statement in block.iter().rev() {
        match &statement.kind {
            ast::FuncStmt::Return { .. } => return true,
            ast::FuncStmt::Revert { .. } => return true,
            ast::FuncStmt::If {
                test: _,
                body,
                or_else,
            } => {
                let body_returns = all_paths_return_or_revert(body);
                let or_else_returns = all_paths_return_or_revert(or_else);
                if body_returns && or_else_returns {
                    return true;
                }
            }
            _ => {}
        }
    }

    false
}
