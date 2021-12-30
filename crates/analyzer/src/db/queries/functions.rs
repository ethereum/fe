use crate::context::{AnalyzerContext, CallType, FunctionBody};
use crate::db::{Analysis, AnalyzerDb};
use crate::errors::TypeError;
use crate::namespace::items::{
    Class, DepGraph, DepGraphWrapper, DepLocality, FunctionId, Item, TypeDef,
};
use crate::namespace::scopes::{BlockScope, BlockScopeType, FunctionScope, ItemScope};
use crate::namespace::types::{self, Contract, FixedSize, SelfDecl, Struct, Type};
use crate::traversal::functions::traverse_statements;
use crate::traversal::types::type_desc;
use fe_common::diagnostics::Label;
use fe_parser::ast;
use fe_parser::node::Node;
use if_chain::if_chain;
use std::collections::HashMap;
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
    let fn_parent = function.class(db);

    if_chain! {
        if let Some(Class::Contract(_)) = fn_parent;
        if let Some(pub_span) = function.pub_span(db);
        if let Some(unsafe_span) = function.unsafe_span(db);
        then {
            scope.error("public contract functions can't be unsafe",
                        pub_span + unsafe_span,
                        "a contract function can be either `pub` or `unsafe`, but not both");
        }
    }

    let mut self_decl = None;
    let mut names = HashMap::new();
    let params = def
        .args
        .iter()
        .enumerate()
        .filter_map(|(index, arg)| match &arg.kind {
            ast::FunctionArg::Zelf => {
                if fn_parent.is_none() {
                    scope.error(
                        "`self` can only be used in contract or struct functions",
                        arg.span,
                        "not allowed in functions defined outside of a contract or struct",
                    );
                } else {
                    self_decl = Some(SelfDecl::Mutable);
                    if index != 0 {
                        scope.error(
                            "`self` is not the first parameter",
                            arg.span,
                            "`self` may only be used as the first parameter",
                        );
                    }
                }
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

                if let Some(named_item) = scope.resolve_name(&name.kind) {
                    scope.name_conflict_error(
                        "function parameter",
                        &name.kind,
                        &named_item,
                        named_item.name_span(db),
                        name.span,
                    );
                    None
                } else if let Some(dup_idx) = names.get(&name.kind) {
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

    let mut block_scope = BlockScope::new(
        &scope,
        if function.is_unsafe(db) {
            BlockScopeType::Unsafe
        } else {
            BlockScopeType::Function
        },
    );

    // If `traverse_statements` fails, we can be confident that a diagnostic
    // has been emitted, either while analyzing this fn body or while analyzing
    // a type or fn used in this fn body, because of the `DiagnosticVoucher`
    // system. (See the definition of `FatalError`)
    let _ = traverse_statements(&mut block_scope, &def.body);
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
            ast::FuncStmt::Unsafe(body) => {
                if all_paths_return_or_revert(body) {
                    return true;
                }
            }
            _ => {}
        }
    }

    false
}

pub fn function_dependency_graph(db: &dyn AnalyzerDb, function: FunctionId) -> DepGraphWrapper {
    let root = Item::Function(function);

    // Edges to direct dependencies.
    let mut directs = vec![];

    let sig = function.signature(db);
    directs.extend(
        sig.return_type
            .clone()
            .into_iter()
            .chain(sig.params.iter().filter_map(|param| param.typ.clone().ok()))
            .filter_map(|typ| match typ {
                FixedSize::Contract(Contract { id, .. }) => {
                    // Contract types that are taken as (non-self) args or returned are "external",
                    // meaning that they're addresses of other contracts, so we don't have direct
                    // access to their fields, etc.
                    Some((
                        root,
                        Item::Type(TypeDef::Contract(id)),
                        DepLocality::External,
                    ))
                }
                FixedSize::Struct(Struct { id, .. }) => {
                    Some((root, Item::Type(TypeDef::Struct(id)), DepLocality::Local))
                }
                _ => None,
            }),
    );
    // A function that takes `self` depends on the type of `self`, so that any
    // relevant struct getters/setters are included when compiling.
    if let Some(class) = function.class(db) {
        directs.push((root, class.as_item(), DepLocality::Local));
    }

    let body = function.body(db);
    for calltype in body.calls.values() {
        match calltype {
            CallType::Pure(function) | CallType::AssociatedFunction { function, .. } => {
                directs.push((root, Item::Function(*function), DepLocality::Local));
            }
            CallType::ValueMethod { class, method, .. } => {
                // Including the "class" type here is probably redundant; the type will
                // also be part of the fn sig, or some type decl, or some create/create2 call, or...
                directs.push((root, class.as_item(), DepLocality::Local));
                directs.push((root, Item::Function(*method), DepLocality::Local));
            }
            CallType::External { contract, function } => {
                directs.push((root, Item::Function(*function), DepLocality::External));
                // Probably redundant:
                directs.push((
                    root,
                    Item::Type(TypeDef::Contract(*contract)),
                    DepLocality::External,
                ));
            }
            CallType::TypeConstructor(Type::Struct(Struct { id, .. })) => {
                directs.push((root, Item::Type(TypeDef::Struct(*id)), DepLocality::Local));
            }
            CallType::TypeConstructor(Type::Contract(Contract { id, .. })) => {
                directs.push((
                    root,
                    Item::Type(TypeDef::Contract(*id)),
                    DepLocality::External,
                ));
            }
            CallType::TypeConstructor(_) => {}
            CallType::BuiltinAssociatedFunction { contract, .. } => {
                // create/create2 call. The contract type is "external" for dependency graph purposes.
                directs.push((
                    root,
                    Item::Type(TypeDef::Contract(*contract)),
                    DepLocality::External,
                ));
            }
            // Builtin functions aren't part of the dependency graph yet.
            CallType::BuiltinFunction(_)
            | CallType::Intrinsic(_)
            | CallType::BuiltinValueMethod { .. } => {}
        }
    }

    directs.extend(
        body.emits
            .values()
            .map(|event| (root, Item::Event(*event), DepLocality::Local)),
    );
    directs.extend(body.var_decl_types.values().filter_map(|typ| match typ {
        FixedSize::Contract(Contract { id, .. }) => Some((
            root,
            Item::Type(TypeDef::Contract(*id)),
            DepLocality::External,
        )),
        FixedSize::Struct(Struct { id, .. }) => {
            Some((root, Item::Type(TypeDef::Struct(*id)), DepLocality::Local))
        }
        _ => None,
    }));

    let mut graph = DepGraph::from_edges(directs.iter());
    for (_, item, _) in directs {
        if let Some(subgraph) = item.dependency_graph(db) {
            graph.extend(subgraph.all_edges())
        }
    }
    DepGraphWrapper(Rc::new(graph))
}

pub fn function_dependency_graph_cycle(
    _db: &dyn AnalyzerDb,
    _cycle: &[String],
    _function: &FunctionId,
) -> DepGraphWrapper {
    DepGraphWrapper(Rc::new(DepGraph::new()))
}
