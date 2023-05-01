use crate::context::{AnalyzerContext, CallType, FunctionBody};
use crate::db::{Analysis, AnalyzerDb};
use crate::display::Displayable;
use crate::errors::TypeError;
use crate::namespace::items::{
    DepGraph, DepGraphWrapper, DepLocality, FunctionId, FunctionSigId, Item, TypeDef,
};
use crate::namespace::scopes::{BlockScope, BlockScopeType, FunctionScope, ItemScope};
use crate::namespace::types::{self, CtxDecl, Generic, SelfDecl, Type, TypeId};
use crate::traversal::functions::traverse_statements;
use crate::traversal::types::{type_desc, type_desc_to_trait};
use fe_common::diagnostics::Label;
use fe_parser::ast::{self, GenericParameter};
use fe_parser::node::Node;
use if_chain::if_chain;
use smol_str::SmolStr;
use std::collections::HashMap;
use std::rc::Rc;

/// Gather context information for a function definition and check for type
/// errors. Does not inspect the function body.
pub fn function_signature(
    db: &dyn AnalyzerDb,
    function: FunctionSigId,
) -> Analysis<Rc<types::FunctionSignature>> {
    let def = &function.data(db).ast;

    let mut scope = ItemScope::new(db, function.module(db));
    let fn_parent = function.parent(db);

    let mut self_decl = None;
    let mut ctx_decl = None;
    let mut names = HashMap::new();
    let mut labels = HashMap::new();

    let sig_ast = &function.data(db).ast.kind;
    sig_ast.generic_params.kind.iter().fold(
        HashMap::<SmolStr, Node<_>>::new(),
        |mut accum, param| {
            if let Some(previous) = accum.get(&param.name()) {
                scope.duplicate_name_error(
                    "duplicate generic parameter",
                    &param.name(),
                    previous.span,
                    param.name_node().span,
                );
            } else {
                accum.insert(param.name(), param.name_node());
            };

            accum
        },
    );

    if !matches!(fn_parent, Item::Type(TypeDef::Struct(_))) && function.is_generic(db) {
        scope.fancy_error(
            "generic function parameters aren't yet supported outside of struct functions",
            vec![Label::primary(
                function.data(db).ast.kind.generic_params.span,
                "this cannot appear here",
            )],
            vec!["Hint: Struct functions can have generic parameters".into()],
        );
    }

    if function.is_generic(db) {
        for param in function.data(db).ast.kind.generic_params.kind.iter() {
            if let GenericParameter::Unbounded(val) = param {
                scope.fancy_error(
                    "unbounded generic parameters aren't yet supported",
                    vec![Label::primary(
                        val.span,
                        format!("`{}` needs to be bound by some trait", val.kind),
                    )],
                    vec![format!(
                        "Hint: Change `{}` to `{}: SomeTrait`",
                        val.kind, val.kind
                    )],
                );
            }
        }
    }

    let params = def
        .kind
        .args
        .iter()
        .enumerate()
        .filter_map(|(index, arg)| match &arg.kind {
            ast::FunctionArg::Self_ { mut_ }=> {
                if matches!(fn_parent, Item::Module(_)) {
                    scope.error(
                        "`self` can only be used in contract, struct, trait or impl functions",
                        arg.span,
                        "not allowed in functions defined directly in a module",
                    );
                } else {
                    self_decl = Some(SelfDecl { span: arg.span, mut_: *mut_ });
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
            ast::FunctionArg::Regular { mut_, label, name, typ: typedesc } => {
                let typ = resolve_function_param_type(db, function, &mut scope, typedesc).and_then(|typ| match typ {
                    typ if typ.has_fixed_size(db) => {
                        if let Some(mut_span) = mut_ {
                            if typ.is_primitive(db) {
                                Err(TypeError::new(scope.error(
                                    "primitive type function parameters cannot be `mut`",
                                    *mut_span + typedesc.span,
                                    &format!("`{}` type can't be used as a `mut` function parameter",
                                             typ.display(db)))))
                            } else {
                                Ok(Type::Mut(typ).id(db))
                            }
                        } else {
                            Ok(typ)
                        }
                    }
                    _ => Err(TypeError::new(scope.error(
                        "function parameter types must have fixed size",
                        typedesc.span,
                        &format!("`{}` type can't be used as a function parameter", typ.display(db)),
                    ))),
                });

                if let Some(context_type) = scope.get_context_type() {
                    if arg.name() == "ctx" &&  typ.as_ref().map(|val| val.deref(db)) != Ok(context_type) {
                        scope.error(
                            "`ctx` is reserved for instances of `Context`",
                            arg.span,
                            "`ctx` must be an instance of `Context`",
                        );
                    };

                    if typ.as_ref().map(|val| val.deref(db)) == Ok(context_type) {
                        if arg.name() != "ctx" {
                            scope.error(
                                "invalid `Context` instance name",
                                arg.span,
                                "instances of `Context` must be named `ctx`",
                            );
                        } else if self_decl.is_some() && index != 1 {
                            scope.error(
                                "invalid parameter order",
                                arg.span,
                                "`ctx: Context` must be placed after the `self` parameter",
                            );
                        } else if self_decl.is_none() && index != 0 {
                            scope.error(
                                "invalid parameter order",
                                arg.span,
                                "`ctx: Context` must be the first parameter",
                            );
                        }
                        else {
                            ctx_decl = Some(CtxDecl {span: arg.span,  mut_: *mut_})
                        }
                    }
                }

                if let Some(label) = &label {
                    if_chain! {
                        if label.kind != "_";
                        if let Some(dup_idx) = labels.get(&label.kind);
                        then {
                            let dup_arg: &Node<ast::FunctionArg> = &def.kind.args[*dup_idx];
                            scope.fancy_error(
                                &format!("duplicate parameter labels in function `{}`", def.kind.name.kind),
                                vec![
                                    Label::primary(dup_arg.span, "the label `{}` was first used here"),
                                    Label::primary(label.span, "label `{}` used again here"),
                                ], vec![]);
                            return None;
                        } else {
                            labels.insert(&label.kind, index);
                        }
                    }
                }

                if let Ok(Some(named_item)) = scope.resolve_name(&name.kind, name.span) {
                    scope.name_conflict_error(
                        "function parameter",
                        &name.kind,
                        &named_item,
                        named_item.name_span(db),
                        name.span,
                    );
                    None
                } else if let Some(dup_idx) = names.get(&name.kind) {
                    let dup_arg: &Node<ast::FunctionArg> = &def.kind.args[*dup_idx];
                    scope.duplicate_name_error(
                        &format!("duplicate parameter names in function `{}`", function.name(db)),
                        &name.kind,
                        dup_arg.span,
                        arg.span,
                    );
                    None
                } else {
                    names.insert(&name.kind, index);

                    Some(types::FunctionParam::new(
                        label.as_ref().map(|s| s.kind.as_str()),
                        &name.kind,
                        typ,
                    ))
                }
            }
        })
        .collect();

    let return_type = def
        .kind
        .return_type
        .as_ref()
        .map(|type_node| {
            let fn_name = &function.name(db);
            if fn_name == "__init__" || fn_name == "__call__" {
                // `__init__` and `__call__` must not return any type other than `()`.
                if type_node.kind != ast::TypeDesc::Unit {
                    scope.fancy_error(
                        &format!("`{fn_name}` function has incorrect return type"),
                        vec![Label::primary(type_node.span, "return type should be `()`")],
                        vec![
                            "Hint: Remove the return type specification.".to_string(),
                            format!("Example: `pub fn {fn_name}():`"),
                        ],
                    );
                }
                Ok(TypeId::unit(scope.db()))
            } else {
                let self_ty = match function.parent(db) {
                    Item::Trait(id) => Some(id.as_trait_or_type()),
                    _ => function.self_type(db).map(|ty| ty.as_trait_or_type()),
                };

                match type_desc(&mut scope, type_node, self_ty)? {
                    typ if typ.has_fixed_size(scope.db()) => Ok(typ),
                    _ => Err(TypeError::new(scope.error(
                        "function return type must have a fixed size",
                        type_node.span,
                        "this can't be returned from a function",
                    ))),
                }
            }
        })
        .unwrap_or_else(|| Ok(TypeId::unit(db)));

    Analysis {
        value: Rc::new(types::FunctionSignature {
            self_decl,
            ctx_decl,
            params,
            return_type,
        }),
        diagnostics: scope.diagnostics.take().into(),
    }
}

fn resolve_function_param_type(
    db: &dyn AnalyzerDb,
    function: FunctionSigId,
    context: &mut dyn AnalyzerContext,
    desc: &Node<ast::TypeDesc>,
) -> Result<TypeId, TypeError> {
    // First check if the param type is a local generic of the function. This won't
    // hold when in the future generics can appear on the contract, struct or
    // module level but it could be good enough for now.
    if let ast::TypeDesc::Base { base } = &desc.kind {
        if let Some(val) = function.generic_param(db, base) {
            let bounds = match val {
                ast::GenericParameter::Unbounded(_) => vec![].into(),
                ast::GenericParameter::Bounded { bound, .. } => {
                    vec![type_desc_to_trait(context, &bound)?].into()
                }
            };

            return Ok(db.intern_type(Type::Generic(Generic {
                name: base.clone(),
                bounds,
            })));
        }
    }

    let self_ty = if let Item::Trait(id) = function.parent(db) {
        Some(id.as_trait_or_type())
    } else {
        function.self_type(db).map(|ty| ty.as_trait_or_type())
    };

    type_desc(context, desc, self_ty)
}

/// Gather context information for a function body and check for type errors.
pub fn function_body(db: &dyn AnalyzerDb, function: FunctionId) -> Analysis<Rc<FunctionBody>> {
    let def = &function.data(db).ast.kind;
    let scope = FunctionScope::new(db, function);

    // If the return type is unit, explicit return or no return (implicit) is valid,
    // so no scanning is necessary.
    // If the return type is anything else, we need to ensure that all code paths
    // return or revert.
    if let Ok(return_type) = &function.signature(db).return_type {
        if !return_type.typ(db).is_unit() && !all_paths_return_or_revert(&def.body) {
            scope.fancy_error(
                "function body is missing a return or revert statement",
                vec![
                    Label::primary(
                        function.name_span(db),
                        "all paths of this function must `return` or `revert`",
                    ),
                    Label::secondary(
                        def.sig.kind.return_type.as_ref().unwrap().span,
                        format!("expected function to return `{}`", return_type.display(db)),
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
        diagnostics: scope.diagnostics.into_inner().into(),
    }
}

fn all_paths_return_or_revert(block: &[Node<ast::FuncStmt>]) -> bool {
    for statement in block.iter().rev() {
        match &statement.kind {
            ast::FuncStmt::Return { .. } | ast::FuncStmt::Revert { .. } => return true,
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

            ast::FuncStmt::Match { arms, .. } => {
                return arms
                    .iter()
                    .all(|arm| all_paths_return_or_revert(&arm.kind.body));
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
            .filter_map(|id| match id.typ(db) {
                Type::Contract(id) => {
                    // Contract types that are taken as (non-self) args or returned are "external",
                    // meaning that they're addresses of other contracts, so we don't have direct
                    // access to their fields, etc.
                    Some((
                        root,
                        Item::Type(TypeDef::Contract(id)),
                        DepLocality::External,
                    ))
                }
                Type::Struct(id) => {
                    Some((root, Item::Type(TypeDef::Struct(id)), DepLocality::Local))
                }
                _ => None,
            }),
    );
    // A function that takes `self` depends on the type of `self`, so that any
    // relevant struct getters/setters are included when compiling.
    if !function.sig(db).is_module_fn(db) {
        directs.push((root, function.parent(db), DepLocality::Local));
    }

    let body = function.body(db);
    for calltype in body.calls.values() {
        match calltype {
            CallType::Pure(function) | CallType::AssociatedFunction { function, .. } => {
                directs.push((root, Item::Function(*function), DepLocality::Local));
            }
            CallType::ValueMethod { method, .. } => {
                directs.push((root, Item::Function(*method), DepLocality::Local));
            }
            CallType::TraitValueMethod { trait_id, .. } => {
                directs.push((root, Item::Trait(*trait_id), DepLocality::Local));
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
            CallType::TypeConstructor(type_id) => match type_id.typ(db) {
                Type::Struct(id) => {
                    directs.push((root, Item::Type(TypeDef::Struct(id)), DepLocality::Local))
                }
                Type::Contract(id) => directs.push((
                    root,
                    Item::Type(TypeDef::Contract(id)),
                    DepLocality::External,
                )),
                _ => {}
            },
            CallType::EnumConstructor(variant) => directs.push((
                root,
                Item::Type(TypeDef::Enum(variant.parent(db))),
                DepLocality::Local,
            )),
            CallType::BuiltinAssociatedFunction { contract, .. } => {
                // create/create2 call. The contract type is "external" for dependency graph
                // purposes.
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
        body.var_types
            .values()
            .filter_map(|typid| match typid.typ(db) {
                Type::Contract(id) => Some((
                    root,
                    Item::Type(TypeDef::Contract(id)),
                    DepLocality::External,
                )),
                Type::Struct(id) => {
                    Some((root, Item::Type(TypeDef::Struct(id)), DepLocality::Local))
                }
                _ => None,
            }),
    );

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
