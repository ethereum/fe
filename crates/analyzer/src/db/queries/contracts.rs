use crate::context::AnalyzerContext;
use crate::db::{Analysis, AnalyzerDb};
use crate::errors;
use crate::namespace::items::{
    self, ContractFieldId, ContractId, DepGraph, DepGraphWrapper, DepLocality, FunctionId, Item,
    TypeDef,
};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types::{self, Type};
use crate::traversal::types::type_desc;
use fe_common::diagnostics::Label;
use fe_parser::ast;
use indexmap::map::{Entry, IndexMap};
use smol_str::SmolStr;
use std::rc::Rc;

/// A `Vec` of every function defined in the contract, including duplicates and
/// the init function.
pub fn contract_all_functions(db: &dyn AnalyzerDb, contract: ContractId) -> Rc<[FunctionId]> {
    let module = contract.module(db);
    let body = &contract.data(db).ast.kind.body;
    body.iter()
        .map(|stmt| match stmt {
            ast::ContractStmt::Function(node) => db.intern_function(Rc::new(items::Function::new(
                db,
                node,
                Some(Item::Type(TypeDef::Contract(contract))),
                module,
            ))),
        })
        .collect()
}

pub fn contract_function_map(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Analysis<Rc<IndexMap<SmolStr, FunctionId>>> {
    let scope = ItemScope::new(db, contract.module(db));
    let mut map = IndexMap::<SmolStr, FunctionId>::new();

    for func in db.contract_all_functions(contract).iter() {
        let def = &func.data(db).ast;
        let def_name = def.name();
        if def_name == "__init__" || def_name == "__call__" {
            continue;
        }

        if let Ok(Some(named_item)) = scope.resolve_name(def_name, func.name_span(db)) {
            scope.name_conflict_error(
                "function",
                def_name,
                &named_item,
                named_item.name_span(db),
                def.kind.sig.kind.name.span,
            );
            continue;
        }

        let func_sig = func.sig(db);
        if let Ok(ret_ty) = func_sig.signature(db).return_type {
            if func.is_public(db) && !ret_ty.is_encodable(db).unwrap_or(false) {
                scope.fancy_error(
                    "can't return unencodable type from public contract function",
                    vec![Label::primary(
                        func_sig
                            .data(db)
                            .ast
                            .kind
                            .return_type
                            .as_ref()
                            .unwrap()
                            .span,
                        format! {"can't return `{}` here", ret_ty.name(db)},
                    )],
                    vec![],
                );
            }
        }
        for (i, param) in func_sig.signature(db).params.iter().enumerate() {
            if let Ok(param_ty) = param.typ {
                if func.is_public(db) && !param_ty.is_encodable(db).unwrap_or(false) {
                    scope.fancy_error(
                        "can't use unencodable type as a public contract function argument",
                        vec![Label::primary(
                            func_sig.data(db).ast.kind.args[i].kind.typ_span().unwrap(),
                            format! {"can't use `{}` here", param_ty.name(db)},
                        )],
                        vec![],
                    );
                }
            }
        }

        match map.entry(def.name().into()) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    &format!(
                        "duplicate function names in `contract {}`",
                        contract.name(db),
                    ),
                    entry.key(),
                    entry.get().data(db).ast.span,
                    def.span,
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(*func);
            }
        }
    }
    Analysis {
        value: Rc::new(map),
        diagnostics: scope.diagnostics.take().into(),
    }
}

pub fn contract_public_function_map(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Rc<IndexMap<SmolStr, FunctionId>> {
    Rc::new(
        contract
            .functions(db)
            .iter()
            .filter_map(|(name, func)| func.is_public(db).then(|| (name.clone(), *func)))
            .collect(),
    )
}

pub fn contract_init_function(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Analysis<Option<FunctionId>> {
    let all_fns = db.contract_all_functions(contract);
    let mut init_fns = all_fns.iter().filter_map(|func| {
        let def = &func.data(db).ast;
        (def.name() == "__init__").then_some((func, def.span))
    });

    let mut diagnostics = vec![];

    let first_def = init_fns.next();
    if let Some((_, dupe_span)) = init_fns.next() {
        let mut labels = vec![
            Label::primary(first_def.unwrap().1, "`__init__` first defined here"),
            Label::secondary(dupe_span, "`init` redefined here"),
        ];
        for (_, dupe_span) in init_fns {
            labels.push(Label::secondary(dupe_span, "`__init__` redefined here"));
        }
        diagnostics.push(errors::fancy_error(
            format!(
                "`fn __init__()` is defined multiple times in `contract {}`",
                contract.name(db),
            ),
            labels,
            vec![],
        ));
    }

    if let Some((id, span)) = first_def {
        // `__init__` must be `pub`.
        // Return type is checked in `queries::functions::function_signature`.
        if !id.is_public(db) {
            diagnostics.push(errors::fancy_error(
                "`__init__` function is not public",
                vec![Label::primary(span, "`__init__` function must be public")],
                vec![
                    "Hint: Add the `pub` modifier.".to_string(),
                    "Example: `pub fn __init__():`".to_string(),
                ],
            ));
        }
    }

    Analysis {
        value: first_def.map(|(id, _span)| *id),
        diagnostics: diagnostics.into(),
    }
}

pub fn contract_call_function(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Analysis<Option<FunctionId>> {
    let all_fns = db.contract_all_functions(contract);
    let mut call_fns = all_fns.iter().filter_map(|func| {
        let def = &func.data(db).ast;
        (def.name() == "__call__").then_some((func, def.span))
    });

    let mut diagnostics = vec![];

    let first_def = call_fns.next();
    if let Some((_, dupe_span)) = call_fns.next() {
        let mut labels = vec![
            Label::primary(first_def.unwrap().1, "`__call__` first defined here"),
            Label::secondary(dupe_span, "`__call__` redefined here"),
        ];
        for (_, dupe_span) in call_fns {
            labels.push(Label::secondary(dupe_span, "`__call__` redefined here"));
        }
        diagnostics.push(errors::fancy_error(
            format!(
                "`fn __call__()` is defined multiple times in `contract {}`",
                contract.name(db),
            ),
            labels,
            vec![],
        ));
    }

    if let Some((id, span)) = first_def {
        // `__call__` must be `pub`.
        // Return type is checked in `queries::functions::function_signature`.
        if !id.is_public(db) {
            diagnostics.push(errors::fancy_error(
                "`__call__` function is not public",
                vec![Label::primary(span, "`__call__` function must be public")],
                vec![
                    "Hint: Add the `pub` modifier.".to_string(),
                    "Example: `pub fn __call__():`".to_string(),
                ],
            ));
        }
    }

    if let Some((_id, init_span)) = first_def {
        for func in all_fns.iter() {
            let name = func.name(db);
            if func.is_public(db) && name != "__init__" && name != "__call__" {
                diagnostics.push(errors::fancy_error(
                    "`pub` not allowed if `__call__` is defined",
                    vec![
                        Label::primary(func.name_span(db), format!("`{name}` can't be public")),
                        Label::secondary(init_span, "`__call__` defined here"),
                    ],
                    vec![
                        "The `__call__` function replaces the default function dispatcher, which makes `pub` modifiers obsolete.".to_string(),
                        "Hint: Remove the `pub` modifier or `__call__` function.".to_string(),
                    ],
                ));
            }
        }
    }

    Analysis {
        value: first_def.map(|(id, _span)| *id),
        diagnostics: diagnostics.into(),
    }
}

/// All field ids, including those with duplicate names
pub fn contract_all_fields(db: &dyn AnalyzerDb, contract: ContractId) -> Rc<[ContractFieldId]> {
    contract
        .data(db)
        .ast
        .kind
        .fields
        .iter()
        .map(|node| {
            db.intern_contract_field(Rc::new(items::ContractField {
                ast: node.clone(),
                parent: contract,
            }))
        })
        .collect()
}

pub fn contract_field_map(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Analysis<Rc<IndexMap<SmolStr, ContractFieldId>>> {
    let scope = ItemScope::new(db, contract.module(db));
    let mut map = IndexMap::<SmolStr, ContractFieldId>::new();

    let contract_name = contract.name(db);
    for field in db.contract_all_fields(contract).iter() {
        let node = &field.data(db).ast;

        match map.entry(node.name().into()) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    &format!("duplicate field names in `contract {contract_name}`",),
                    entry.key(),
                    entry.get().data(db).ast.span,
                    node.span,
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(*field);
            }
        }
    }

    Analysis {
        value: Rc::new(map),
        diagnostics: scope.diagnostics.take().into(),
    }
}

pub fn contract_field_type(
    db: &dyn AnalyzerDb,
    field: ContractFieldId,
) -> Analysis<Result<types::TypeId, errors::TypeError>> {
    let mut scope = ItemScope::new(db, field.data(db).parent.module(db));
    let self_ty = Some(field.data(db).parent.as_type(db).as_trait_or_type());
    let typ = type_desc(&mut scope, &field.data(db).ast.kind.typ, self_ty);

    let node = &field.data(db).ast;

    if node.kind.is_pub {
        scope.not_yet_implemented("contract `pub` fields", node.span);
    }
    if node.kind.is_const {
        scope.not_yet_implemented("contract `const` fields", node.span);
    }
    if let Some(value_node) = &node.kind.value {
        scope.not_yet_implemented("contract field initial value assignment", value_node.span);
    }

    Analysis {
        value: typ,
        diagnostics: scope.diagnostics.take().into(),
    }
}

pub fn contract_dependency_graph(db: &dyn AnalyzerDb, contract: ContractId) -> DepGraphWrapper {
    // A contract depends on the types of its fields, and the things those types
    // depend on. Note that this *does not* include the contract's public
    // function graph. (See `contract_runtime_dependency_graph` below)

    let fields = contract.fields(db);
    let field_types = fields
        .values()
        .filter_map(|field| match field.typ(db).ok()?.typ(db) {
            Type::Contract(id) => Some(Item::Type(TypeDef::Contract(id))),
            Type::Struct(id) => Some(Item::Type(TypeDef::Struct(id))),
            // TODO: when tuples can contain non-primitive items,
            // we'll have to depend on tuple element types
            _ => None,
        })
        .collect::<Vec<_>>();

    let root = Item::Type(TypeDef::Contract(contract));
    let mut graph = DepGraph::from_edges(
        field_types
            .iter()
            .map(|item| (root, *item, DepLocality::Local)),
    );

    for item in field_types {
        if let Some(subgraph) = item.dependency_graph(db) {
            graph.extend(subgraph.all_edges())
        }
    }
    DepGraphWrapper(Rc::new(graph))
}

pub fn contract_dependency_graph_cycle(
    _db: &dyn AnalyzerDb,
    _cycle: &[String],
    _contract: &ContractId,
) -> DepGraphWrapper {
    DepGraphWrapper(Rc::new(DepGraph::new()))
}

pub fn contract_runtime_dependency_graph(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> DepGraphWrapper {
    // This is the dependency graph of the (as yet imaginary) `__call__` function,
    // which dispatches to the contract's public functions. This should be used
    // when compiling the runtime object for a contract.

    let root = Item::Type(TypeDef::Contract(contract));
    let root_fns = if let Some(call_id) = contract.call_function(db) {
        vec![call_id]
    } else {
        contract.public_functions(db).values().copied().collect()
    }
    .into_iter()
    .map(|fun| (root, Item::Function(fun), DepLocality::Local))
    .collect::<Vec<_>>();

    let mut graph = DepGraph::from_edges(root_fns.iter());

    for (_, item, _) in root_fns {
        if let Some(subgraph) = item.dependency_graph(db) {
            graph.extend(subgraph.all_edges())
        }
    }
    DepGraphWrapper(Rc::new(graph))
}

pub fn contract_runtime_dependency_graph_cycle(
    _db: &dyn AnalyzerDb,
    _cycle: &[String],
    _contract: &ContractId,
) -> DepGraphWrapper {
    DepGraphWrapper(Rc::new(DepGraph::new()))
}
