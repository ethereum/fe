use crate::builtins;
use crate::context::AnalyzerContext;
use crate::db::{Analysis, AnalyzerDb};
use crate::errors;
use crate::namespace::items::{self, ContractFieldId, ContractId, EventId, FunctionId};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types;
use crate::traversal::types::type_desc;
use fe_common::diagnostics::Label;
use fe_parser::ast;
use indexmap::map::{Entry, IndexMap};

use std::rc::Rc;

pub fn contract_all_functions(db: &dyn AnalyzerDb, contract: ContractId) -> Rc<Vec<FunctionId>> {
    let body = &contract.data(db).ast.kind.body;
    Rc::new(
        body.iter()
            .filter_map(|stmt| match stmt {
                ast::ContractStmt::Event(_) => None,
                ast::ContractStmt::Function(node) => {
                    Some(db.intern_function(Rc::new(items::Function {
                        ast: node.clone(),
                        parent: contract,
                    })))
                }
            })
            .collect(),
    )
}

pub fn contract_function_map(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Analysis<Rc<IndexMap<String, FunctionId>>> {
    let mut scope = ItemScope::new(db, contract.module(db));
    let mut map = IndexMap::<String, FunctionId>::new();

    for func in contract.all_functions(db).iter() {
        let def = &func.data(db).ast;
        let def_name = def.name();
        if def_name == "__init__" {
            continue;
        }
        if let Some(reserved) = builtins::reserved_name(def_name) {
            scope.error(
                &format!(
                    "function name conflicts with built-in {}",
                    reserved.as_ref()
                ),
                def.kind.name.span,
                &format!("`{}` is a built-in {}", def_name, reserved.as_ref()),
            );
            continue;
        }

        match map.entry(def.name().to_string()) {
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
        diagnostics: Rc::new(scope.diagnostics),
    }
}

pub fn contract_public_function_map(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Rc<IndexMap<String, FunctionId>> {
    Rc::new(
        contract
            .functions(db)
            .iter()
            .filter_map(|(name, func)| func.is_public(db).then(|| (name.clone(), *func)))
            .collect(),
    )
}

pub fn contract_pure_function_map(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Rc<IndexMap<String, FunctionId>> {
    Rc::new(
        contract
            .functions(db)
            .iter()
            .filter_map(|(name, func)| func.is_pure(db).then(|| (name.clone(), *func)))
            .collect(),
    )
}

pub fn contract_self_function_map(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Rc<IndexMap<String, FunctionId>> {
    Rc::new(
        contract
            .functions(db)
            .iter()
            .filter_map(|(name, func)| (!func.is_pure(db)).then(|| (name.clone(), *func)))
            .collect(),
    )
}

pub fn contract_init_function(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Analysis<Option<FunctionId>> {
    let all_fns = contract.all_functions(db);
    let mut init_fns = all_fns.iter().filter_map(|func| {
        let def = &func.data(db).ast;
        (def.name() == "__init__").then(|| (func, def.span))
    });

    let mut diagnostics = vec![];

    let first_def = init_fns.next();
    if let Some((_, dupe_span)) = init_fns.next() {
        let mut labels = vec![
            Label::primary(first_def.unwrap().1, "`__init__` first defined here"),
            Label::secondary(dupe_span, "`init` redefined here"),
        ];
        for (_, dupe_span) in init_fns {
            labels.push(Label::secondary(dupe_span, "`init` redefined here"));
        }
        diagnostics.push(errors::fancy_error(
            &format!(
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
        if !id.data(db).ast.kind.is_pub {
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
        diagnostics: Rc::new(diagnostics),
    }
}

pub fn contract_all_events(db: &dyn AnalyzerDb, contract: ContractId) -> Rc<Vec<EventId>> {
    let body = &contract.data(db).ast.kind.body;
    Rc::new(
        body.iter()
            .filter_map(|stmt| match stmt {
                ast::ContractStmt::Function(_) => None,
                ast::ContractStmt::Event(node) => Some(db.intern_event(Rc::new(items::Event {
                    ast: node.clone(),
                    contract,
                }))),
            })
            .collect(),
    )
}

pub fn contract_event_map(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Analysis<Rc<IndexMap<String, EventId>>> {
    let mut scope = ItemScope::new(db, contract.module(db));
    let mut map = IndexMap::<String, EventId>::new();

    let contract_name = contract.name(db);
    for event in db.contract_all_events(contract).iter() {
        let node = &event.data(db).ast;

        match map.entry(node.name().to_string()) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    &format!("duplicate event names in `contract {}`", contract_name,),
                    entry.key(),
                    entry.get().data(db).ast.span,
                    node.span,
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(*event);
            }
        }
    }

    Analysis {
        value: Rc::new(map),
        diagnostics: Rc::new(scope.diagnostics),
    }
}

pub fn contract_all_fields(db: &dyn AnalyzerDb, contract: ContractId) -> Rc<Vec<ContractFieldId>> {
    let fields = contract
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
        .collect();
    Rc::new(fields)
}

pub fn contract_field_map(
    db: &dyn AnalyzerDb,
    contract: ContractId,
) -> Analysis<Rc<IndexMap<String, ContractFieldId>>> {
    let mut scope = ItemScope::new(db, contract.module(db));
    let mut map = IndexMap::<String, ContractFieldId>::new();

    let contract_name = contract.name(db);
    for field in db.contract_all_fields(contract).iter() {
        let node = &field.data(db).ast;

        match map.entry(node.name().to_string()) {
            Entry::Occupied(entry) => {
                scope.duplicate_name_error(
                    &format!("duplicate field names in `contract {}`", contract_name,),
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
        diagnostics: Rc::new(scope.diagnostics),
    }
}

pub fn contract_field_type(
    db: &dyn AnalyzerDb,
    field: ContractFieldId,
) -> Analysis<Result<types::Type, errors::TypeError>> {
    let mut scope = ItemScope::new(db, field.data(db).parent.module(db));
    let typ = type_desc(&mut scope, &field.data(db).ast.kind.typ);

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
        diagnostics: Rc::new(scope.diagnostics),
    }
}
