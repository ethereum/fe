use crate::builtins;
use crate::context::Analysis;
use crate::db::AnalyzerDb;
use crate::errors;
use crate::namespace::items::{
    Contract, ContractId, ModuleId, Struct, StructId, TypeAlias, TypeDefId,
};
use crate::namespace::types;
use fe_common::diagnostics::Label;
use fe_parser::ast;
use indexmap::map::{Entry, IndexMap};
use std::rc::Rc;

pub fn module_all_type_defs(db: &dyn AnalyzerDb, module: ModuleId) -> Rc<Vec<TypeDefId>> {
    let ast::Module { body } = &module.data(db).ast;
    let ids = body
        .iter()
        .filter_map(|stmt| match stmt {
            ast::ModuleStmt::TypeAlias(node) => {
                Some(TypeDefId::Alias(db.intern_type_alias(Rc::new(TypeAlias {
                    ast: node.clone(),
                    module,
                }))))
            }
            ast::ModuleStmt::Contract(node) => {
                Some(TypeDefId::Contract(db.intern_contract(Rc::new(Contract {
                    name: node.name().to_string(),
                    ast: node.clone(),
                    module,
                }))))
            }
            ast::ModuleStmt::Struct(node) => {
                Some(TypeDefId::Struct(db.intern_struct(Rc::new(Struct {
                    ast: node.clone(),
                    module,
                }))))
            }
            _ => None,
        })
        .collect();
    Rc::new(ids)
}

pub fn module_type_def_map(
    db: &dyn AnalyzerDb,
    module: ModuleId,
) -> Analysis<Rc<IndexMap<String, TypeDefId>>> {
    let mut diagnostics = vec![];

    let mut map = IndexMap::<String, TypeDefId>::new();
    for def in module.all_type_defs(db).iter() {
        let def_name = def.name(db);
        if let Some(reserved) = builtins::reserved_name(&def_name) {
            diagnostics.push(errors::error(
                &format!("type name conflicts with built-in {}", reserved.as_ref()),
                def.name_span(db),
                &format!("`{}` is a built-in {}", def_name, reserved.as_ref()),
            ));
            continue;
        }

        match map.entry(def.name(db)) {
            Entry::Occupied(entry) => {
                diagnostics.push(errors::fancy_error(
                    "duplicate type name",
                    vec![
                        Label::primary(
                            entry.get().span(db),
                            format!("`{}` first defined here", entry.key()),
                        ),
                        Label::secondary(def.span(db), format!("`{}` redefined here", entry.key())),
                    ],
                    vec![],
                ));
            }
            Entry::Vacant(entry) => {
                entry.insert(*def);
            }
        }
    }
    Analysis {
        value: Rc::new(map),
        diagnostics: Rc::new(diagnostics),
    }
}

pub fn module_resolve_type(
    db: &dyn AnalyzerDb,
    module: ModuleId,
    name: String,
) -> Option<Result<types::Type, errors::TypeError>> {
    Some(module.type_defs(db).get(&name)?.typ(db))
}

#[allow(clippy::ptr_arg)]
pub fn module_resolve_type_cycle(
    _db: &dyn AnalyzerDb,
    _cycle: &[String],
    _module: &ModuleId,
    _name: &String,
) -> Option<Result<types::Type, errors::TypeError>> {
    // The only possible type cycle currently is a recursive type alias,
    // which is handled in queries/types.rs
    // However, salsa will also call this function if there's such a cycle,
    // so we can't panic here, and we can't return a TypeError because
    // there's no way to emit a diagnostic! The only option is to return
    // None, and handle type cycles in more specifc cycle handlers.
    None
}

pub fn module_contracts(db: &dyn AnalyzerDb, module: ModuleId) -> Rc<Vec<ContractId>> {
    let defs = module.all_type_defs(db);
    Rc::new(
        defs.iter()
            .filter_map(|def| match def {
                TypeDefId::Contract(id) => Some(*id),
                _ => None,
            })
            .collect(),
    )
}

pub fn module_structs(db: &dyn AnalyzerDb, module: ModuleId) -> Rc<Vec<StructId>> {
    let defs = module.all_type_defs(db);
    Rc::new(
        defs.iter()
            .filter_map(|def| match def {
                TypeDefId::Struct(id) => Some(*id),
                _ => None,
            })
            .collect(),
    )
}
