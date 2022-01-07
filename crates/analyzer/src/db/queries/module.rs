use crate::context::{Analysis, AnalyzerContext};
use crate::db::AnalyzerDb;
use crate::errors::{self, TypeError};
use crate::namespace::items::{
    Contract, ContractId, Function, Item, ModuleConstant, ModuleConstantId, ModuleContext,
    ModuleFileContent, ModuleId, Struct, StructId, TypeAlias, TypeDef,
};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types::{self, Type};
use crate::traversal::types::type_desc;
use fe_common::diagnostics::Label;
use fe_common::Span;
use fe_parser::ast;
use indexmap::indexmap;
use indexmap::map::{Entry, IndexMap};
use smol_str::SmolStr;
use std::collections::HashSet;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

pub fn module_all_items(db: &dyn AnalyzerDb, module: ModuleId) -> Rc<Vec<Item>> {
    let ast::Module { body } = &module.data(db).ast;

    let items = body
        .iter()
        .filter_map(|stmt| match stmt {
            ast::ModuleStmt::TypeAlias(node) => Some(Item::Type(TypeDef::Alias(
                db.intern_type_alias(Rc::new(TypeAlias {
                    ast: node.clone(),
                    module,
                })),
            ))),
            ast::ModuleStmt::Contract(node) => Some(Item::Type(TypeDef::Contract(
                db.intern_contract(Rc::new(Contract {
                    name: node.name().into(),
                    ast: node.clone(),
                    module,
                })),
            ))),
            ast::ModuleStmt::Struct(node) => Some(Item::Type(TypeDef::Struct(db.intern_struct(
                Rc::new(Struct {
                    ast: node.clone(),
                    module,
                }),
            )))),
            ast::ModuleStmt::Constant(node) => Some(Item::Constant(db.intern_module_const(
                Rc::new(ModuleConstant {
                    ast: *node.clone(),
                    module,
                }),
            ))),
            ast::ModuleStmt::Function(node) => {
                Some(Item::Function(db.intern_function(Rc::new(Function {
                    ast: node.clone(),
                    module,
                    parent: None,
                }))))
            }
            ast::ModuleStmt::Pragma(_) => None,
            ast::ModuleStmt::Use(_) => None,
            ast::ModuleStmt::Event(_) => todo!(),
        })
        .collect();
    Rc::new(items)
}

pub fn module_item_map(
    db: &dyn AnalyzerDb,
    module: ModuleId,
) -> Analysis<Rc<IndexMap<SmolStr, Item>>> {
    // we must check for conflicts with global item names
    let global_items = module.global_items(db);

    // sub modules and used items are included in this map
    let sub_modules = module
        .sub_modules(db)
        .iter()
        .map(|(name, id)| (name.clone(), Item::Module(*id)))
        .collect::<IndexMap<_, _>>();
    let used_items = db.module_used_item_map(module);

    let mut diagnostics = used_items.diagnostics.deref().clone();
    let mut map = IndexMap::<SmolStr, Item>::new();

    for item in module.all_items(db).iter() {
        let item_name = item.name(db);
        if let Some(global_item) = global_items.get(&item_name) {
            let kind = item.item_kind_display_name();
            let other_kind = global_item.item_kind_display_name();
            diagnostics.push(errors::error(
                &format!(
                    "{} name conflicts with the {} named \"{}\"",
                    kind, other_kind, item_name
                ),
                item.name_span(db)
                    .expect("user defined item is missing a name span"),
                &format!("`{}` is already defined", item_name),
            ));
            continue;
        }

        if let Some((used_item_name_span, used_item)) = used_items.value.get(&item_name) {
            diagnostics.push(errors::duplicate_name_error(
                &format!(
                    "a {} with the same name has already been imported",
                    used_item.item_kind_display_name()
                ),
                &item.name(db),
                *used_item_name_span,
                item.name_span(db).expect("missing name span"),
            ));
            continue;
        }

        match map.entry(item_name.clone()) {
            Entry::Occupied(entry) => {
                if let Some(entry_name_span) = entry.get().name_span(db) {
                    diagnostics.push(errors::duplicate_name_error(
                        &format!(
                            "a {} named \"{}\" has already been defined",
                            entry.get().item_kind_display_name(),
                            item_name
                        ),
                        &item_name,
                        entry_name_span,
                        item.name_span(db)
                            .expect("used-defined item does not have name span"),
                    ));
                } else {
                    diagnostics.push(errors::fancy_error(
                        &format!(
                            "a {} named \"{}\" has already been defined",
                            entry.get().item_kind_display_name(),
                            item_name
                        ),
                        vec![Label::primary(
                            item.name_span(db)
                                .expect("used-defined item does not have name span"),
                            format!("`{}` redefined here", entry.key()),
                        )],
                        vec![],
                    ));
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(*item);
            }
        }
    }
    Analysis {
        value: Rc::new(
            map.into_iter()
                .chain(sub_modules)
                .chain(
                    used_items
                        .value
                        .iter()
                        .map(|(name, (_, item))| (name.to_owned(), *item)),
                )
                .collect::<IndexMap<_, _>>(),
        ),
        diagnostics: Rc::new(diagnostics),
    }
}

pub fn module_contracts(db: &dyn AnalyzerDb, module: ModuleId) -> Rc<Vec<ContractId>> {
    Rc::new(
        module
            .all_items(db)
            .iter()
            .filter_map(|item| match item {
                Item::Type(TypeDef::Contract(id)) => Some(*id),
                _ => None,
            })
            .collect(),
    )
}

pub fn module_structs(db: &dyn AnalyzerDb, module: ModuleId) -> Rc<Vec<StructId>> {
    Rc::new(
        module
            .all_items(db)
            .iter()
            .chain(
                module
                    .used_items(db)
                    .values()
                    .into_iter()
                    .map(|(_, item)| item),
            )
            .filter_map(|item| match item {
                Item::Type(TypeDef::Struct(id)) => Some(*id),
                _ => None,
            })
            .collect(),
    )
}

pub fn module_constant_type(
    db: &dyn AnalyzerDb,
    constant: ModuleConstantId,
) -> Analysis<Result<types::Type, TypeError>> {
    let mut scope = ItemScope::new(db, constant.data(db).module);
    let typ = type_desc(&mut scope, &constant.data(db).ast.kind.typ);

    match &typ {
        Ok(typ) if !matches!(typ, Type::Base(_)) => {
            scope.error(
                "Non-base types not yet supported for constants",
                constant.data(db).ast.kind.typ.span,
                &format!("this has type `{}`; expected a primitive type", typ),
            );
        }
        _ => {}
    }

    Analysis {
        value: typ,
        diagnostics: Rc::new(scope.diagnostics),
    }
}

pub fn module_used_item_map(
    db: &dyn AnalyzerDb,
    module: ModuleId,
) -> Analysis<Rc<IndexMap<SmolStr, (Span, Item)>>> {
    // we must check for conflicts with the global items map
    let global_items = module.global_items(db);

    let mut diagnostics = vec![];
    let ast::Module { body } = &module.data(db).ast;

    let items = body
        .iter()
        .fold(indexmap! {}, |mut accum, stmt| {
            if let ast::ModuleStmt::Use(use_stmt) = stmt {
                let items = module.resolve_use_tree(db, &use_stmt.kind.tree, true);
                diagnostics.extend(items.diagnostics.deref().clone());

                for (name, (name_span, item)) in items.value.iter() {
                    if let Some((other_name_span, other_item)) =
                        accum.insert(name.to_owned(), (*name_span, *item))
                    {
                        diagnostics.push(errors::duplicate_name_error(
                            &format!(
                                "a {} with the same name has already been imported",
                                other_item.item_kind_display_name()
                            ),
                            name,
                            other_name_span,
                            *name_span,
                        ));
                    }
                }
            }

            accum
        })
        .into_iter()
        .filter_map(|(name, (name_span, item))| {
            if let Some(global_item) = global_items.get(&name) {
                let other_kind = global_item.item_kind_display_name();

                diagnostics.push(errors::error(
                    &format!(
                        "import name conflicts with the {} named \"{}\"",
                        other_kind, name
                    ),
                    name_span,
                    &format!("`{}` is already defined", name),
                ));

                None
            } else {
                Some((name, (name_span, item)))
            }
        })
        .collect::<IndexMap<_, _>>();

    Analysis {
        value: Rc::new(items),
        diagnostics: Rc::new(diagnostics),
    }
}

pub fn module_parent_module(db: &dyn AnalyzerDb, module: ModuleId) -> Option<ModuleId> {
    match module.context(db) {
        ModuleContext::Ingot(ingot) => {
            let all_modules = ingot.all_modules(db);

            for curr_module in all_modules.iter() {
                if curr_module
                    .sub_modules(db)
                    .values()
                    .collect::<HashSet<_>>()
                    .contains(&module)
                {
                    return Some(*curr_module);
                }
            }

            None
        }
        ModuleContext::Global(_) => None,
    }
}

pub fn module_sub_modules(
    db: &dyn AnalyzerDb,
    module: ModuleId,
) -> Rc<IndexMap<SmolStr, ModuleId>> {
    match module.context(db) {
        ModuleContext::Ingot(ingot) => {
            let all_modules = ingot.all_modules(db);

            match module.file_content(db) {
                ModuleFileContent::Dir { dir_path } => {
                    let sub_modules = all_modules
                        .iter()
                        .filter(|module_id| {
                            Path::new(module_id.ingot_path(db).as_str())
                                .parent()
                                .expect("module file in ingot does not have parent path")
                                == Path::new(dir_path.as_str())
                        })
                        .map(|module_id| (module_id.name(db), *module_id))
                        .collect::<IndexMap<_, _>>();
                    Rc::new(sub_modules)
                }
                ModuleFileContent::File { .. } => {
                    if Some(module) == ingot.root_module(db) {
                        ingot.root_sub_modules(db)
                    } else {
                        Rc::new(indexmap! {})
                    }
                }
            }
        }
        // if we are compiling a module in the global context, then it will not have any sub-modules
        ModuleContext::Global(_) => Rc::new(indexmap! {}),
    }
}
