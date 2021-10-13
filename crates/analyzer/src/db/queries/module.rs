use crate::builtins;
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
use fe_parser::node::Node;
use indexmap::indexmap;
use indexmap::map::{Entry, IndexMap};
use std::collections::HashSet;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;
use strum::IntoEnumIterator;

// Placeholder; someday std::prelude will be a proper module.
fn std_prelude_items() -> IndexMap<String, Item> {
    let mut items = indexmap! {
        "bool".to_string() => Item::Type(TypeDef::Primitive(types::Base::Bool)),
        "address".to_string() => Item::Type(TypeDef::Primitive(types::Base::Address)),
    };
    items.extend(types::Integer::iter().map(|typ| {
        (
            typ.as_ref().to_string(),
            Item::Type(TypeDef::Primitive(types::Base::Numeric(typ))),
        )
    }));
    items.extend(
        types::GenericType::iter().map(|typ| (typ.name().to_string(), Item::GenericType(typ))),
    );
    items.extend(
        builtins::GlobalFunction::iter()
            .map(|fun| (fun.as_ref().to_string(), Item::BuiltinFunction(fun))),
    );
    items.extend(
        builtins::GlobalObject::iter().map(|obj| (obj.as_ref().to_string(), Item::Object(obj))),
    );
    items
}

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
                    name: node.name().to_string(),
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
        })
        .collect();
    Rc::new(items)
}

pub fn module_item_map(
    db: &dyn AnalyzerDb,
    module: ModuleId,
) -> Analysis<Rc<IndexMap<String, Item>>> {
    let builtin_items = std_prelude_items();
    let sub_modules = module
        .sub_modules(db)
        .iter()
        .map(|(name, id)| (name.clone(), Item::Module(*id)))
        .collect::<IndexMap<_, _>>();
    let used_items = db.module_used_item_map(module);
    let mut diagnostics = (*used_items.diagnostics).clone();

    let mut map = IndexMap::<String, Item>::new();

    for item in module.all_items(db).iter() {
        let item_name = item.name(db);
        if let Some(builtin) = builtin_items.get(&item_name) {
            let builtin_kind = builtin.item_kind_display_name();
            diagnostics.push(errors::error(
                &format!("type name conflicts with built-in {}", builtin_kind),
                item.name_span(db).expect("duplicate built-in names?"),
                &format!("`{}` is a built-in {}", item_name, builtin_kind),
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

        match map.entry(item_name) {
            Entry::Occupied(entry) => {
                diagnostics.push(errors::fancy_error(
                    "duplicate type name",
                    vec![
                        Label::primary(
                            entry.get().name_span(db).unwrap(),
                            format!("`{}` first defined here", entry.key()),
                        ),
                        Label::secondary(
                            item.name_span(db)
                                .expect("built-in conflicts with user-defined name?"),
                            format!("`{}` redefined here", entry.key()),
                        ),
                    ],
                    vec![],
                ));
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
                .chain(builtin_items)
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
            // TODO: this needs dependency graph stuff
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
) -> Analysis<Rc<IndexMap<String, (Span, Item)>>> {
    let mut diagnostics = vec![];

    let ast::Module { body } = &module.data(db).ast;

    let items = body
        .iter()
        .fold(indexmap! {}, |mut accum, stmt| {
            if let ast::ModuleStmt::Use(use_stmt) = stmt {
                let parent = module
                    .parent_module(db)
                    .expect("module does not have a parent");
                let items = db.module_resolve_use_tree(parent, use_stmt.kind.tree.clone());
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
            let builtin_items = std_prelude_items();

            if let Some(builtin) = builtin_items.get(&name) {
                let builtin_kind = builtin.item_kind_display_name();

                diagnostics.push(errors::error(
                    &format!("import name conflicts with built-in {}", builtin_kind),
                    name_span,
                    &format!("`{}` is a built-in {}", name, builtin_kind),
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

pub fn module_resolve_use_tree(
    db: &dyn AnalyzerDb,
    module: ModuleId,
    tree: Node<ast::UseTree>,
) -> Analysis<Rc<IndexMap<String, (Span, Item)>>> {
    let mut diagnostics = vec![];

    match &tree.kind {
        ast::UseTree::Glob { prefix } => {
            let prefix_module = Item::Module(module).resolve_path(db, prefix);
            diagnostics.extend(prefix_module.diagnostics.deref().clone());

            let items = match prefix_module.value {
                Some(Item::Module(module)) => (*module.items(db))
                    .clone()
                    .into_iter()
                    .map(|(name, item)| (name, (tree.span, item)))
                    .collect(),
                Some(item) => {
                    diagnostics.push(errors::error(
                        format!("cannot glob import from {}", item.item_kind_display_name()),
                        prefix.segments.last().expect("path is empty").span,
                        "prefix item must be a module",
                    ));
                    indexmap! {}
                }
                None => indexmap! {},
            };

            Analysis {
                value: Rc::new(items),
                diagnostics: Rc::new(diagnostics),
            }
        }
        ast::UseTree::Nested { prefix, children } => {
            let prefix_module = Item::Module(module).resolve_path(db, prefix);
            diagnostics.extend(prefix_module.diagnostics.deref().clone());

            let items = match prefix_module.value {
                Some(Item::Module(module)) => {
                    children.iter().fold(indexmap! {}, |mut accum, node| {
                        let child_items = db.module_resolve_use_tree(module, node.clone());
                        diagnostics.extend(child_items.diagnostics.deref().clone());

                        for (name, (name_span, item)) in child_items.value.iter() {
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

                        accum
                    })
                }
                Some(item) => {
                    diagnostics.push(errors::error(
                        format!("cannot glob import from {}", item.item_kind_display_name()),
                        prefix.segments.last().unwrap().span,
                        "prefix item must be a module",
                    ));
                    indexmap! {}
                }
                None => indexmap! {},
            };

            Analysis {
                value: Rc::new(items),
                diagnostics: Rc::new(diagnostics),
            }
        }
        ast::UseTree::Simple { path, rename } => {
            let item = Item::Module(module).resolve_path(db, path);

            let items = match item.value {
                Some(item) => {
                    let (item_name, item_name_span) = if let Some(name) = rename {
                        (name.kind.clone(), name.span)
                    } else {
                        let name_segment_node = path.segments.last().expect("path is empty");
                        (name_segment_node.kind.clone(), name_segment_node.span)
                    };

                    indexmap! { item_name => (item_name_span, item) }
                }
                None => indexmap! {},
            };

            Analysis {
                value: Rc::new(items),
                diagnostics: item.diagnostics,
            }
        }
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

pub fn module_adjacent_modules(
    db: &dyn AnalyzerDb,
    module: ModuleId,
) -> Rc<IndexMap<String, ModuleId>> {
    if let Some(parent) = module.parent_module(db) {
        parent.sub_modules(db)
    } else {
        Rc::new(indexmap! {})
    }
}

pub fn module_sub_modules(db: &dyn AnalyzerDb, module: ModuleId) -> Rc<IndexMap<String, ModuleId>> {
    match module.context(db) {
        ModuleContext::Ingot(ingot) => {
            let all_modules = ingot.all_modules(db);

            match module.file_content(db) {
                ModuleFileContent::Dir { dir_path } => {
                    let sub_modules = all_modules
                        .iter()
                        .filter(|module_id| {
                            Path::new(&module_id.ingot_path(db))
                                .parent()
                                .expect("module file in ingot does not have parent path")
                                == Path::new(&dir_path)
                        })
                        .map(|module_id| (module_id.name(db), *module_id))
                        .collect::<IndexMap<_, _>>();
                    Rc::new(sub_modules)
                }
                // file modules do not have sub-modules (for now, at least)
                ModuleFileContent::File { .. } => Rc::new(indexmap! {}),
            }
        }
        // if we are compiling a module in the global context, then it will not have any sub-modules
        ModuleContext::Global(_) => Rc::new(indexmap! {}),
    }
}
