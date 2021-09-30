use crate::builtins;
use crate::context::{Analysis, AnalyzerContext};
use crate::db::AnalyzerDb;
use crate::errors::{self, TypeError};
use crate::namespace::items::{
    Contract, ContractId, Item, ModuleConstant, ModuleConstantId, ModuleId, Struct, StructId,
    TypeAlias, TypeDef,
};
use crate::namespace::scopes::ItemScope;
use crate::namespace::types::{self, Type};
use crate::traversal::types::type_desc;
use fe_common::diagnostics::Label;
use fe_parser::ast;
use indexmap::indexmap;
use indexmap::map::{Entry, IndexMap};
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
        builtins::GlobalMethod::iter()
            .map(|fun| (fun.as_ref().to_string(), Item::BuiltinFunction(fun))),
    );
    items.extend(builtins::Object::iter().map(|obj| (obj.as_ref().to_string(), Item::Object(obj))));
    items
}

// This is probably too simple for real module imports
pub fn module_imported_item_map(_: &dyn AnalyzerDb, _: ModuleId) -> Rc<IndexMap<String, Item>> {
    Rc::new(std_prelude_items())
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
                    ast: node.clone(),
                    module,
                }),
            ))),
            ast::ModuleStmt::Pragma(_) => None,
            ast::ModuleStmt::Use(_) => todo!(),
        })
        .collect();
    Rc::new(items)
}

pub fn module_item_map(
    db: &dyn AnalyzerDb,
    module: ModuleId,
) -> Analysis<Rc<IndexMap<String, Item>>> {
    let mut diagnostics = vec![];

    let imports = db.module_imported_item_map(module);
    let mut map = IndexMap::<String, Item>::new();

    for item in module.all_items(db).iter() {
        let item_name = item.name(db);
        if let Some(builtin) = imports.get(&item_name) {
            let builtin_kind = builtin.item_kind_display_name();
            diagnostics.push(errors::error(
                &format!("type name conflicts with built-in {}", builtin_kind),
                item.name_span(db).expect("duplicate built-in names?"),
                &format!("`{}` is a built-in {}", item_name, builtin_kind),
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
        value: Rc::new(map),
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
