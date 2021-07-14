use crate::context::Analysis;
use crate::db::AnalyzerDb;
use crate::errors;
use crate::namespace::items::{Contract, ModuleId, Struct, TypeAlias, TypeDefId};
use crate::namespace::types;
use fe_common::diagnostics::Diagnostic;
use fe_common::diagnostics::Label;
use fe_parser::ast;
use fe_parser::node::Node;
use indexmap::map::{Entry, IndexMap};
use semver::{Version, VersionReq};
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
) -> Option<Rc<types::Type>> {
    let type_defs = module.type_def_map(db);
    Some(type_defs.get(&name)?.typ(db))
}

pub fn module_diagnostics(db: &dyn AnalyzerDb, module: ModuleId) -> Rc<Vec<Diagnostic>> {
    let mut diags = vec![];
    let ast::Module { body } = &module.data(db).ast;
    for stmt in body {
        match stmt {
            ast::ModuleStmt::Pragma(inner) => diags.extend(check_pragma_version(inner).into_iter()),
            ast::ModuleStmt::Import(inner) => {
                diags.push(errors::not_yet_implemented("import", inner.span))
            }
            _ => {} // everything else is a type def, handled below.
        }
    }

    // duplicate type name errors
    diags.extend(db.module_type_def_map(module).diagnostics.iter().cloned());

    // errors for each type def
    for type_def in module.all_type_defs(db).iter() {
        diags.extend(type_def.diagnostics(db).iter().cloned())
    }
    Rc::new(diags)
}

fn check_pragma_version(stmt: &Node<ast::Pragma>) -> Option<Diagnostic> {
    let version_requirement = &stmt.kind.version_requirement;
    // This can't fail because the parser already validated it
    let requirement =
        VersionReq::parse(&version_requirement.kind).expect("Invalid version requirement");
    let actual_version =
        Version::parse(env!("CARGO_PKG_VERSION")).expect("Missing package version");

    if !requirement.matches(&actual_version) {
        Some(errors::fancy_error(
            format!(
                "The current compiler version {} doesn't match the specified requirement",
                actual_version
            ),
            vec![Label::primary(
                version_requirement.span,
                "The specified version requirement",
            )],
            vec![format!(
                "Note: Use `pragma {}` to make the code compile",
                actual_version
            )],
        ))
    } else {
        None
    }
}
