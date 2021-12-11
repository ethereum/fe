use crate::context::Analysis;
use crate::namespace::items::{IngotId, Module, ModuleContext, ModuleFileContent, ModuleId};
use crate::AnalyzerDb;
use fe_common::diagnostics::{Diagnostic, Severity};
use fe_parser::ast;
use indexmap::set::IndexSet;
use std::path::Path;
use std::rc::Rc;

pub fn ingot_all_modules(db: &dyn AnalyzerDb, ingot_id: IngotId) -> Rc<Vec<ModuleId>> {
    let ingot = &ingot_id.data(db);

    let file_modules = ingot
        .fe_files
        .values()
        .into_iter()
        .map(|(file, ast)| {
            let module = Module {
                name: Path::new(&file.name)
                    .file_stem()
                    .expect("file does not have stem")
                    .to_str()
                    .expect("could not convert file stem to string")
                    .into(),
                ast: ast.clone(),
                file_content: ModuleFileContent::File { file: file.id },
                context: ModuleContext::Ingot(ingot_id),
            };

            db.intern_module(Rc::new(module))
        })
        .collect::<Vec<_>>();

    let dir_modules = ingot
        .fe_files
        .values()
        .into_iter()
        .map(|(file, _)| {
            Path::new(&file.name)
                .parent()
                .expect("file does not have parent path")
        })
        .collect::<IndexSet<_>>()
        .into_iter()
        .map(|dir| {
            let module = Module {
                name: dir
                    .file_name()
                    .expect("missing file name")
                    .to_str()
                    .expect("could not convert dir name to string")
                    .into(),
                ast: ast::Module { body: vec![] },
                context: ModuleContext::Ingot(ingot_id),
                file_content: ModuleFileContent::Dir {
                    dir_path: dir
                        .to_str()
                        .expect("could not convert dir path to string")
                        .into(),
                },
            };

            db.intern_module(Rc::new(module))
        })
        .collect::<Vec<ModuleId>>();

    let all_modules = [file_modules, dir_modules].concat();
    Rc::new(all_modules)
}

pub fn ingot_main_module(db: &dyn AnalyzerDb, ingot_id: IngotId) -> Analysis<Option<ModuleId>> {
    let main_id = ingot_id
        .all_modules(db)
        .iter()
        .find(|module_id| {
            module_id.name(db) == "main" && {
                if let Some(parent_id) = module_id.parent_module(db) {
                    parent_id.name(db) == "src"
                } else {
                    false
                }
            }
        })
        .copied();

    Analysis {
        value: main_id,
        diagnostics: Rc::new({
            if main_id.is_none() {
                vec![Diagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "The ingot named \"{}\" is missing a main module. \
                            \nPlease add a `src/main.fe` file to the base directory.",
                        ingot_id.name(db)
                    ),
                    labels: vec![],
                    notes: vec![],
                }]
            } else {
                vec![]
            }
        }),
    }
}
