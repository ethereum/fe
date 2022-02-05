//! Fe Lowering.

use fe_analyzer::namespace::items::{Ingot, IngotId, Module, ModuleId, ModuleSource};

mod ast_utils;
mod context;
pub mod db;
mod mappers;
mod names;
mod utils;

pub use crate::db::{LoweringDb, TestDb};

use std::rc::Rc;

pub fn lower_main_module(db: &mut dyn LoweringDb, module: ModuleId) -> ModuleId {
    let original_ingot = module.ingot(db.upcast());
    let lowered_ingot = lower_ingot(db, original_ingot);

    let lowered_mod = lowered_ingot
        .root_module(db.upcast())
        .expect("lowered ingot has no main module");

    lowered_mod
}

/// Lower every module of an ingot.
///
/// Creates a new `IngotId` and new `ModuleId`s for the ingot's modules.
pub fn lower_ingot(db: &mut dyn LoweringDb, ingot: IngotId) -> IngotId {
    let data = ingot.data(db.upcast());
    let lowered_ingot = db.intern_ingot(Rc::new(Ingot {
        name: data.name.clone(),
        mode: data.mode,
        original: Some(ingot),
        src_dir: data.src_dir.clone(),
    }));

    let lowered_mods = ingot
        .all_modules(db.upcast())
        .iter()
        .map(|module| lower_module(db, *module, lowered_ingot))
        .collect();

    db.set_ingot_modules(lowered_ingot, lowered_mods);
    db.set_ingot_external_ingots(lowered_ingot, ingot.external_ingots(db.upcast()));
    lowered_ingot
}

fn lower_module(db: &dyn LoweringDb, module: ModuleId, lowered_ingot: IngotId) -> ModuleId {
    let data = module.data(db.upcast());

    assert!(
        !matches!(data.source, ModuleSource::Lowered { .. }),
        "lowering an already lowered module"
    );

    db.intern_module(Rc::new(Module {
        name: data.name.clone(),
        ingot: lowered_ingot,
        source: ModuleSource::Lowered {
            original: module,
            ast: db.lowered_module_ast(module),
        },
    }))
}
