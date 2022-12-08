use fe_mir::ir::FunctionSigId;
use indexmap::IndexMap;
use yultsur::yul;

use crate::{
    cgu::CodegenUnit,
    db::CodegenDb,
    yul::runtime::{DefaultRuntimeProvider, RuntimeProvider},
};

use super::lower_function;

pub(super) fn compile_cgu(db: &dyn CodegenDb, cgu: CodegenUnit) -> CompiledCgu {
    let mut ctx = CguContext::default();
    let mut functions = IndexMap::with_capacity(cgu.functions.len());
    for func in cgu.functions {
        let sig = func.sig;
        let yul_func = lower_function(db, &mut ctx, func);
        functions.insert(sig, yul_func);
    }

    let mut runtime_functions = IndexMap::default();

    for func in ctx.runtime.collect_definitions() {
        runtime_functions.insert(func.name.identifier.clone(), func);
    }

    CompiledCgu {
        functions,
        constants: ctx.string_constants,
        runtime_functions,
    }
}

pub(super) struct CompiledCgu {
    functions: IndexMap<FunctionSigId, yul::FunctionDefinition>,
    constants: IndexMap<String, yul::Data>,
    runtime_functions: IndexMap<String, yul::FunctionDefinition>,
}

pub struct CguContext {
    pub(super) runtime: Box<dyn RuntimeProvider>,
    pub(super) string_constants: IndexMap<String, yul::Data>,
}

impl CguContext {
    pub fn add_string_constant(&mut self, db: &dyn CodegenDb, s: &str) {
        let symbol_name = db.codegen_constant_string_symbol_name(s.into());
        if self.string_constants.contains_key(symbol_name.as_ref()) {
            return;
        }

        let data = yul::Data {
            name: symbol_name.as_ref().clone(),
            value: s.to_string(),
        };
        self.string_constants
            .insert(symbol_name.as_ref().clone(), data);
    }
}

impl Default for CguContext {
    fn default() -> Self {
        Self {
            runtime: Box::new(DefaultRuntimeProvider::default()),
            string_constants: IndexMap::default(),
        }
    }
}
