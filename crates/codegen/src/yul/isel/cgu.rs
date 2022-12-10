use fe_mir::ir::FunctionSigId;
use indexmap::{IndexMap, IndexSet};
use yultsur::yul;

use crate::{
    cgu::CodegenUnit,
    db::CodegenDb,
    yul::runtime::{DefaultRuntimeProvider, RuntimeProvider},
};

use super::lower_function;

pub(super) fn compile_cgu(db: &dyn CodegenDb, cgu: CodegenUnit) -> CompiledCgu {
    let mut runtime_func_deps = IndexMap::default();
    let mut functions = IndexMap::with_capacity(cgu.functions.len());
    let mut runtime_funcs = IndexMap::default();
    let mut constants = IndexSet::default();

    for func in cgu.functions {
        let mut ctx = CguLowerContext::default();
        let sig = func.sig;
        let yul_func = lower_function(db, &mut ctx, func);
        for runtime_func in ctx.runtime.collect_definitions() {
            runtime_func_deps
                .entry(sig)
                .or_insert_with(Vec::new)
                .push(runtime_func.name.identifier.clone());
            runtime_funcs.insert(runtime_func.name.identifier.clone(), runtime_func);
            constants.extend(ctx.string_constants);
        }
        functions.insert(sig, yul_func);
    }

    CompiledCgu {
        functions,
        constants,
        runtime_funcs,
        runtime_func_deps,
    }
}

#[derive(Default)]
pub(super) struct CompiledCgu {
    functions: IndexMap<FunctionSigId, yul::FunctionDefinition>,
    constants: IndexSet<yul::Data>,
    runtime_funcs: IndexMap<String, yul::FunctionDefinition>,
    runtime_func_deps: IndexMap<FunctionSigId, Vec<String>>,
}

pub(super) struct CguLowerContext {
    pub(super) runtime: Box<dyn RuntimeProvider>,
    pub(super) string_constants: IndexSet<yul::Data>,
}

impl Default for CguLowerContext {
    fn default() -> Self {
        Self {
            runtime: Box::new(DefaultRuntimeProvider::default()),
            string_constants: IndexSet::default(),
        }
    }
}

impl CguLowerContext {
    pub(super) fn add_string_constant(&mut self, db: &dyn CodegenDb, s: &str) {
        let symbol_name = db.codegen_constant_string_symbol_name(s.into());
        let data = yul::Data {
            name: symbol_name.as_ref().clone(),
            value: s.to_string(),
        };
        self.string_constants.insert(data);
    }
}
