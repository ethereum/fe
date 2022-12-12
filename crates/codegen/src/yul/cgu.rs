use fe_mir::ir::FunctionSigId;
use indexmap::{IndexMap, IndexSet};
use yultsur::yul;

use crate::{
    cgu::CodegenUnitId,
    db::CodegenDb,
    yul::{
        isel::lower_function,
        runtime::{DefaultRuntimeProvider, RuntimeProvider},
    },
};

pub(crate) fn compile_cgu(db: &dyn CodegenDb, cgu: CodegenUnitId) -> CompiledCgu {
    let mut runtime_func_deps = IndexMap::new();
    let cgu_functions = &cgu.data(db).functions;
    let mut functions = IndexMap::with_capacity(cgu_functions.len());
    let mut runtime_funcs = IndexMap::new();
    let mut constants = IndexSet::new();

    for func in cgu_functions.values() {
        let mut ctx = CguLowerContext::default();
        let sig = func.sig;
        let yul_func = lower_function(db, &mut ctx, &mut func.clone());
        // Ensures all signatures have its own runtime function entry.
        runtime_func_deps.insert(sig, IndexSet::new());
        for runtime_func in ctx.runtime.collect_definitions() {
            let name = &runtime_func.name.identifier;
            runtime_func_deps[&sig].insert(name.clone());
            runtime_funcs.insert(name.clone(), runtime_func);
        }

        constants.extend(ctx.string_constants.into_iter());
        functions.insert(sig, yul_func);
    }

    CompiledCgu {
        functions,
        constants,
        runtime_funcs,
        runtime_func_deps,
    }
}

#[derive(Default, Debug)]
pub struct CompiledCgu {
    pub(super) functions: IndexMap<FunctionSigId, yul::FunctionDefinition>,
    pub(super) constants: IndexSet<yul::Data>,
    // NOTE: The below runtime function related fields are removed when std library and intrinsics
    // are properly defined.
    pub(super) runtime_funcs: IndexMap<String, yul::FunctionDefinition>,
    pub(super) runtime_func_deps: IndexMap<FunctionSigId, IndexSet<String>>,
}

pub(crate) struct CguLowerContext {
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
