use fe_analyzer::namespace::items::{ContractId, IngotId, ModuleId};
use fxhash::FxHashMap;
use yultsur;

use crate::{cgu::CodegenUnitId, db::CodegenDb, yul};

#[derive(Debug, Default)]
pub struct CodegenContext {
    codegen_units: FxHashMap<ModuleId, CodegenUnitId>,
    compiled_units: FxHashMap<ModuleId, yul::cgu::CompiledCgu>,
    dependency_graph: yul::dependency_graph::DependencyGraph,
}

impl CodegenContext {
    pub fn attach_ingot(&mut self, db: &dyn CodegenDb, ingot: IngotId) {
        self.codegen_units
            .extend(db.codegen_generate_cgu(ingot).iter());
    }

    /// Compile all attached cgus into yul cgus.
    pub fn compile_ingots(&mut self, db: &dyn CodegenDb) {
        for (module_id, cgu_id) in self.codegen_units.iter() {
            let compiled_cgu = yul::cgu::compile_cgu(db, *cgu_id);
            self.compiled_units.insert(*module_id, compiled_cgu);
        }
    }

    /// This should be call after `compile_ingots` has been called.
    pub fn compile_contract(
        &mut self,
        db: &dyn CodegenDb,
        contract: ContractId,
    ) -> yultsur::yul::Object {
        self.dependency_graph
            .update_graph_by_contract(db, &self.codegen_units, contract);

        yul::isel::lower_deployable_contract(
            db,
            &self.compiled_units,
            &self.dependency_graph,
            contract,
        )
    }
}
