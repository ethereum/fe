use std::collections::BTreeSet;

use fe_analyzer::namespace::items::ContractId;
use fe_mir::ir::FunctionId;
use fxhash::FxHashSet;
use yultsur::yul;

use crate::{
    db::CodegenDb,
    yul::runtime::{DefaultRuntimeProvider, RuntimeProvider},
};

use super::{lower_contract_deployable, lower_function};

pub struct Context {
    pub runtime: Box<dyn RuntimeProvider>,
    pub(super) contract_dependency: BTreeSet<ContractId>,
    pub(super) function_dependency: BTreeSet<FunctionId>,
    pub(super) string_constants: BTreeSet<String>,
    pub(super) lowered_functions: FxHashSet<FunctionId>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            runtime: Box::new(DefaultRuntimeProvider::default()),
            contract_dependency: BTreeSet::default(),
            function_dependency: BTreeSet::default(),
            string_constants: BTreeSet::default(),
            lowered_functions: FxHashSet::default(),
        }
    }
}

impl Context {
    pub(super) fn resolve_function_dependency(
        &mut self,
        db: &dyn CodegenDb,
    ) -> Vec<yul::FunctionDefinition> {
        let mut funcs = vec![];
        loop {
            let dependencies = std::mem::take(&mut self.function_dependency);
            if dependencies.is_empty() {
                break;
            }
            for dependency in dependencies {
                if self.lowered_functions.contains(&dependency) {
                    // Ignore dependency if it's already lowered.
                    continue;
                } else {
                    funcs.push(lower_function(db, self, dependency))
                }
            }
        }

        funcs
    }

    pub(super) fn resolve_constant_dependency(&self, db: &dyn CodegenDb) -> Vec<yul::Data> {
        self.string_constants
            .iter()
            .map(|s| {
                let symbol = db.codegen_constant_string_symbol_name(s.to_string());
                yul::Data {
                    name: symbol.as_ref().clone(),
                    value: s.to_string(),
                }
            })
            .collect()
    }

    pub(super) fn resolve_contract_dependency(&self, db: &dyn CodegenDb) -> Vec<yul::Object> {
        self.contract_dependency
            .iter()
            .map(|cid| lower_contract_deployable(db, *cid))
            .collect()
    }
}
