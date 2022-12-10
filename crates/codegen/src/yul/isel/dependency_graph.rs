//! This module includes the data structures and algorithms for building the
//! dependency graphs both for contracts and functions.
//!
//! NOTE: The graph itself is not yul-specific structure, but these
//! dependencies will be properly resolved by the sonatina-builtin linker. After
//! all, these graphs are only used by yul, and that's why this module is
//! defined inside `yul` module.

use fe_analyzer::namespace::items::{ContractId, ModuleId};
use fe_mir::ir::{inst::InstKind, FunctionSigId};
use fxhash::FxHashMap;
use petgraph::graph::{DiGraph, NodeIndex};

use crate::{cgu::CodegenUnitId, db::CodegenDb};

#[derive(Default)]
pub(super) struct DependencyGraph {
    func_deps: DiGraph<FunctionSigId, ()>,
    contract_deps: DiGraph<ContractId, ()>,
    func_node_map: FxHashMap<FunctionSigId, NodeIndex>,
    contract_node_map: FxHashMap<ContractId, NodeIndex>,
    contract_scope: Option<NodeIndex>,
}

impl DependencyGraph {
    pub fn update_graph_by_contract(
        &mut self,
        db: &dyn CodegenDb,
        codegen_units: &FxHashMap<ModuleId, CodegenUnitId>,
        contract: ContractId,
    ) {
        if self.contract_node_map.contains_key(&contract) {
            return;
        }

        let contract_node = self.add_contract_node(contract);
        let prev_scope = self.set_contract_scope(contract_node);

        for func in contract.all_functions(db.upcast()).iter() {
            let func = db.mir_lowered_func_signature(func.sig(db.upcast()));
            let sig = self.update_graph_by_func(db, codegen_units, func);
        }

        if let Some(prev_scope) = prev_scope {
            self.set_contract_scope(prev_scope);
        }
    }

    fn update_graph_by_func(
        &mut self,
        db: &dyn CodegenDb,
        cgus: &FxHashMap<ModuleId, CodegenUnitId>,
        func: FunctionSigId,
    ) {
        if self.func_node_map.contains_key(&func) {
            return;
        }

        let func_node = self.add_func_node(func);

        let module = func.module(db.upcast());
        let cgu_func = cgus[&module].cgu_func(db, func);
        let body = &cgu_func.body;

        for block in body.order.iter_block() {
            for inst in body.order.iter_inst(block) {
                match &body.store.inst_data(inst).kind {
                    InstKind::Call { func: callee, .. } => {
                        self.update_graph_by_func(db, cgus, *callee);
                        let callee_node = self.func_node_map[&callee];
                        self.func_deps.update_edge(func_node, callee_node, ());
                    }

                    InstKind::Create { contract, .. } | InstKind::Create2 { contract, .. } => {
                        self.update_graph_by_contract(db, cgus, *contract);
                        let contract_node = self.contract_node_map[&contract];
                        self.contract_deps.update_edge(
                            self.contract_scope.unwrap(),
                            contract_node,
                            (),
                        );
                    }

                    _ => {}
                }
            }
        }
    }

    fn add_func_node(&mut self, func: FunctionSigId) -> NodeIndex {
        debug_assert!(!self.func_node_map.contains_key(&func));
        let node = self.func_deps.add_node(func);
        self.func_node_map.insert(func, node);
        node
    }

    fn add_contract_node(&mut self, contract: ContractId) -> NodeIndex {
        debug_assert!(!self.contract_node_map.contains_key(&contract));
        let node = self.contract_deps.add_node(contract);
        self.contract_node_map.insert(contract, node);
        node
    }

    fn set_contract_scope(&mut self, contract: NodeIndex) -> Option<NodeIndex> {
        std::mem::replace(&mut self.contract_scope, Some(contract))
    }
}
