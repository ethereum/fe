//! This module includes the data structures and algorithms for building the
//! dependency graphs both for contracts and functions.
//!
//! NOTE: The graph itself is not yul-specific structure, but these
//! dependencies will be properly resolved by the sonatina-builtin linker. After
//! all, these graphs are only used by yul, and that's why this module is
//! defined inside `yul` module.

use fe_analyzer::namespace::items::{ContractId, ModuleId};
use fe_mir::ir::{
    inst::{CallType, InstKind},
    FunctionSigId,
};
use fxhash::FxHashMap;
use petgraph::{
    graph::{DiGraph, NodeIndex},
    visit::Dfs,
};

use crate::{cgu::CodegenUnitId, db::CodegenDb};

#[derive(Debug, Default)]
pub(crate) struct DependencyGraph {
    dep_graph: DiGraph<DepNode, ()>,
    func_node_map: FxHashMap<FunctionSigId, NodeIndex>,
    contract_node_map: FxHashMap<ContractId, NodeIndex>,
    contract_scope: Option<NodeIndex>,
}

#[derive(Debug)]
enum DepNode {
    Func(FunctionSigId),
    Contract(ContractId),
}

impl DependencyGraph {
    pub fn collect_dep_funcs(&self, func: FunctionSigId) -> Vec<FunctionSigId> {
        let mut deps = Vec::new();
        let mut dfs = Dfs::new(&self.dep_graph, self.func_node_map[&func]);
        while let Some(node) = dfs.next(&self.dep_graph) {
            if let DepNode::Func(func) = self.dep_graph.node_weight(node).unwrap() {
                deps.push(*func);
            }
        }
        deps
    }

    /// Returns the list of contracts that `func` depends on.
    /// The returned contracts are limited to immediate successors of the
    /// functions.
    pub fn collect_dep_contracts(&self, func: FunctionSigId) -> Vec<ContractId> {
        self.dep_graph
            .neighbors(self.func_node_map[&func])
            .filter_map(|node| {
                if let DepNode::Contract(contract) = self.dep_graph.node_weight(node).unwrap() {
                    Some(*contract)
                } else {
                    None
                }
            })
            .collect()
    }

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
            self.update_graph_by_func(db, codegen_units, func);
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
        let cgu = cgus[&module].data(db);
        let cgu_func = &cgu.functions[&func];
        let body = &cgu_func.body;

        for block in body.order.iter_block() {
            for inst in body.order.iter_inst(block) {
                match &body.store.inst_data(inst).kind {
                    InstKind::Call {
                        func: callee,
                        call_type: CallType::Internal,
                        ..
                    } => {
                        self.update_graph_by_func(db, cgus, *callee);
                        let callee_node = self.func_node_map[callee];
                        self.dep_graph.update_edge(func_node, callee_node, ());
                    }

                    InstKind::Create { contract, .. } | InstKind::Create2 { contract, .. } => {
                        self.update_graph_by_contract(db, cgus, *contract);
                        let contract_node = self.contract_node_map[contract];
                        self.dep_graph
                            .update_edge(self.contract_scope.unwrap(), contract_node, ());
                        self.dep_graph.update_edge(func_node, contract_node, ());
                    }

                    _ => {}
                }
            }
        }
    }

    fn add_func_node(&mut self, func: FunctionSigId) -> NodeIndex {
        debug_assert!(!self.func_node_map.contains_key(&func));
        let node = self.dep_graph.add_node(DepNode::Func(func));
        self.func_node_map.insert(func, node);
        node
    }

    fn add_contract_node(&mut self, contract: ContractId) -> NodeIndex {
        debug_assert!(!self.contract_node_map.contains_key(&contract));
        let node = self.dep_graph.add_node(DepNode::Contract(contract));
        self.contract_node_map.insert(contract, node);
        node
    }

    fn set_contract_scope(&mut self, contract: NodeIndex) -> Option<NodeIndex> {
        std::mem::replace(&mut self.contract_scope, Some(contract))
    }
}
