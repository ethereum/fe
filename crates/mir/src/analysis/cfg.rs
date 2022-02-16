use fxhash::FxHashMap;

use crate::ir::{inst::BranchInfo, BasicBlockId, FunctionBody, InstId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ControlFlowGraph {
    entry: BasicBlockId,
    blocks: FxHashMap<BasicBlockId, BlockNode>,
}

impl ControlFlowGraph {
    pub fn compute(func: &FunctionBody) -> Self {
        let entry = func.order.entry_block();
        let mut cfg = Self {
            entry,
            blocks: FxHashMap::default(),
        };

        for block in func.order.iter_block() {
            let terminator = func.order.terminator(&func.store, block);
            cfg.analyze_terminator(func, terminator);
        }

        cfg
    }

    pub fn preds(&self, block: BasicBlockId) -> &[BasicBlockId] {
        self.blocks[&block].preds()
    }

    pub fn succs(&self, block: BasicBlockId) -> &[BasicBlockId] {
        self.blocks[&block].succs()
    }

    fn analyze_terminator(&mut self, func: &FunctionBody, terminator: InstId) {
        let block = func.order.inst_block(terminator);
        match func.store.branch_info(terminator) {
            BranchInfo::NotBranch => {}
            BranchInfo::Jump(dest) => self.add_edge(block, dest),
            BranchInfo::Branch((then, else_)) => {
                self.add_edge(block, then);
                self.add_edge(block, else_);
            }
        }
    }

    fn add_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        self.node_mut(to).push_pred(from);
        self.node_mut(from).push_succ(to);
    }

    fn node_mut(&mut self, block: BasicBlockId) -> &mut BlockNode {
        self.blocks.entry(block).or_default()
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
struct BlockNode {
    preds: Vec<BasicBlockId>,
    succs: Vec<BasicBlockId>,
}

impl BlockNode {
    fn push_pred(&mut self, pred: BasicBlockId) {
        self.preds.push(pred);
    }

    fn push_succ(&mut self, succ: BasicBlockId) {
        self.succs.push(succ);
    }

    fn preds(&self) -> &[BasicBlockId] {
        &self.preds
    }

    fn succs(&self) -> &[BasicBlockId] {
        &self.succs
    }
}
