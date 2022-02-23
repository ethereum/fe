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
            let terminator = func
                .order
                .terminator(&func.store, block)
                .expect("a block must have terminator");
            cfg.analyze_terminator(func, terminator);
        }

        cfg
    }

    pub fn entry(&self) -> BasicBlockId {
        self.entry
    }

    pub fn preds(&self, block: BasicBlockId) -> &[BasicBlockId] {
        self.blocks[&block].preds()
    }

    pub fn succs(&self, block: BasicBlockId) -> &[BasicBlockId] {
        self.blocks[&block].succs()
    }

    pub fn post_order(&self) -> CfgPostOrder {
        CfgPostOrder::new(self)
    }

    fn analyze_terminator(&mut self, func: &FunctionBody, terminator: InstId) {
        let block = func.order.inst_block(terminator);
        match func.store.branch_info(terminator) {
            BranchInfo::NotBranch => {
                self.node_mut(block);
            }
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

pub struct CfgPostOrder<'a> {
    cfg: &'a ControlFlowGraph,
    node_state: FxHashMap<BasicBlockId, NodeState>,
    stack: Vec<BasicBlockId>,
}

impl<'a> CfgPostOrder<'a> {
    fn new(cfg: &'a ControlFlowGraph) -> Self {
        let stack = vec![cfg.entry()];

        Self {
            cfg,
            node_state: FxHashMap::default(),
            stack,
        }
    }
}

impl<'a> Iterator for CfgPostOrder<'a> {
    type Item = BasicBlockId;

    fn next(&mut self) -> Option<BasicBlockId> {
        while let Some(&block) = self.stack.last() {
            let node_state = self.node_state.entry(block).or_default();
            if node_state.is_unvisited() {
                node_state.set_visited();
                for &succ in self.cfg.succs(block) {
                    let pred_state = self.node_state.entry(succ).or_default();
                    if pred_state.is_unvisited() {
                        self.stack.push(succ);
                    }
                }
            } else {
                self.stack.pop().unwrap();
                if !node_state.has_finished() {
                    node_state.set_finished();
                    return Some(block);
                }
            }
        }

        None
    }
}

#[derive(Default, Debug, Clone, Copy)]
struct NodeState(u8);

impl NodeState {
    fn is_unvisited(self) -> bool {
        self.0 == 0
    }

    fn has_finished(self) -> bool {
        self.0 == 2
    }

    fn set_visited(&mut self) {
        self.0 = 1;
    }

    fn set_finished(&mut self) {
        self.0 = 2;
    }
}
