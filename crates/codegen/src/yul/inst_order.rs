#![allow(unused)]

use fe_mir::{
    analysis::{
        domtree::DFSet, loop_tree::LoopId, post_domtree::PostIDom, ControlFlowGraph, DomTree,
        LoopTree, PostDomTree,
    },
    ir::{inst::BranchInfo, BasicBlockId, FunctionBody, InstId, ValueId},
};

#[derive(Debug, Clone, Default)]
pub struct InstOrder {
    pub order: Vec<StructuralInst>,
}

#[derive(Debug, Clone)]
pub enum StructuralInst {
    Inst(InstId),
    If {
        cond: ValueId,
        then: Vec<StructuralInst>,
        else_: Vec<StructuralInst>,
    },
    For {
        body: Vec<StructuralInst>,
    },
    Break,
    Continue,
}

struct InstSerializer<'a> {
    body: &'a FunctionBody,
    cfg: ControlFlowGraph,
    loop_tree: LoopTree,
    df: DFSet,
    pd_tree: PostDomTree,
    scope: Option<Scope>,
}

impl<'a> InstSerializer<'a> {
    fn new(body: &'a FunctionBody) -> Self {
        let cfg = ControlFlowGraph::compute(body);
        let domtree = DomTree::compute(&cfg);
        let df = domtree.compute_df(&cfg);
        let pd_tree = PostDomTree::compute(body);
        let loop_tree = LoopTree::compute(&cfg, &domtree);

        Self {
            body,
            cfg,
            loop_tree,
            df,
            pd_tree,
            scope: None,
        }
    }

    fn serialize_insts(&mut self) -> InstOrder {
        self.scope = None;
        let entry = self.cfg.entry();
        let mut order = vec![];
        self.analyze_block(entry, &mut order);
        InstOrder { order }
    }

    fn analyze_block(&mut self, block: BasicBlockId, order: &mut Vec<StructuralInst>) {
        match self.loop_tree.loop_of_block(block) {
            Some(lp)
                if block == self.loop_tree.loop_header(lp)
                    && Some(block) != self.scope.as_ref().and_then(Scope::loop_header) =>
            {
                let loop_exit = self.find_loop_exit(lp);
                self.enter_loop_scope(block, loop_exit);
                let mut body = vec![];
                self.analyze_block(block, &mut body);
                self.exit_scope();
                order.push(StructuralInst::For { body });

                if let Some(exit) = loop_exit {
                    self.analyze_block(exit, order);
                }
                return;
            }
            _ => {}
        };

        for inst in self.body.order.iter_inst(block) {
            if self.body.store.is_terminator(inst) {
                break;
            }
            order.push(StructuralInst::Inst(inst));
        }

        let terminator = self.body.order.terminator(&self.body.store, block).unwrap();
        match self.analyze_terminator(terminator) {
            TerminatorInfo::If {
                cond,
                then,
                else_,
                merge_block,
            } => {
                let mut then_body = vec![];
                let mut else_body = vec![];

                self.enter_if_scope(merge_block);
                if let Some(merge_block) = merge_block {
                    if merge_block == else_ {
                        self.analyze_block(then, &mut then_body)
                    } else if merge_block == then {
                        self.analyze_block(else_, &mut else_body);
                    } else {
                        self.analyze_block(then, &mut then_body);
                        self.analyze_block(else_, &mut else_body);
                    }
                    order.push(StructuralInst::If {
                        cond,
                        then: then_body,
                        else_: else_body,
                    });
                    self.exit_scope();
                    self.analyze_block(merge_block, order)
                } else {
                    self.analyze_block(then, &mut then_body);
                    self.analyze_block(else_, &mut else_body);
                    self.exit_scope();
                    order.push(StructuralInst::If {
                        cond,
                        then: then_body,
                        else_: else_body,
                    });
                }
            }
            TerminatorInfo::ToMergeBlock => {}
            TerminatorInfo::Continue => order.push(StructuralInst::Continue),
            TerminatorInfo::Break => order.push(StructuralInst::Break),
            TerminatorInfo::FallThrough(next) => self.analyze_block(next, order),
            TerminatorInfo::NormalInst(inst) => order.push(StructuralInst::Inst(inst)),
        }
    }

    fn enter_loop_scope(&mut self, header: BasicBlockId, exit: Option<BasicBlockId>) {
        let kind = ScopeKind::Loop { header, exit };
        let current_scope = std::mem::take(&mut self.scope);
        self.scope = Some(Scope {
            kind,
            parent: current_scope.map(Into::into),
        });
    }

    fn enter_if_scope(&mut self, merge_block: Option<BasicBlockId>) {
        let kind = ScopeKind::If { merge_block };
        let current_scope = std::mem::take(&mut self.scope);
        self.scope = Some(Scope {
            kind,
            parent: current_scope.map(Into::into),
        });
    }

    fn exit_scope(&mut self) {
        let current_scope = std::mem::take(&mut self.scope);
        self.scope = current_scope.unwrap().parent.map(|parent| *parent);
    }

    // NOTE: We assume loop has at most one canonical loop exit.
    fn find_loop_exit(&self, lp: LoopId) -> Option<BasicBlockId> {
        let mut exit_candidates = vec![];
        for block_in_loop in self.loop_tree.iter_blocks_post_order(&self.cfg, lp) {
            for &succ in self.cfg.succs(block_in_loop) {
                if !self.loop_tree.is_block_in_loop(succ, lp) {
                    exit_candidates.push(succ);
                }
            }
        }

        if exit_candidates.is_empty() {
            return None;
        }

        for &cand in &exit_candidates {
            // `cand` is true loop exit if the `cand` is contained in the dominance frontier
            // of all other candidates. and yeset foo
            if exit_candidates.iter().all(|&block| {
                if block == cand {
                    true
                } else if let Some(mut df) = self.df.frontiers(block) {
                    df.any(|frontier| frontier == cand)
                } else {
                    true
                }
            }) {
                return Some(cand);
            }
        }

        None
    }

    fn analyze_terminator(&self, inst: InstId) -> TerminatorInfo {
        debug_assert!(self.body.store.is_terminator(inst));

        match self.body.store.branch_info(inst) {
            BranchInfo::Jump(dest) => self.analyze_jump(dest),
            BranchInfo::Branch(cond, then, else_) => {
                self.analyze_branch(self.body.order.inst_block(inst), cond, then, else_)
            }
            BranchInfo::NotBranch => TerminatorInfo::NormalInst(inst),
        }
    }

    // NOTE: We remove critical edges in legalization pass, so `break` and
    // `continue` never appear in branch info.
    fn analyze_branch(
        &self,
        block: BasicBlockId,
        cond: ValueId,
        then: BasicBlockId,
        else_: BasicBlockId,
    ) -> TerminatorInfo {
        let merge_block = match self.pd_tree.post_idom(block) {
            PostIDom::DummyEntry | PostIDom::DummyExit => None,
            PostIDom::Block(block) => Some(block),
        };

        TerminatorInfo::If {
            cond,
            then,
            else_,
            merge_block,
        }
    }

    fn analyze_jump(&self, dest: BasicBlockId) -> TerminatorInfo {
        match &self.scope {
            Some(scope) => {
                if Some(dest) == scope.loop_header_recursive() {
                    TerminatorInfo::Continue
                } else if Some(dest) == scope.loop_exit_recursive() {
                    TerminatorInfo::Break
                } else if Some(dest) == scope.if_merge_block() {
                    TerminatorInfo::ToMergeBlock
                } else {
                    TerminatorInfo::FallThrough(dest)
                }
            }

            None => TerminatorInfo::FallThrough(dest),
        }
    }
}

struct Scope {
    kind: ScopeKind,
    parent: Option<Box<Scope>>,
}

#[derive(Debug, Clone, Copy)]
enum ScopeKind {
    Loop {
        header: BasicBlockId,
        exit: Option<BasicBlockId>,
    },
    If {
        merge_block: Option<BasicBlockId>,
    },
}

impl Scope {
    fn loop_header(&self) -> Option<BasicBlockId> {
        match self.kind {
            ScopeKind::Loop { header, .. } => Some(header),
            _ => None,
        }
    }
    fn loop_header_recursive(&self) -> Option<BasicBlockId> {
        match self.kind {
            ScopeKind::Loop { header, .. } => Some(header),
            _ => self.parent.as_ref()?.loop_header_recursive(),
        }
    }

    fn loop_exit_recursive(&self) -> Option<BasicBlockId> {
        match self.kind {
            ScopeKind::Loop { exit, .. } => exit,
            _ => self.parent.as_ref()?.loop_exit_recursive(),
        }
    }

    fn if_merge_block(&self) -> Option<BasicBlockId> {
        match self.kind {
            ScopeKind::If { merge_block } => merge_block,
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum TerminatorInfo {
    If {
        cond: ValueId,
        then: BasicBlockId,
        else_: BasicBlockId,
        merge_block: Option<BasicBlockId>,
    },
    ToMergeBlock,
    Continue,
    Break,
    FallThrough(BasicBlockId),
    NormalInst(InstId),
}
