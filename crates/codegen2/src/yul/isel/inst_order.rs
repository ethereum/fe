use fe_mir::{
    analysis::{
        domtree::DFSet, loop_tree::LoopId, post_domtree::PostIDom, ControlFlowGraph, DomTree,
        LoopTree, PostDomTree,
    },
    ir::{
        inst::{BranchInfo, SwitchTable},
        BasicBlockId, FunctionBody, InstId, ValueId,
    },
};
use indexmap::{IndexMap, IndexSet};

#[derive(Debug, Clone)]
pub(super) enum StructuralInst {
    Inst(InstId),
    If {
        cond: ValueId,
        then: Vec<StructuralInst>,
        else_: Vec<StructuralInst>,
    },

    Switch {
        scrutinee: ValueId,
        table: Vec<(ValueId, Vec<StructuralInst>)>,
        default: Option<Vec<StructuralInst>>,
    },

    For {
        body: Vec<StructuralInst>,
    },

    Break,

    Continue,
}

pub(super) struct InstSerializer<'a> {
    body: &'a FunctionBody,
    cfg: ControlFlowGraph,
    loop_tree: LoopTree,
    df: DFSet,
    domtree: DomTree,
    pd_tree: PostDomTree,
    scope: Option<Scope>,
}

impl<'a> InstSerializer<'a> {
    pub(super) fn new(body: &'a FunctionBody) -> Self {
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
            domtree,
            pd_tree,
            scope: None,
        }
    }

    pub(super) fn serialize(&mut self) -> Vec<StructuralInst> {
        self.scope = None;
        let entry = self.cfg.entry();
        let mut order = vec![];
        self.serialize_block(entry, &mut order);
        order
    }

    fn serialize_block(&mut self, block: BasicBlockId, order: &mut Vec<StructuralInst>) {
        match self.loop_tree.loop_of_block(block) {
            Some(lp)
                if block == self.loop_tree.loop_header(lp)
                    && Some(block) != self.scope.as_ref().and_then(Scope::loop_header) =>
            {
                let loop_exit = self.find_loop_exit(lp);
                self.enter_loop_scope(lp, block, loop_exit);
                let mut body = vec![];
                self.serialize_block(block, &mut body);
                self.exit_scope();
                order.push(StructuralInst::For { body });

                match loop_exit {
                    Some(exit)
                        if self
                            .scope
                            .as_ref()
                            .map(|scope| scope.branch_merge_block() != Some(exit))
                            .unwrap_or(true) =>
                    {
                        self.serialize_block(exit, order);
                    }
                    _ => {}
                }

                return;
            }
            _ => {}
        };

        for inst in self.body.order.iter_inst(block) {
            if self.body.store.is_terminator(inst) {
                break;
            }
            if !self.body.store.is_nop(inst) {
                order.push(StructuralInst::Inst(inst));
            }
        }

        let terminator = self.body.order.terminator(&self.body.store, block).unwrap();
        match self.analyze_terminator(terminator) {
            TerminatorInfo::If {
                cond,
                then,
                else_,
                merge_block,
            } => self.serialize_if_terminator(cond, *then, *else_, merge_block, order),

            TerminatorInfo::Switch {
                scrutinee,
                table,
                default,
                merge_block,
            } => self.serialize_switch_terminator(
                scrutinee,
                table,
                default.map(|value| *value),
                merge_block,
                order,
            ),

            TerminatorInfo::ToMergeBlock => {}
            TerminatorInfo::Continue => order.push(StructuralInst::Continue),
            TerminatorInfo::Break => order.push(StructuralInst::Break),
            TerminatorInfo::FallThrough(next) => self.serialize_block(next, order),
            TerminatorInfo::NormalInst(inst) => order.push(StructuralInst::Inst(inst)),
        }
    }

    fn serialize_if_terminator(
        &mut self,
        cond: ValueId,
        then: TerminatorInfo,
        else_: TerminatorInfo,
        merge_block: Option<BasicBlockId>,
        order: &mut Vec<StructuralInst>,
    ) {
        let mut then_body = vec![];
        let mut else_body = vec![];

        self.enter_branch_scope(merge_block);
        self.serialize_branch_dest(then, &mut then_body, merge_block);
        self.serialize_branch_dest(else_, &mut else_body, merge_block);
        self.exit_scope();

        order.push(StructuralInst::If {
            cond,
            then: then_body,
            else_: else_body,
        });
        if let Some(merge_block) = merge_block {
            self.serialize_block(merge_block, order);
        }
    }

    fn serialize_switch_terminator(
        &mut self,
        scrutinee: ValueId,
        table: Vec<(ValueId, TerminatorInfo)>,
        default: Option<TerminatorInfo>,
        merge_block: Option<BasicBlockId>,
        order: &mut Vec<StructuralInst>,
    ) {
        self.enter_branch_scope(merge_block);

        let mut serialized_table = Vec::with_capacity(table.len());
        for (value, dest) in table {
            let mut body = vec![];
            self.serialize_branch_dest(dest, &mut body, merge_block);
            serialized_table.push((value, body));
        }

        let serialized_default = default.map(|dest| {
            let mut body = vec![];
            self.serialize_branch_dest(dest, &mut body, merge_block);
            body
        });

        order.push(StructuralInst::Switch {
            scrutinee,
            table: serialized_table,
            default: serialized_default,
        });

        self.exit_scope();

        if let Some(merge_block) = merge_block {
            self.serialize_block(merge_block, order);
        }
    }

    fn serialize_branch_dest(
        &mut self,
        dest: TerminatorInfo,
        body: &mut Vec<StructuralInst>,
        merge_block: Option<BasicBlockId>,
    ) {
        match dest {
            TerminatorInfo::Break => body.push(StructuralInst::Break),
            TerminatorInfo::Continue => body.push(StructuralInst::Continue),
            TerminatorInfo::ToMergeBlock => {}
            TerminatorInfo::FallThrough(dest) => {
                if Some(dest) != merge_block {
                    self.serialize_block(dest, body);
                }
            }
            _ => unreachable!(),
        };
    }

    fn enter_loop_scope(&mut self, lp: LoopId, header: BasicBlockId, exit: Option<BasicBlockId>) {
        let kind = ScopeKind::Loop { lp, header, exit };
        let current_scope = std::mem::take(&mut self.scope);
        self.scope = Some(Scope {
            kind,
            parent: current_scope.map(Into::into),
        });
    }

    fn enter_branch_scope(&mut self, merge_block: Option<BasicBlockId>) {
        let kind = ScopeKind::Branch { merge_block };
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

        if exit_candidates.len() == 1 {
            let candidate = exit_candidates[0];
            let exit = if let Some(mut df) = self.df.frontiers(candidate) {
                debug_assert_eq!(self.df.frontier_num(candidate), 1);
                df.next()
            } else {
                Some(candidate)
            };
            return exit;
        }

        // If a candidate is a dominance frontier of all other nodes, then the candidate
        // is a loop exit.
        for &cand in &exit_candidates {
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

        // If all candidates have the same dominance frontier, then the frontier block
        // is the canonicalized loop exit.
        let mut frontier: IndexSet<_> = self
            .df
            .frontiers(exit_candidates.pop().unwrap())
            .map(std::iter::Iterator::collect)
            .unwrap_or_default();
        for cand in exit_candidates {
            for cand_frontier in self.df.frontiers(cand).unwrap() {
                if !frontier.contains(&cand_frontier) {
                    frontier.remove(&cand_frontier);
                }
            }
        }
        debug_assert!(frontier.len() < 2);
        frontier.iter().next().copied()
    }

    fn analyze_terminator(&self, inst: InstId) -> TerminatorInfo {
        debug_assert!(self.body.store.is_terminator(inst));

        let inst_block = self.body.order.inst_block(inst);
        match self.body.store.branch_info(inst) {
            BranchInfo::Jump(dest) => self.analyze_jump(dest),

            BranchInfo::Branch(cond, then, else_) => self.analyze_if(inst_block, cond, then, else_),

            BranchInfo::Switch(scrutinee, table, default) => {
                self.analyze_switch(inst_block, scrutinee, table, default)
            }

            BranchInfo::NotBranch => TerminatorInfo::NormalInst(inst),
        }
    }

    fn analyze_if(
        &self,
        block: BasicBlockId,
        cond: ValueId,
        then_bb: BasicBlockId,
        else_bb: BasicBlockId,
    ) -> TerminatorInfo {
        let then = Box::new(self.analyze_dest(then_bb));
        let else_ = Box::new(self.analyze_dest(else_bb));

        let then_cands = self.find_merge_block_candidates(block, then_bb);
        let else_cands = self.find_merge_block_candidates(block, else_bb);
        debug_assert!(then_cands.len() < 2);
        debug_assert!(else_cands.len() < 2);

        let merge_block = match (then_cands.as_slice(), else_cands.as_slice()) {
            (&[then_cand], &[else_cand]) => {
                if then_cand == else_cand {
                    Some(then_cand)
                } else {
                    None
                }
            }

            (&[cand], []) => {
                if cand == else_bb {
                    Some(cand)
                } else {
                    None
                }
            }

            ([], &[cand]) => {
                if cand == then_bb {
                    Some(cand)
                } else {
                    None
                }
            }

            ([], []) => match self.pd_tree.post_idom(block) {
                PostIDom::Block(block) => {
                    if let Some(lp) = self.scope.as_ref().and_then(Scope::loop_recursive) {
                        if self.loop_tree.is_block_in_loop(block, lp) {
                            Some(block)
                        } else {
                            None
                        }
                    } else {
                        Some(block)
                    }
                }
                _ => None,
            },

            (_, _) => unreachable!(),
        };

        TerminatorInfo::If {
            cond,
            then,
            else_,
            merge_block,
        }
    }

    fn analyze_switch(
        &self,
        block: BasicBlockId,
        scrutinee: ValueId,
        table: &SwitchTable,
        default: Option<BasicBlockId>,
    ) -> TerminatorInfo {
        let mut analyzed_table = Vec::with_capacity(table.len());

        let mut merge_block_cands = IndexSet::default();
        for (value, dest) in table.iter() {
            analyzed_table.push((value, self.analyze_dest(dest)));
            merge_block_cands.extend(self.find_merge_block_candidates(block, dest));
        }

        let analyzed_default = default.map(|dest| {
            merge_block_cands.extend(self.find_merge_block_candidates(block, dest));
            Box::new(self.analyze_dest(dest))
        });

        TerminatorInfo::Switch {
            scrutinee,
            table: analyzed_table,
            default: analyzed_default,
            merge_block: self.select_switch_merge_block(
                &merge_block_cands,
                table.iter().map(|(_, d)| d).chain(default),
            ),
        }
    }

    fn find_merge_block_candidates(
        &self,
        branch_inst_bb: BasicBlockId,
        branch_dest_bb: BasicBlockId,
    ) -> Vec<BasicBlockId> {
        if self.domtree.dominates(branch_dest_bb, branch_inst_bb) {
            return vec![];
        }

        // a block `cand` can be a candidate of a `merge` block iff
        // 1. `cand` is a dominance frontier of `branch_dest_bb`.
        // 2. `cand` is NOT a dominator of `branch_dest_bb`.
        // 3. `cand` is NOT a "merge" block of parent `if` or `switch`.
        // 4. `cand` is NOT a "loop_exit" block of parent `loop`.
        match self.df.frontiers(branch_dest_bb) {
            Some(cands) => cands
                .filter(|cand| {
                    !self.domtree.dominates(*cand, branch_dest_bb)
                        && Some(*cand)
                            != self
                                .scope
                                .as_ref()
                                .and_then(Scope::branch_merge_block_recursive)
                        && Some(*cand) != self.scope.as_ref().and_then(Scope::loop_exit_recursive)
                })
                .collect(),
            None => vec![],
        }
    }

    /// Each destination block of `switch` instruction could have multiple
    /// candidates for the merge block because arm bodies can have multiple
    /// predecessors, e.g., `default` arm.
    /// So we need a heuristic to select the merge block from candidates.
    ///
    /// First, if one of the dominance frontiers of switch dests is a parent
    /// merge block, then we stop searching the merge block because the parent
    /// merge block should be the subsequent codes after the switch in terms of
    /// high-level flow structure like Fe or yul.
    ///
    /// If no parent merge block is found, we start scoring the candidates by
    /// the following function.
    ///
    /// The scoring function `F` is defined as follows:
    /// 1. The initial score of each candidate('cand_bb`) is number of
    /// predecessors of the candidate.
    ///
    /// 2. Find the `top_cand` of each `cand_bb`. `top_cand` can be found by
    /// [`Self::try_find_top_cand`] method, see the method for details.
    ///
    /// 3. If `top_cand` is found, then add the `cand_bb` score to the
    /// `top_cand` score, then set 0 to the `cand_bb` score.
    ///
    /// After the scoring, the candidates with the highest score will be
    /// selected.
    fn select_switch_merge_block(
        &self,
        cands: &IndexSet<BasicBlockId>,
        dests: impl Iterator<Item = BasicBlockId>,
    ) -> Option<BasicBlockId> {
        let parent_merge = self
            .scope
            .as_ref()
            .and_then(Scope::branch_merge_block_recursive);
        for dest in dests {
            if self
                .df
                .frontiers(dest)
                .map(|mut frontieres| frontieres.any(|frontier| Some(frontier) == parent_merge))
                .unwrap_or_default()
            {
                return None;
            }
        }

        let mut cands_with_score = cands
            .iter()
            .map(|cand| (*cand, self.cfg.preds(*cand).len()))
            .collect::<IndexMap<_, _>>();

        for cand_bb in cands_with_score.keys().copied().collect::<Vec<_>>() {
            if let Some(top_cand) = self.try_find_top_cand(&cands_with_score, cand_bb) {
                let score = std::mem::take(cands_with_score.get_mut(&cand_bb).unwrap());
                *cands_with_score.get_mut(&top_cand).unwrap() += score;
            }
        }

        cands_with_score
            .iter()
            .max_by_key(|(_, score)| *score)
            .map(|(&cand, _)| cand)
    }

    /// Try to find the `top_cand` of the `cand_bb`.
    /// A `top_cand` can be found by the following rules:
    ///
    /// 1. Find the block which is contained in DF of `cand_bb` and in
    /// `cands_with_score`.
    ///
    /// 2. If a block is found in 1., and the score of the block is positive,
    /// then the block is `top_cand`.
    ///
    /// 2'. If a block is found in 1., and the score of the block is 0, then the
    /// `top_cand` of the block is `top_cand` of `cand_bb`.
    ///
    /// 2''. If a block is NOT found in 1., then there is no `top_cand` for
    /// `cand_bb`.
    fn try_find_top_cand(
        &self,
        cands_with_score: &IndexMap<BasicBlockId, usize>,
        cand_bb: BasicBlockId,
    ) -> Option<BasicBlockId> {
        let mut frontiers = match self.df.frontiers(cand_bb) {
            Some(frontiers) => frontiers,
            _ => return None,
        };

        while let Some(frontier_bb) = frontiers.next() {
            if cands_with_score.contains_key(&frontier_bb) {
                debug_assert!(frontiers.all(|bb| !cands_with_score.contains_key(&bb)));
                if cands_with_score[&frontier_bb] != 0 {
                    return Some(frontier_bb);
                } else {
                    return self.try_find_top_cand(cands_with_score, frontier_bb);
                }
            }
        }

        None
    }

    fn analyze_jump(&self, dest: BasicBlockId) -> TerminatorInfo {
        self.analyze_dest(dest)
    }

    fn analyze_dest(&self, dest: BasicBlockId) -> TerminatorInfo {
        match &self.scope {
            Some(scope) => {
                if Some(dest) == scope.loop_header_recursive() {
                    TerminatorInfo::Continue
                } else if Some(dest) == scope.loop_exit_recursive() {
                    TerminatorInfo::Break
                } else if Some(dest) == scope.branch_merge_block_recursive() {
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
        lp: LoopId,
        header: BasicBlockId,
        exit: Option<BasicBlockId>,
    },
    Branch {
        merge_block: Option<BasicBlockId>,
    },
}

impl Scope {
    fn loop_recursive(&self) -> Option<LoopId> {
        match self.kind {
            ScopeKind::Loop { lp, .. } => Some(lp),
            _ => self.parent.as_ref()?.loop_recursive(),
        }
    }

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

    fn branch_merge_block(&self) -> Option<BasicBlockId> {
        match self.kind {
            ScopeKind::Branch { merge_block } => merge_block,
            _ => None,
        }
    }

    fn branch_merge_block_recursive(&self) -> Option<BasicBlockId> {
        match self.kind {
            ScopeKind::Branch {
                merge_block: Some(merge_block),
            } => Some(merge_block),
            _ => self.parent.as_ref()?.branch_merge_block_recursive(),
        }
    }
}

#[derive(Debug, Clone)]
enum TerminatorInfo {
    If {
        cond: ValueId,
        then: Box<TerminatorInfo>,
        else_: Box<TerminatorInfo>,
        merge_block: Option<BasicBlockId>,
    },

    Switch {
        scrutinee: ValueId,
        table: Vec<(ValueId, TerminatorInfo)>,
        default: Option<Box<TerminatorInfo>>,
        merge_block: Option<BasicBlockId>,
    },

    ToMergeBlock,
    Continue,
    Break,
    FallThrough(BasicBlockId),
    NormalInst(InstId),
}

#[cfg(test)]
mod tests {
    use fe_mir::ir::{body_builder::BodyBuilder, inst::InstKind, FunctionId, SourceInfo, TypeId};

    use super::*;

    fn body_builder() -> BodyBuilder {
        BodyBuilder::new(FunctionId(0), SourceInfo::dummy())
    }

    fn serialize_func_body(func: &mut FunctionBody) -> impl Iterator<Item = StructuralInst> {
        InstSerializer::new(func).serialize().into_iter()
    }

    fn expect_if(
        insts: &mut impl Iterator<Item = StructuralInst>,
    ) -> (
        impl Iterator<Item = StructuralInst>,
        impl Iterator<Item = StructuralInst>,
    ) {
        match insts.next().unwrap() {
            StructuralInst::If { then, else_, .. } => (then.into_iter(), else_.into_iter()),
            _ => panic!("expect if inst"),
        }
    }

    fn expect_switch(
        insts: &mut impl Iterator<Item = StructuralInst>,
    ) -> Vec<impl Iterator<Item = StructuralInst>> {
        match insts.next().unwrap() {
            StructuralInst::Switch { table, default, .. } => {
                let mut arms: Vec<_> = table
                    .into_iter()
                    .map(|(_, insts)| insts.into_iter())
                    .collect();
                if let Some(default) = default {
                    arms.push(default.into_iter());
                }

                arms
            }

            _ => panic!("expect if inst"),
        }
    }

    fn expect_for(
        insts: &mut impl Iterator<Item = StructuralInst>,
    ) -> impl Iterator<Item = StructuralInst> {
        match insts.next().unwrap() {
            StructuralInst::For { body } => body.into_iter(),
            _ => panic!("expect if inst"),
        }
    }

    fn expect_break(insts: &mut impl Iterator<Item = StructuralInst>) {
        assert!(matches!(insts.next().unwrap(), StructuralInst::Break))
    }

    fn expect_continue(insts: &mut impl Iterator<Item = StructuralInst>) {
        assert!(matches!(insts.next().unwrap(), StructuralInst::Continue))
    }

    fn expect_return(func: &FunctionBody, insts: &mut impl Iterator<Item = StructuralInst>) {
        let inst = insts.next().unwrap();
        match inst {
            StructuralInst::Inst(inst) => {
                assert!(matches!(
                    func.store.inst_data(inst).kind,
                    InstKind::Return { .. }
                ))
            }
            _ => panic!("expect return"),
        }
    }

    fn expect_end(insts: &mut impl Iterator<Item = StructuralInst>) {
        assert!(insts.next().is_none())
    }

    #[test]
    fn if_non_merge() {
        // +------+     +-------+
        // | then | <-- |  bb0  |
        // +------+     +-------+
        //                |
        //                |
        //                v
        //              +-------+
        //              | else_ |
        //              +-------+
        let mut builder = body_builder();

        let then = builder.make_block();
        let else_ = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        let unit = builder.make_unit(dummy_ty);

        builder.branch(v0, then, else_, SourceInfo::dummy());

        builder.move_to_block(then);
        builder.ret(unit, SourceInfo::dummy());

        builder.move_to_block(else_);
        builder.ret(unit, SourceInfo::dummy());

        let mut func = builder.build();
        let mut order = serialize_func_body(&mut func);

        let (mut then, mut else_) = expect_if(&mut order);
        expect_return(&func, &mut then);
        expect_end(&mut then);
        expect_return(&func, &mut else_);
        expect_end(&mut else_);

        expect_end(&mut order);
    }

    #[test]
    fn if_merge() {
        // +------+     +-------+
        // | then | <-- |  bb0  |
        // +------+     +-------+
        //   |            |
        //   |            |
        //   |            v
        //   |          +-------+
        //   |          | else_ |
        //   |          +-------+
        //   |            |
        //   |            |
        //   |            v
        //   |          +-------+
        //   +--------> | merge |
        //              +-------+
        let mut builder = body_builder();

        let then = builder.make_block();
        let else_ = builder.make_block();
        let merge = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        let unit = builder.make_unit(dummy_ty);

        builder.branch(v0, then, else_, SourceInfo::dummy());

        builder.move_to_block(then);
        builder.jump(merge, SourceInfo::dummy());

        builder.move_to_block(else_);
        builder.jump(merge, SourceInfo::dummy());

        builder.move_to_block(merge);
        builder.ret(unit, SourceInfo::dummy());

        let mut func = builder.build();
        let mut order = serialize_func_body(&mut func);

        let (mut then, mut else_) = expect_if(&mut order);
        expect_end(&mut then);
        expect_end(&mut else_);

        expect_return(&func, &mut order);
        expect_end(&mut order);
    }

    #[test]
    fn nested_if() {
        //             +-----+
        //             | bb0 | -+
        //             +-----+  |
        //               |      |
        //               |      |
        //               v      |
        // +-----+     +-----+  |
        // | bb3 | <-- | bb1 |  |
        // +-----+     +-----+  |
        //               |      |
        //               |      |
        //               v      |
        //             +-----+  |
        //             | bb4 |  |
        //             +-----+  |
        //               |      |
        //               |      |
        //               v      |
        //             +-----+  |
        //             | bb2 | <+
        //             +-----+
        let mut builder = body_builder();

        let bb1 = builder.make_block();
        let bb2 = builder.make_block();
        let bb3 = builder.make_block();
        let bb4 = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        let unit = builder.make_unit(dummy_ty);

        builder.branch(v0, bb1, bb2, SourceInfo::dummy());

        builder.move_to_block(bb1);
        builder.branch(v0, bb3, bb4, SourceInfo::dummy());

        builder.move_to_block(bb3);
        builder.ret(unit, SourceInfo::dummy());

        builder.move_to_block(bb4);
        builder.jump(bb2, SourceInfo::dummy());

        builder.move_to_block(bb2);
        builder.ret(unit, SourceInfo::dummy());

        let mut func = builder.build();
        let mut order = serialize_func_body(&mut func);

        let (mut then1, mut else2) = expect_if(&mut order);
        expect_end(&mut else2);

        let (mut then3, mut else4) = expect_if(&mut then1);
        expect_end(&mut then1);
        expect_return(&func, &mut then3);
        expect_end(&mut then3);
        expect_end(&mut else4);

        expect_return(&func, &mut order);
        expect_end(&mut order);
    }

    #[test]
    fn simple_loop() {
        //    +--------+
        //    |  bb0   | -+
        //    +--------+  |
        //      |         |
        //      |         |
        //      v         |
        //    +--------+  |
        // +> | header |  |
        // |  +--------+  |
        // |    |         |
        // |    |         |
        // |    v         |
        // |  +--------+  |
        // +- | latch  |  |
        //    +--------+  |
        //      |         |
        //      |         |
        //      v         |
        //    +--------+  |
        //    |  exit  | <+
        //    +--------+
        let mut builder = body_builder();

        let header = builder.make_block();
        let latch = builder.make_block();
        let exit = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        let unit = builder.make_unit(dummy_ty);

        builder.branch(v0, header, exit, SourceInfo::dummy());

        builder.move_to_block(header);
        builder.jump(latch, SourceInfo::dummy());

        builder.move_to_block(latch);
        builder.branch(v0, header, exit, SourceInfo::dummy());

        builder.move_to_block(exit);
        builder.ret(unit, SourceInfo::dummy());

        let mut func = builder.build();
        let mut order = serialize_func_body(&mut func);

        let (mut lp, mut empty) = expect_if(&mut order);

        let mut body = expect_for(&mut lp);
        let (mut continue_, mut break_) = expect_if(&mut body);
        expect_end(&mut body);

        expect_continue(&mut continue_);
        expect_end(&mut continue_);

        expect_break(&mut break_);
        expect_end(&mut break_);

        expect_end(&mut empty);

        expect_return(&func, &mut order);
        expect_end(&mut order);
    }

    #[test]
    fn loop_with_continue() {
        //    +-----+
        // +- | bb0 |
        // |  +-----+
        // |    |
        // |    |
        // |    v
        // |  +---------------+     +-----+
        // |  |      bb1      | --> | bb3 |
        // |  +---------------+     +-----+
        // |    |      ^    ^         |
        // |    |      |    +---------+
        // |    v      |
        // |  +-----+  |
        // |  | bb4 | -+
        // |  +-----+
        // |    |
        // |    |
        // |    v
        // |  +-----+
        // +> | bb2 |
        //    +-----+
        let mut builder = body_builder();

        let bb1 = builder.make_block();
        let bb2 = builder.make_block();
        let bb3 = builder.make_block();
        let bb4 = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        let unit = builder.make_unit(dummy_ty);

        builder.branch(v0, bb1, bb2, SourceInfo::dummy());

        builder.move_to_block(bb1);
        builder.branch(v0, bb3, bb4, SourceInfo::dummy());

        builder.move_to_block(bb3);
        builder.jump(bb1, SourceInfo::dummy());

        builder.move_to_block(bb4);
        builder.branch(v0, bb1, bb2, SourceInfo::dummy());

        builder.move_to_block(bb2);
        builder.ret(unit, SourceInfo::dummy());

        let mut func = builder.build();
        let mut order = serialize_func_body(&mut func);

        let (mut lp, mut empty) = expect_if(&mut order);
        expect_end(&mut empty);

        let mut body = expect_for(&mut lp);

        let (mut continue_, mut empty) = expect_if(&mut body);
        expect_continue(&mut continue_);
        expect_end(&mut continue_);
        expect_end(&mut empty);

        let (mut continue_, mut break_) = expect_if(&mut body);
        expect_continue(&mut continue_);
        expect_end(&mut continue_);
        expect_break(&mut break_);
        expect_end(&mut break_);

        expect_end(&mut body);
        expect_end(&mut lp);

        expect_return(&func, &mut order);
        expect_end(&mut order);
    }

    #[test]
    fn loop_with_break() {
        //    +-----+
        // +- | bb0 |
        // |  +-----+
        // |    |
        // |    |           +---------+
        // |    v           v         |
        // |  +---------------+     +-----+
        // |  |      bb1      | --> | bb4 |
        // |  +---------------+     +-----+
        // |    |                     |
        // |    |                     |
        // |    v                     |
        // |  +-----+                 |
        // |  | bb3 |                 |
        // |  +-----+                 |
        // |    |                     |
        // |    |                     |
        // |    v                     |
        // |  +-----+                 |
        // +> | bb2 | <---------------+
        //    +-----+
        let mut builder = body_builder();

        let bb1 = builder.make_block();
        let bb2 = builder.make_block();
        let bb3 = builder.make_block();
        let bb4 = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        let unit = builder.make_unit(dummy_ty);

        builder.branch(v0, bb1, bb2, SourceInfo::dummy());

        builder.move_to_block(bb1);
        builder.branch(v0, bb3, bb4, SourceInfo::dummy());

        builder.move_to_block(bb3);
        builder.jump(bb2, SourceInfo::dummy());

        builder.move_to_block(bb4);
        builder.branch(v0, bb1, bb2, SourceInfo::dummy());

        builder.move_to_block(bb2);
        builder.ret(unit, SourceInfo::dummy());

        let mut func = builder.build();
        let mut order = serialize_func_body(&mut func);

        let (mut lp, mut empty) = expect_if(&mut order);
        expect_end(&mut empty);

        let mut body = expect_for(&mut lp);

        let (mut break_, mut latch) = expect_if(&mut body);
        expect_break(&mut break_);
        expect_end(&mut break_);

        let (mut continue_, mut break_) = expect_if(&mut latch);
        expect_end(&mut latch);
        expect_continue(&mut continue_);
        expect_end(&mut continue_);
        expect_break(&mut break_);
        expect_end(&mut break_);

        expect_end(&mut body);
        expect_end(&mut lp);

        expect_return(&func, &mut order);
        expect_end(&mut order);
    }

    #[test]
    fn loop_no_guard() {
        // +-----+
        // | bb0 |
        // +-----+
        //   |
        //   |
        //   v
        // +-----+
        // | bb1 | <+
        // +-----+  |
        //   |      |
        //   |      |
        //   v      |
        // +-----+  |
        // | bb2 | -+
        // +-----+
        //   |
        //   |
        //   v
        // +-----+
        // | bb3 |
        // +-----+
        let mut builder = body_builder();

        let bb1 = builder.make_block();
        let bb2 = builder.make_block();
        let bb3 = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(true, dummy_ty);
        let unit = builder.make_unit(dummy_ty);

        builder.jump(bb1, SourceInfo::dummy());

        builder.move_to_block(bb1);
        builder.jump(bb2, SourceInfo::dummy());

        builder.move_to_block(bb2);
        builder.branch(v0, bb1, bb3, SourceInfo::dummy());

        builder.move_to_block(bb3);
        builder.ret(unit, SourceInfo::dummy());

        let mut func = builder.build();
        let mut order = serialize_func_body(&mut func);

        let mut body = expect_for(&mut order);
        let (mut continue_, mut break_) = expect_if(&mut body);
        expect_end(&mut body);

        expect_continue(&mut continue_);
        expect_end(&mut continue_);

        expect_break(&mut break_);
        expect_end(&mut break_);

        expect_return(&func, &mut order);
        expect_end(&mut order);
    }

    #[test]
    fn infinite_loop() {
        // +-----+
        // | bb0 |
        // +-----+
        //   |
        //   |
        //   v
        // +-----+
        // | bb1 | <+
        // +-----+  |
        //   |      |
        //   |      |
        //   v      |
        // +-----+  |
        // | bb2 | -+
        // +-----+
        let mut builder = body_builder();

        let bb1 = builder.make_block();
        let bb2 = builder.make_block();

        builder.jump(bb1, SourceInfo::dummy());

        builder.move_to_block(bb1);
        builder.jump(bb2, SourceInfo::dummy());

        builder.move_to_block(bb2);
        builder.jump(bb1, SourceInfo::dummy());

        let mut func = builder.build();
        let mut order = serialize_func_body(&mut func);

        let mut body = expect_for(&mut order);
        expect_continue(&mut body);
        expect_end(&mut body);

        expect_end(&mut order);
    }

    #[test]
    fn switch_basic() {
        // +-----+     +-------+     +-----+
        // | bb2 | <-- |  bb0  | --> | bb3 |
        // +-----+     +-------+     +-----+
        //   |           |             |
        //   |           |             |
        //   |           v             |
        //   |         +-------+       |
        //   |         |  bb1  |       |
        //   |         +-------+       |
        //   |           |             |
        //   |           |             |
        //   |           v             |
        //   |         +-------+       |
        //   +-------> | merge | <-----+
        //             +-------+
        let mut builder = body_builder();
        let dummy_ty = TypeId(0);
        let dummy_value = builder.make_unit(dummy_ty);

        let bb1 = builder.make_block();
        let bb2 = builder.make_block();
        let bb3 = builder.make_block();
        let merge = builder.make_block();

        let mut table = SwitchTable::default();
        table.add_arm(dummy_value, bb1);
        table.add_arm(dummy_value, bb2);
        table.add_arm(dummy_value, bb3);
        builder.switch(dummy_value, table, None, SourceInfo::dummy());

        builder.move_to_block(bb1);
        builder.jump(merge, SourceInfo::dummy());

        builder.move_to_block(bb2);
        builder.jump(merge, SourceInfo::dummy());
        builder.move_to_block(bb3);
        builder.jump(merge, SourceInfo::dummy());

        builder.move_to_block(merge);
        builder.ret(dummy_value, SourceInfo::dummy());

        let mut func = builder.build();
        let mut order = serialize_func_body(&mut func);

        let arms = expect_switch(&mut order);
        assert_eq!(arms.len(), 3);
        for mut arm in arms {
            expect_end(&mut arm);
        }

        expect_return(&func, &mut order);
        expect_end(&mut order);
    }

    #[test]
    fn switch_default() {
        //      +-----------+
        //      |           |
        //      |           |
        //      |      +----+--------+
        //      v      |    |        |
        //    +-----+  |  +-------+  |  +---------+
        //    | bb2 | -+  |  bb0  | -+> |   bb3   |
        //    +-----+     +-------+  |  +---------+
        //      |           |        |    |
        //      |           |        |    |
        //      v           v        |    v
        //    +-----+     +-------+  |  +---------+
        //    | bb5 |  +- |  bb1  |  +> | default | <+
        //    +-----+  |  +-------+     +---------+  |
        //      |      |    |             |          |
        //      |      |    |             |          |
        //      |      |    v             |          |
        //      |      |  +-------+       |          |
        //      |      |  |  bb4  |       |          |
        //      |      |  +-------+       |          |
        //      |      |    |             |          |
        // +----+------+    |             |          |
        // |    |           v             |          |
        // |    |         +-------+       |          |
        // |    +-------> | merge | <-----+          |
        // |              +-------+                  |
        // |                                         |
        // +-----------------------------------------+
        let mut builder = body_builder();
        let dummy_ty = TypeId(0);
        let dummy_value = builder.make_unit(dummy_ty);

        let bb1 = builder.make_block();
        let bb2 = builder.make_block();
        let bb3 = builder.make_block();
        let bb4 = builder.make_block();
        let bb5 = builder.make_block();
        let default = builder.make_block();
        let merge = builder.make_block();

        let mut table = SwitchTable::default();
        table.add_arm(dummy_value, bb1);
        table.add_arm(dummy_value, bb2);
        table.add_arm(dummy_value, bb3);
        builder.switch(dummy_value, table, None, SourceInfo::dummy());

        builder.move_to_block(bb1);
        let mut table = SwitchTable::default();
        table.add_arm(dummy_value, bb4);
        table.add_arm(dummy_value, default);
        builder.switch(dummy_value, table, None, SourceInfo::dummy());

        builder.move_to_block(bb2);
        let mut table = SwitchTable::default();
        table.add_arm(dummy_value, bb5);
        table.add_arm(dummy_value, default);
        builder.switch(dummy_value, table, None, SourceInfo::dummy());

        builder.move_to_block(bb3);
        builder.jump(default, SourceInfo::dummy());

        builder.move_to_block(bb4);
        builder.jump(merge, SourceInfo::dummy());

        builder.move_to_block(bb5);
        builder.jump(merge, SourceInfo::dummy());

        builder.move_to_block(default);
        builder.jump(merge, SourceInfo::dummy());

        builder.move_to_block(merge);
        builder.ret(dummy_value, SourceInfo::dummy());

        let mut func = builder.build();
        let mut order = serialize_func_body(&mut func);

        let mut arms = expect_switch(&mut order);
        assert_eq!(arms.len(), 3);

        let mut bb3_jump = arms.pop().unwrap();
        expect_end(&mut bb3_jump);

        let mut bb2_switch = arms.pop().unwrap();
        let bb2_switch_arms = expect_switch(&mut bb2_switch);
        assert_eq!(bb2_switch_arms.len(), 2);
        for mut bb2_switch_arm in bb2_switch_arms {
            expect_end(&mut bb2_switch_arm);
        }
        expect_end(&mut bb2_switch);

        let mut bb1_switch = arms.pop().unwrap();
        let bb1_switch_arms = expect_switch(&mut bb1_switch);
        assert_eq!(bb1_switch_arms.len(), 2);
        for mut bb1_switch_arm in bb1_switch_arms {
            expect_end(&mut bb1_switch_arm);
        }
        expect_end(&mut bb1_switch);

        expect_return(&func, &mut order);
        expect_end(&mut order);
    }
}
