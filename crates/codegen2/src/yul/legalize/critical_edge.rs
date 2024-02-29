use fe_mir::{
    analysis::ControlFlowGraph,
    ir::{
        body_cursor::{BodyCursor, CursorLocation},
        inst::InstKind,
        BasicBlock, BasicBlockId, FunctionBody, Inst, InstId, SourceInfo,
    },
};

#[derive(Debug)]
pub struct CriticalEdgeSplitter {
    critical_edges: Vec<CriticalEdge>,
}

impl CriticalEdgeSplitter {
    pub fn new() -> Self {
        Self {
            critical_edges: Vec::default(),
        }
    }

    pub fn run(&mut self, func: &mut FunctionBody) {
        let cfg = ControlFlowGraph::compute(func);

        for block in cfg.post_order() {
            let terminator = func.order.terminator(&func.store, block).unwrap();
            self.add_critical_edges(terminator, func, &cfg);
        }

        self.split_edges(func);
    }

    fn add_critical_edges(
        &mut self,
        terminator: InstId,
        func: &FunctionBody,
        cfg: &ControlFlowGraph,
    ) {
        for to in func.store.branch_info(terminator).block_iter() {
            if cfg.preds(to).len() > 1 {
                self.critical_edges.push(CriticalEdge { terminator, to });
            }
        }
    }

    fn split_edges(&mut self, func: &mut FunctionBody) {
        for edge in std::mem::take(&mut self.critical_edges) {
            let terminator = edge.terminator;
            let source_block = func.order.inst_block(terminator);
            let original_dest = edge.to;

            // Create new block that contains only jump inst.
            let new_dest = func.store.store_block(BasicBlock {});
            let mut cursor = BodyCursor::new(func, CursorLocation::BlockTop(source_block));
            cursor.insert_block(new_dest);
            cursor.set_loc(CursorLocation::BlockTop(new_dest));
            cursor.store_and_insert_inst(Inst::new(
                InstKind::Jump {
                    dest: original_dest,
                },
                SourceInfo::dummy(),
            ));

            // Rewrite branch destination to the new dest.
            func.store
                .rewrite_branch_dest(terminator, original_dest, new_dest);
        }
    }
}

#[derive(Debug)]
struct CriticalEdge {
    terminator: InstId,
    to: BasicBlockId,
}

#[cfg(test)]
mod tests {
    use fe_mir::ir::{body_builder::BodyBuilder, FunctionId, TypeId};

    use super::*;

    fn body_builder() -> BodyBuilder {
        BodyBuilder::new(FunctionId(0), SourceInfo::dummy())
    }

    #[test]
    fn critical_edge_remove() {
        let mut builder = body_builder();
        let lp_header = builder.make_block();
        let lp_body = builder.make_block();
        let exit = builder.make_block();

        let dummy_ty = TypeId(0);
        let v0 = builder.make_imm_from_bool(false, dummy_ty);
        builder.branch(v0, lp_header, exit, SourceInfo::dummy());

        builder.move_to_block(lp_header);
        builder.jump(lp_body, SourceInfo::dummy());

        builder.move_to_block(lp_body);
        builder.branch(v0, lp_header, exit, SourceInfo::dummy());

        builder.move_to_block(exit);
        builder.ret(v0, SourceInfo::dummy());

        let mut func = builder.build();
        CriticalEdgeSplitter::new().run(&mut func);
        let cfg = ControlFlowGraph::compute(&func);

        for &header_pred in cfg.preds(lp_header) {
            debug_assert_eq!(cfg.succs(header_pred).len(), 1);
            debug_assert_eq!(cfg.succs(header_pred)[0], lp_header);
        }

        for &exit_pred in cfg.preds(exit) {
            debug_assert_eq!(cfg.succs(exit_pred).len(), 1);
            debug_assert_eq!(cfg.succs(exit_pred)[0], exit);
        }
    }
}
