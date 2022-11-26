use std::fmt::Write;

use dot2::{label, Id};
use fe_analyzer::namespace::items::FunctionId;

use crate::{
    analysis::ControlFlowGraph,
    db::MirDb,
    ir::{BasicBlockId, FunctionSigId},
    pretty_print::PrettyPrint,
};

#[derive(Debug, Clone, Copy)]
pub(super) struct BlockNode {
    sig: FunctionSigId,
    func: FunctionId,
    pub block: BasicBlockId,
}

impl BlockNode {
    pub(super) fn new(sig: FunctionSigId, func: FunctionId, block: BasicBlockId) -> Self {
        Self { sig, func, block }
    }
    pub(super) fn id(self) -> dot2::Result<Id<'static>> {
        Id::new(format!("fn{}_bb{}", self.sig.0, self.block.index()))
    }

    pub(super) fn label(self, db: &dyn MirDb) -> label::Text<'static> {
        let mut label = r#"<table border="0" cellborder="1" cellspacing="0">"#.to_string();

        // Write block header.
        write!(
            &mut label,
            r#"<tr><td bgcolor="gray" align="center" colspan="1">BB{}</td></tr>"#,
            self.block.index()
        )
        .unwrap();

        // Write block body.
        let func_body = db.mir_lowered_func_body(self.func);
        write!(label, r#"<tr><td align="left" balign="left">"#).unwrap();
        for inst in func_body.order.iter_inst(self.block) {
            let mut inst_string = String::new();
            inst.pretty_print(db, &func_body.store, &mut inst_string)
                .unwrap();
            write!(label, "{}", dot2::escape_html(&inst_string)).unwrap();
            write!(label, "<br/>").unwrap();
        }
        write!(label, r#"</td></tr>"#).unwrap();

        write!(label, "</table>").unwrap();

        label::Text::HtmlStr(label.into())
    }

    pub(super) fn succs(self, db: &dyn MirDb) -> Vec<BlockNode> {
        let func_body = db.mir_lowered_func_body(self.func);
        let cfg = ControlFlowGraph::compute(&func_body);
        cfg.succs(self.block)
            .iter()
            .map(|block| Self::new(self.sig, self.func, *block))
            .collect()
    }
}
