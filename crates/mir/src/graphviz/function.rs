use std::fmt::Write;

use dot2::{label, Id};
use fe_analyzer::namespace::items::FunctionId;

use crate::{analysis::ControlFlowGraph, db::MirDb, ir::FunctionSigId, pretty_print::PrettyPrint};

use super::block::BlockNode;

#[derive(Debug, Clone, Copy)]
pub(super) struct FunctionNode {
    pub(super) sig: FunctionSigId,
    func: FunctionId,
}

impl FunctionNode {
    pub(super) fn new(sig: FunctionSigId, func: FunctionId) -> Self {
        Self { sig, func }
    }

    pub(super) fn subgraph_id(self) -> Option<Id<'static>> {
        dot2::Id::new(format!("cluster_{}", self.sig.0)).ok()
    }

    pub(super) fn label(self, db: &dyn MirDb) -> label::Text<'static> {
        let mut label = self.signature(db);
        write!(label, r#"<br/><br align="left"/>"#).unwrap();

        // Maps local value id to local name.
        let body = db.mir_lowered_func_body(self.func);
        for local in body.store.locals() {
            local.pretty_print(db, &body.store, &mut label).unwrap();
            write!(
                label,
                r#" =&gt; {}<br align="left"/>"#,
                body.store.local_name(*local).unwrap()
            )
            .unwrap();
        }

        label::Text::HtmlStr(label.into())
    }

    pub(super) fn blocks(self, db: &dyn MirDb) -> Vec<BlockNode> {
        let body = db.mir_lowered_func_body(self.func);
        // We use control flow graph to collect reachable blocks.
        let cfg = ControlFlowGraph::compute(&body);
        cfg.post_order()
            .map(|block| BlockNode::new(self.sig, self.func, block))
            .collect()
    }

    fn signature(self, db: &dyn MirDb) -> String {
        let body = db.mir_lowered_func_body(self.func);

        let sig_data = self.sig.signature(db);
        let mut sig = format!("fn {}", self.sig.debug_name(db));
        if self.sig.is_generic(db) {
            write!(sig, "<").unwrap();
            let mut delim = "";
            for param in self.sig.type_params(db) {
                write!(sig, "{delim}{}", param.name).unwrap();
                delim = ", ";
            }
            write!(sig, ">(").unwrap()
        } else {
            write!(sig, "(").unwrap()
        }

        let params = &sig_data.params;
        let mut delim = "";
        for param in params.iter() {
            let name = &param.name;
            let ty = param.ty;
            write!(&mut sig, "{delim}{}: ", name).unwrap();
            ty.pretty_print(db, &body.store, &mut sig).unwrap();
            delim = ", ";
        }
        write!(sig, ")").unwrap();

        let ret_ty = self.sig.return_type(db);
        if let Some(ret_ty) = ret_ty {
            write!(sig, " -> ").unwrap();
            ret_ty.pretty_print(db, &body.store, &mut sig).unwrap();
        }

        dot2::escape_html(&sig)
    }
}
