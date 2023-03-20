use cranelift_entity::{PrimaryMap, SecondaryMap};
use fe_parser2::ast;

use crate::{
    hir_def::{Body, Expr, ExprId, ItemKind, MaybeInvalid, Pat, PatId, Stmt, StmtId},
    span::{FileId, HirOrigin},
    HirDb,
};

impl Body {
    pub(crate) fn from_ast(
        db: &dyn HirDb,
        fid: FileId,
        parent_item: Option<ItemKind>,
        ast: ast::Expr,
    ) -> Self {
        todo!()
    }
}

pub(super) struct BodyCtxt<'db> {
    pub(super) stmts: PrimaryMap<StmtId, MaybeInvalid<Stmt>>,
    pub(super) exprs: PrimaryMap<ExprId, MaybeInvalid<Expr>>,
    pub(super) pats: PrimaryMap<PatId, MaybeInvalid<Pat>>,
    pub(super) db: &'db dyn HirDb,

    pub(super) stmt_source_map: SecondaryMap<StmtId, HirOrigin<ast::Stmt>>,
    pub(super) expr_source_map: SecondaryMap<ExprId, HirOrigin<ast::Expr>>,
    pub(super) pat_source_map: SecondaryMap<PatId, HirOrigin<ast::Pat>>,

    fid: FileId,
}
impl<'db> BodyCtxt<'db> {
    pub(super) fn push_pat(&mut self, pat: Option<Pat>, ast: &ast::Pat) -> PatId {
        let pat_id = self.pats.push(pat.into());
        self.pat_source_map[pat_id] = HirOrigin::raw(self.fid, ast);
        pat_id
    }

    pub(super) fn push_missing_pat(&mut self) -> PatId {
        let pat_id = self.pats.push(None.into());
        self.pat_source_map[pat_id] = HirOrigin::none(self.fid);
        pat_id
    }
}
