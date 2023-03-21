use cranelift_entity::{PrimaryMap, SecondaryMap};
use fe_parser2::ast;

use crate::{
    hir_def::{Body, Expr, ExprId, ItemKind, MaybeInvalid, Pat, PatId, Stmt, StmtId},
    span::{FileId, HirOrigin, HirOriginKind},
    HirDb,
};

impl Body {
    pub(crate) fn item_body_from_ast(
        db: &dyn HirDb,
        fid: FileId,
        parent_item: ItemKind,
        ast: ast::Expr,
    ) -> Self {
        todo!()
    }

    pub(crate) fn nameless_from_ast(db: &dyn HirDb, fid: FileId, ast: ast::Expr) -> Self {
        todo!()
    }
}

pub(super) struct BodyCtxt<'db> {
    pub(super) stmts: PrimaryMap<StmtId, MaybeInvalid<Stmt>>,
    pub(super) exprs: PrimaryMap<ExprId, MaybeInvalid<Expr>>,
    pub(super) pats: PrimaryMap<PatId, MaybeInvalid<Pat>>,
    pub(super) db: &'db dyn HirDb,
    pub(super) fid: FileId,

    stmt_source_map: SecondaryMap<StmtId, HirOrigin<ast::Stmt>>,
    expr_source_map: SecondaryMap<ExprId, HirOrigin<ast::Expr>>,
    pat_source_map: SecondaryMap<PatId, HirOrigin<ast::Pat>>,
}
impl<'db> BodyCtxt<'db> {
    pub(super) fn push_expr(&mut self, expr: Expr, origin: HirOriginKind<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(Some(expr).into());
        self.expr_source_map[expr_id] = HirOrigin::new(self.fid, origin);
        expr_id
    }

    pub(super) fn push_invalid_expr(&mut self, origin: HirOriginKind<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(None.into());
        self.expr_source_map[expr_id] = HirOrigin::new(self.fid, origin);
        expr_id
    }

    pub(super) fn push_missing_expr(&mut self) -> ExprId {
        let expr_id = self.exprs.push(None.into());
        self.expr_source_map[expr_id] = HirOrigin::none(self.fid);
        expr_id
    }

    pub(super) fn push_stmt(&mut self, stmt: Stmt, origin: HirOriginKind<ast::Stmt>) -> StmtId {
        let stmt_id = self.stmts.push(Some(stmt).into());
        self.stmt_source_map[stmt_id] = HirOrigin::new(self.fid, origin);
        stmt_id
    }

    pub(super) fn push_pat(&mut self, pat: Pat, origin: HirOriginKind<ast::Pat>) -> PatId {
        let pat_id = self.pats.push(Some(pat).into());
        self.pat_source_map[pat_id] = HirOrigin::new(self.fid, origin);
        pat_id
    }

    pub(super) fn push_missing_pat(&mut self) -> PatId {
        let pat_id = self.pats.push(None.into());
        self.pat_source_map[pat_id] = HirOrigin::none(self.fid);
        pat_id
    }
}
