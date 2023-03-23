use parser::ast;

use crate::{
    hir_def::{
        Body, BodyNodeMap, BodySourceMap, Expr, ExprId, MaybeInvalid, Pat, PatId, Stmt, StmtId,
        TrackedBodyId, TrackedItemId,
    },
    span::{HirOrigin, HirOriginKind},
};

use super::FileLowerCtxt;

impl Body {
    pub(super) fn lower_ast(
        f_ctxt: &mut FileLowerCtxt,
        parent_id: TrackedItemId,
        ast: ast::Expr,
    ) -> Self {
        let bid = TrackedBodyId::ItemBody(parent_id.into());
        let mut ctxt = BodyCtxt::new(f_ctxt, bid);
        Expr::lower_ast(&mut ctxt, ast.clone());
        ctxt.build(&ast)
    }

    pub(super) fn lower_ast_nested(
        f_ctxt: &mut FileLowerCtxt,
        bid: TrackedBodyId,
        ast: ast::Expr,
    ) -> Self {
        let bid = TrackedBodyId::NestedBody(bid.into());
        let mut ctxt = BodyCtxt::new(f_ctxt, bid);
        Expr::lower_ast(&mut ctxt, ast.clone());
        ctxt.build(&ast)
    }

    pub(super) fn lower_ast_nameless(f_ctxt: &mut FileLowerCtxt<'_>, ast: ast::Expr) -> Self {
        let bid = TrackedBodyId::NamelessBody;
        let mut ctxt = BodyCtxt::new(f_ctxt, bid);
        Expr::lower_ast(&mut ctxt, ast.clone());
        ctxt.build(&ast)
    }
}

pub(super) struct BodyCtxt<'ctxt, 'db> {
    pub(super) f_ctxt: &'ctxt mut FileLowerCtxt<'db>,
    pub(super) bid: TrackedBodyId,

    pub(super) stmts: BodyNodeMap<StmtId, MaybeInvalid<Stmt>>,
    pub(super) exprs: BodyNodeMap<ExprId, MaybeInvalid<Expr>>,
    pub(super) pats: BodyNodeMap<PatId, MaybeInvalid<Pat>>,

    stmt_source_map: BodySourceMap<StmtId, ast::Stmt>,
    expr_source_map: BodySourceMap<ExprId, ast::Expr>,
    pat_source_map: BodySourceMap<PatId, ast::Pat>,
}
impl<'ctxt, 'db> BodyCtxt<'ctxt, 'db> {
    pub(super) fn push_expr(&mut self, expr: Expr, origin: HirOriginKind<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(Some(expr).into());
        self.expr_source_map[expr_id] = HirOrigin::new(self.f_ctxt.file, origin);
        expr_id
    }

    pub(super) fn push_invalid_expr(&mut self, origin: HirOriginKind<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(None.into());
        self.expr_source_map[expr_id] = HirOrigin::new(self.f_ctxt.file, origin);
        expr_id
    }

    pub(super) fn push_missing_expr(&mut self) -> ExprId {
        let expr_id = self.exprs.push(None.into());
        self.expr_source_map[expr_id] = HirOrigin::none(self.f_ctxt.file);
        expr_id
    }

    pub(super) fn push_stmt(&mut self, stmt: Stmt, origin: HirOriginKind<ast::Stmt>) -> StmtId {
        let stmt_id = self.stmts.push(Some(stmt).into());
        self.stmt_source_map[stmt_id] = HirOrigin::new(self.f_ctxt.file, origin);
        stmt_id
    }

    pub(super) fn push_pat(&mut self, pat: Pat, origin: HirOriginKind<ast::Pat>) -> PatId {
        let pat_id = self.pats.push(Some(pat).into());
        self.pat_source_map[pat_id] = HirOrigin::new(self.f_ctxt.file, origin);
        pat_id
    }

    pub(super) fn push_missing_pat(&mut self) -> PatId {
        let pat_id = self.pats.push(None.into());
        self.pat_source_map[pat_id] = HirOrigin::none(self.f_ctxt.file);
        pat_id
    }

    fn new(f_ctxt: &'ctxt mut FileLowerCtxt<'db>, bid: TrackedBodyId) -> Self {
        f_ctxt.enter_scope();
        Self {
            f_ctxt,
            bid,
            stmts: BodyNodeMap::new(),
            exprs: BodyNodeMap::new(),
            pats: BodyNodeMap::new(),
            stmt_source_map: BodySourceMap::new(),
            expr_source_map: BodySourceMap::new(),
            pat_source_map: BodySourceMap::new(),
        }
    }

    fn build(self, ast: &ast::Expr) -> Body {
        let origin = HirOrigin::raw(self.f_ctxt.file, ast);
        let body = Body::new(
            self.f_ctxt.db,
            self.bid,
            self.stmts,
            self.exprs,
            self.pats,
            self.stmt_source_map,
            self.expr_source_map,
            self.pat_source_map,
            origin,
        );

        self.f_ctxt.leave_scope(body);
        body
    }
}
