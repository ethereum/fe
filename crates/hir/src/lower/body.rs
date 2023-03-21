use fe_parser2::ast;

use crate::{
    hir_def::{
        Body, BodyNodeMap, BodySourceMap, Expr, ExprId, MaybeInvalid, Pat, PatId, Stmt, StmtId,
        TrackedBodyId, TrackedItemId,
    },
    input::File,
    span::{HirOrigin, HirOriginKind},
    HirDb,
};

impl Body {
    pub(crate) fn item_body_from_ast(
        db: &dyn HirDb,
        file: File,
        parent_id: TrackedItemId,
        ast: ast::Expr,
    ) -> Self {
        let bid = TrackedBodyId::ItemBody(parent_id.into());
        let mut ctxt = BodyCtxt::new(db, file, bid);
        Expr::push_to_body(&mut ctxt, ast.clone());
        ctxt.build(HirOrigin::raw(file, &ast))
    }

    pub(crate) fn nested_body_from_ast(
        db: &dyn HirDb,
        file: File,
        bid: TrackedBodyId,
        ast: ast::Expr,
    ) -> Self {
        let bid = TrackedBodyId::NestedBody(bid.into());
        let mut ctxt = BodyCtxt::new(db, file, bid);
        Expr::push_to_body(&mut ctxt, ast.clone());
        ctxt.build(HirOrigin::raw(file, &ast))
    }

    pub(crate) fn nameless_body_from_ast(db: &dyn HirDb, file: File, ast: ast::Expr) -> Self {
        let bid = TrackedBodyId::NamelessBody;
        let mut ctxt = BodyCtxt::new(db, file, bid);
        Expr::push_to_body(&mut ctxt, ast.clone());
        ctxt.build(HirOrigin::raw(file, &ast))
    }
}

pub(super) struct BodyCtxt<'db> {
    pub(super) stmts: BodyNodeMap<StmtId, MaybeInvalid<Stmt>>,
    pub(super) exprs: BodyNodeMap<ExprId, MaybeInvalid<Expr>>,
    pub(super) pats: BodyNodeMap<PatId, MaybeInvalid<Pat>>,
    pub(super) db: &'db dyn HirDb,
    pub(super) file: File,
    pub(super) bid: TrackedBodyId,

    stmt_source_map: BodySourceMap<StmtId, ast::Stmt>,
    expr_source_map: BodySourceMap<ExprId, ast::Expr>,
    pat_source_map: BodySourceMap<PatId, ast::Pat>,
}
impl<'db> BodyCtxt<'db> {
    pub(super) fn push_expr(&mut self, expr: Expr, origin: HirOriginKind<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(Some(expr).into());
        self.expr_source_map[expr_id] = HirOrigin::new(self.file, origin);
        expr_id
    }

    pub(super) fn push_invalid_expr(&mut self, origin: HirOriginKind<ast::Expr>) -> ExprId {
        let expr_id = self.exprs.push(None.into());
        self.expr_source_map[expr_id] = HirOrigin::new(self.file, origin);
        expr_id
    }

    pub(super) fn push_missing_expr(&mut self) -> ExprId {
        let expr_id = self.exprs.push(None.into());
        self.expr_source_map[expr_id] = HirOrigin::none(self.file);
        expr_id
    }

    pub(super) fn push_stmt(&mut self, stmt: Stmt, origin: HirOriginKind<ast::Stmt>) -> StmtId {
        let stmt_id = self.stmts.push(Some(stmt).into());
        self.stmt_source_map[stmt_id] = HirOrigin::new(self.file, origin);
        stmt_id
    }

    pub(super) fn push_pat(&mut self, pat: Pat, origin: HirOriginKind<ast::Pat>) -> PatId {
        let pat_id = self.pats.push(Some(pat).into());
        self.pat_source_map[pat_id] = HirOrigin::new(self.file, origin);
        pat_id
    }

    pub(super) fn push_missing_pat(&mut self) -> PatId {
        let pat_id = self.pats.push(None.into());
        self.pat_source_map[pat_id] = HirOrigin::none(self.file);
        pat_id
    }

    fn new(db: &'db dyn HirDb, file: File, bid: TrackedBodyId) -> Self {
        Self {
            stmts: BodyNodeMap::new(),
            exprs: BodyNodeMap::new(),
            pats: BodyNodeMap::new(),
            db,
            file,
            bid,
            stmt_source_map: BodySourceMap::new(),
            expr_source_map: BodySourceMap::new(),
            pat_source_map: BodySourceMap::new(),
        }
    }

    fn build(self, origin: HirOrigin<ast::Expr>) -> Body {
        Body::new(
            self.db,
            self.bid,
            self.stmts,
            self.exprs,
            self.pats,
            self.stmt_source_map,
            self.expr_source_map,
            self.pat_source_map,
            origin,
        )
    }
}
