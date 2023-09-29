use parser::ast::{self, prelude::*};

use crate::{
    hir_def::{stmt::*, Expr, Pat, TypeId},
    span::HirOrigin,
};

use super::body::BodyCtxt;

impl Stmt {
    pub(super) fn push_to_body(ctxt: &mut BodyCtxt<'_, '_>, ast: ast::Stmt) -> StmtId {
        let (stmt, origin_kind) = match ast.kind() {
            ast::StmtKind::Let(let_) => {
                let pat = Pat::lower_ast_opt(ctxt, let_.pat());
                let ty = let_
                    .type_annotation()
                    .map(|ty| TypeId::lower_ast(ctxt.f_ctxt, ty));
                let init = let_.initializer().map(|init| Expr::lower_ast(ctxt, init));
                (Stmt::Let(pat, ty, init), HirOrigin::raw(&ast))
            }
            ast::StmtKind::For(for_) => {
                let bind = Pat::lower_ast_opt(ctxt, for_.pat());
                let iter = Expr::push_to_body_opt(ctxt, for_.iterable());
                let body = Expr::push_to_body_opt(
                    ctxt,
                    for_.body()
                        .and_then(|body| ast::Expr::cast(body.syntax().clone())),
                );

                (Stmt::For(bind, iter, body), HirOrigin::raw(&ast))
            }

            ast::StmtKind::While(while_) => {
                let cond = Expr::push_to_body_opt(ctxt, while_.cond());
                let body = Expr::push_to_body_opt(
                    ctxt,
                    while_
                        .body()
                        .and_then(|body| ast::Expr::cast(body.syntax().clone())),
                );

                (Stmt::While(cond, body), HirOrigin::raw(&ast))
            }

            ast::StmtKind::Continue(_) => (Stmt::Continue, HirOrigin::raw(&ast)),

            ast::StmtKind::Break(_) => (Stmt::Break, HirOrigin::raw(&ast)),

            ast::StmtKind::Return(ret) => {
                let expr = ret
                    .has_value()
                    .then(|| Expr::push_to_body_opt(ctxt, ret.expr()));
                (Stmt::Return(expr), HirOrigin::raw(&ast))
            }

            ast::StmtKind::Expr(expr) => {
                let expr = Expr::push_to_body_opt(ctxt, expr.expr());
                (Stmt::Expr(expr), HirOrigin::raw(&ast))
            }
        };

        ctxt.push_stmt(stmt, origin_kind)
    }
}
<<<<<<< HEAD

fn desugar_aug_assign(
    ctxt: &mut BodyCtxt<'_, '_>,
    ast: &ast::AugAssignStmt,
) -> (Stmt, HirOrigin<ast::Stmt>) {
    let lhs_ident = ast.ident();
    let path = lhs_ident
        .clone()
        .map(|ident| PathId::from_token(ctxt.f_ctxt, ident));

    let lhs_origin: AugAssignDesugared = lhs_ident.unwrap().text_range().into();
    let lhs_pat = if let Some(path) = path {
        ctxt.push_pat(
            Pat::Path(Some(path).into()),
            HirOrigin::desugared(lhs_origin.clone()),
        )
    } else {
        ctxt.push_missing_pat()
    };

    let binop_lhs = if let Some(path) = path {
        ctxt.push_expr(
            Expr::Path(Some(path).into()),
            HirOrigin::desugared(lhs_origin),
        )
    } else {
        ctxt.push_missing_expr()
    };

    let binop_rhs = ast
        .expr()
        .map(|expr| Expr::lower_ast(ctxt, expr))
        .unwrap_or_else(|| ctxt.push_missing_expr());

    let binop = ast.op().map(|op| ArithBinOp::lower_ast(op).into()).into();
    let expr = ctxt.push_expr(
        Expr::Bin(binop_lhs, binop_rhs, binop),
        HirOrigin::desugared(AugAssignDesugared::stmt(ast)),
    );

    (
        Stmt::Assign(lhs_pat, expr),
        HirOrigin::desugared(AugAssignDesugared::stmt(ast)),
    )
}
=======
>>>>>>> 8e95c982 (Assign and Aug Assign)
