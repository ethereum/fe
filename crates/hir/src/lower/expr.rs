use fe_parser2::ast;

use crate::hir_def::expr::*;

use super::body::BodyCtxt;

impl Expr {
    pub(super) fn push_to_body(ctxt: &mut BodyCtxt<'_>, ast: ast::Expr) -> ExprId {
        todo!()
    }

    pub(super) fn push_to_body_opt(ctxt: &mut BodyCtxt<'_>, ast: Option<ast::Expr>) -> ExprId {
        todo!()
    }
}

impl BinOp {
    pub fn from_ast(&self, ast: ast::BinOp) -> Self {
        match ast {
            ast::BinOp::Arith(arith) => ArithBinOp::from_ast(arith).into(),
            _ => {
                todo!()
            }
        }
    }
}

impl ArithBinOp {
    pub(super) fn from_ast(ast: ast::ArithBinOp) -> Self {
        match ast {
            ast::ArithBinOp::Add(_) => ArithBinOp::Add,
            ast::ArithBinOp::Sub(_) => ArithBinOp::Sub,
            ast::ArithBinOp::Mul(_) => ArithBinOp::Mul,
            ast::ArithBinOp::Div(_) => ArithBinOp::Div,
            ast::ArithBinOp::Mod(_) => ArithBinOp::Mod,
            ast::ArithBinOp::Pow(_) => ArithBinOp::Pow,
            ast::ArithBinOp::LShift(_) => ArithBinOp::LShift,
            ast::ArithBinOp::RShift(_) => ArithBinOp::RShift,
            ast::ArithBinOp::BitAnd(_) => ArithBinOp::BitAnd,
            ast::ArithBinOp::BitOr(_) => ArithBinOp::BitOr,
            ast::ArithBinOp::BitXor(_) => ArithBinOp::BitXor,
        }
    }
}
