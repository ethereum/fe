use fe_parser2::ast::{self, prelude::*};

use crate::{
    hir_def::{expr::*, Body, IdentId, IntegerId, LitKind, MaybeInvalid, Pat, PathId, Stmt},
    span::HirOriginKind,
};

use super::body::BodyCtxt;

impl Expr {
    pub(super) fn push_to_body(ctxt: &mut BodyCtxt<'_>, ast: ast::Expr) -> ExprId {
        let expr = match ast.kind() {
            ast::ExprKind::Lit(lit) => {
                if let Some(lit) = lit.lit() {
                    let lit = LitKind::from_ast(ctxt.db, lit);
                    Self::Lit(lit)
                } else {
                    return ctxt.push_invalid_expr(HirOriginKind::raw(&ast));
                }
            }

            ast::ExprKind::Block(block) => {
                let mut stmts = vec![];
                for stmt in block.stmts() {
                    let stmt = Stmt::push_to_body(ctxt, stmt);
                    stmts.push(stmt);
                }
                Self::Block(stmts)
            }

            ast::ExprKind::Bin(bin) => {
                let lhs = Self::push_to_body_opt(ctxt, bin.lhs());
                let rhs = Self::push_to_body_opt(ctxt, bin.rhs());
                let op = bin.op().map(|op| BinOp::from_ast(op)).into();
                Self::Bin(lhs, rhs, op)
            }

            ast::ExprKind::Un(un) => {
                let expr = Self::push_to_body_opt(ctxt, un.expr());
                let op = un.op().map(|op| UnOp::from_ast(op)).into();
                Self::Un(expr, op)
            }

            ast::ExprKind::Call(call) => {
                let callee = Self::push_to_body_opt(ctxt, call.callee());
                let args = call
                    .args()
                    .map(|args| {
                        args.into_iter()
                            .map(|arg| CallArg::from_ast(ctxt, arg))
                            .collect()
                    })
                    .unwrap_or_default();
                Self::Call(callee, args)
            }

            ast::ExprKind::MethodCall(method_call) => {
                let receiver = Self::push_to_body_opt(ctxt, method_call.receiver());
                let method_name = IdentId::maybe_from_token(ctxt.db, method_call.method_name());
                let args = method_call
                    .args()
                    .map(|args| {
                        args.into_iter()
                            .map(|arg| CallArg::from_ast(ctxt, arg))
                            .collect()
                    })
                    .unwrap_or_default();
                Self::MethodCall(receiver, method_name, args)
            }

            ast::ExprKind::Path(path) => {
                let path = PathId::maybe_from_ast(ctxt.db, path.path());
                Self::Path(path)
            }

            ast::ExprKind::RecordInit(record_init) => {
                let path = PathId::maybe_from_ast(ctxt.db, record_init.path());
                let fields = record_init
                    .fields()
                    .map(|fields| {
                        fields
                            .into_iter()
                            .map(|field| RecordField::from_ast(ctxt, field))
                            .collect()
                    })
                    .unwrap_or_default();
                Self::RecordInit(path, fields)
            }

            ast::ExprKind::Field(field) => {
                let receiver = Self::push_to_body_opt(ctxt, field.receiver());
                let field = if let Some(name) = field.field_name() {
                    Some(FieldIndex::Ident(IdentId::from_token(ctxt.db, name))).into()
                } else if let Some(num) = field.field_index() {
                    Some(FieldIndex::Index(IntegerId::from_ast(ctxt.db, num))).into()
                } else {
                    None.into()
                };
                Self::Field(receiver, field)
            }

            ast::ExprKind::Index(index) => {
                let indexed = Self::push_to_body_opt(ctxt, index.expr());
                let index = Self::push_to_body_opt(ctxt, index.index());
                Self::Index(indexed, index)
            }

            ast::ExprKind::Tuple(tup) => {
                let elems = tup
                    .elems()
                    .map(|elem| Self::push_to_body_opt(ctxt, elem))
                    .collect();

                Self::Tuple(elems)
            }

            ast::ExprKind::Array(array) => {
                let elems = array
                    .elems()
                    .map(|elem| Self::push_to_body_opt(ctxt, elem))
                    .collect();
                Self::Array(elems)
            }

            ast::ExprKind::ArrayRep(array_rep) => {
                let val = Self::push_to_body_opt(ctxt, array_rep.val());
                let len = array_rep
                    .len()
                    .map(|ast| Body::nameless_from_ast(ctxt.db, ctxt.fid, ast))
                    .into();
                Self::ArrayRep(val, len)
            }

            ast::ExprKind::If(if_) => {
                let cond = Self::push_to_body_opt(ctxt, if_.cond());
                let then = Expr::push_to_body_opt(
                    ctxt,
                    if_.then()
                        .map(|body| ast::Expr::cast(body.syntax().clone()))
                        .flatten(),
                );
                let else_ = if_.else_().map(|ast| Self::push_to_body(ctxt, ast));
                Self::If(cond, then, else_)
            }

            ast::ExprKind::Match(match_) => {
                let scrutinee = Self::push_to_body_opt(ctxt, match_.scrutinee());
                let arm = match_
                    .arms()
                    .map(|arms| {
                        arms.into_iter()
                            .map(|arm| MatchArm::from_ast(ctxt, arm))
                            .collect()
                    })
                    .into();

                Self::Match(scrutinee, arm)
            }

            ast::ExprKind::Paren(paren) => {
                return Self::push_to_body_opt(ctxt, paren.expr());
            }
        };

        ctxt.push_expr(expr, HirOriginKind::raw(&ast))
    }

    pub(super) fn push_to_body_opt(ctxt: &mut BodyCtxt<'_>, ast: Option<ast::Expr>) -> ExprId {
        if let Some(ast) = ast {
            Expr::push_to_body(ctxt, ast)
        } else {
            ctxt.push_missing_expr()
        }
    }
}

impl BinOp {
    pub(super) fn from_ast(ast: ast::BinOp) -> Self {
        match ast {
            ast::BinOp::Arith(arith) => ArithBinOp::from_ast(arith).into(),
            ast::BinOp::Comp(arith) => CompBinOp::from_ast(arith).into(),
            ast::BinOp::Logical(arith) => LogicalBinOp::from_ast(arith).into(),
        }
    }
}

impl ArithBinOp {
    pub(super) fn from_ast(ast: ast::ArithBinOp) -> Self {
        match ast {
            ast::ArithBinOp::Add(_) => Self::Add,
            ast::ArithBinOp::Sub(_) => Self::Sub,
            ast::ArithBinOp::Mul(_) => Self::Mul,
            ast::ArithBinOp::Div(_) => Self::Div,
            ast::ArithBinOp::Mod(_) => Self::Mod,
            ast::ArithBinOp::Pow(_) => Self::Pow,
            ast::ArithBinOp::LShift(_) => Self::LShift,
            ast::ArithBinOp::RShift(_) => Self::RShift,
            ast::ArithBinOp::BitAnd(_) => Self::BitAnd,
            ast::ArithBinOp::BitOr(_) => Self::BitOr,
            ast::ArithBinOp::BitXor(_) => Self::BitXor,
        }
    }
}

impl CompBinOp {
    pub(super) fn from_ast(ast: ast::CompBinOp) -> Self {
        match ast {
            ast::CompBinOp::Eq(_) => Self::Eq,
            ast::CompBinOp::NotEq(_) => Self::NotEq,
            ast::CompBinOp::Lt(_) => Self::Lt,
            ast::CompBinOp::LtEq(_) => Self::LtEq,
            ast::CompBinOp::Gt(_) => Self::Gt,
            ast::CompBinOp::GtEq(_) => Self::GtEq,
        }
    }
}

impl LogicalBinOp {
    pub(super) fn from_ast(ast: ast::LogicalBinOp) -> Self {
        match ast {
            ast::LogicalBinOp::And(_) => Self::And,
            ast::LogicalBinOp::Or(_) => Self::Or,
        }
    }
}

impl UnOp {
    fn from_ast(ast: ast::UnOp) -> Self {
        match ast {
            ast::UnOp::Plus(_) => Self::Plus,
            ast::UnOp::Minus(_) => Self::Minus,
            ast::UnOp::Not(_) => Self::Not,
            ast::UnOp::BitNot(_) => Self::BitNot,
        }
    }
}

impl MatchArm {
    fn from_ast(ctxt: &mut BodyCtxt<'_>, ast: ast::MatchArm) -> Self {
        let pat = Pat::push_to_body_opt(ctxt, ast.pat());
        let body = Expr::push_to_body_opt(ctxt, ast.body());
        Self { pat, body }
    }
}

impl CallArg {
    fn from_ast(ctxt: &mut BodyCtxt<'_>, ast: ast::CallArg) -> Self {
        let label = ast.label().map(|label| IdentId::from_token(ctxt.db, label));
        let expr = Expr::push_to_body_opt(ctxt, ast.expr());
        Self { label, expr }
    }

    fn from_ast_opt(ctxt: &mut BodyCtxt<'_>, ast: Option<ast::CallArg>) -> MaybeInvalid<Self> {
        ast.map(|ast| Self::from_ast(ctxt, ast)).into()
    }
}

impl RecordField {
    fn from_ast(ctxt: &mut BodyCtxt<'_>, ast: ast::RecordField) -> Self {
        let label = ast.label().map(|label| IdentId::from_token(ctxt.db, label));
        let expr = Expr::push_to_body_opt(ctxt, ast.expr());
        Self { label, expr }
    }
}
