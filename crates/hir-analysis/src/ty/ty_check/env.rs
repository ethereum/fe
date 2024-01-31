use std::collections::HashMap;

use hir::hir_def::{
    scope_graph::ScopeId, Body, Expr, ExprId, Func, IdentId, Partial, PatId, Stmt, StmtId,
};
use rustc_hash::FxHashMap;

use super::TypedBody;
use crate::{
    ty::{
        ty_def::{InvalidCause, TyId},
        ty_lower::lower_hir_ty,
    },
    HirAnalysisDb,
};

pub(super) struct ThCheckEnv<'db> {
    db: &'db dyn HirAnalysisDb,
    body: Body,

    pat_ty: FxHashMap<PatId, TyId>,
    expr_ty: FxHashMap<ExprId, TyId>,

    var_env: Vec<BlockEnv>,
}

impl<'db> ThCheckEnv<'db> {
    pub(super) fn new_with_func(db: &'db dyn HirAnalysisDb, func: Func) -> Result<Self, ()> {
        let hir_db = db.as_hir_db();
        let Some(body) = func.body(hir_db) else {
            return Err(());
        };

        let mut env = Self {
            db,
            body,
            pat_ty: HashMap::default(),
            expr_ty: HashMap::default(),
            var_env: vec![],
        };

        env.enter_block(body.expr(hir_db))?;

        let Some(params) = func.params(hir_db).to_opt() else {
            return Err(());
        };

        for param in params.data(hir_db) {
            let Some(name) = param.name() else {
                continue;
            };

            let mut ty = match param.ty {
                Partial::Present(hir_ty) => lower_hir_ty(db, hir_ty, func.scope()),
                Partial::Absent => TyId::invalid(db, InvalidCause::Other),
            };

            if ty.is_star_kind(db) {
                ty = TyId::invalid(db, InvalidCause::Other);
            }

            env.register_var(name, ty);
        }

        Ok(env)
    }

    pub(super) fn body(&self) -> Body {
        self.body
    }

    pub(super) fn enter_block(&mut self, block: ExprId) -> Result<(), ()> {
        if !matches!(
            block.data(self.db.as_hir_db(), self.body),
            Partial::Present(Expr::Block(_))
        ) {
            return Err(());
        }

        let var_env = BlockEnv::new(ScopeId::Block(self.body, block));
        self.var_env.push(var_env);
        Ok(())
    }

    pub(super) fn leave_block(&mut self) {
        self.var_env.pop().unwrap();
    }

    pub(super) fn type_expr(&mut self, expr: ExprId, ty: TyId) {
        self.expr_ty.insert(expr, ty);
    }

    pub(super) fn type_pat(&mut self, pat: PatId, ty: TyId) {
        self.pat_ty.insert(pat, ty);
    }

    pub(super) fn register_var(&mut self, name: IdentId, ty: TyId) {
        let var_env = self.var_env.last_mut().unwrap();
        var_env.vars.insert(name, ty);
    }

    pub(super) fn finish(self) -> TypedBody {
        TypedBody {
            body: Some(self.body),
            pat_ty: self.pat_ty,
            expr_ty: self.expr_ty,
        }
    }

    pub(super) fn expr_data(&self, expr: ExprId) -> &'db Partial<Expr> {
        expr.data(self.db.as_hir_db(), self.body)
    }

    pub(super) fn stmt_data(&self, stmt: StmtId) -> &'db Partial<Stmt> {
        stmt.data(self.db.as_hir_db(), self.body)
    }
}

struct BlockEnv {
    scope: ScopeId,
    vars: FxHashMap<IdentId, TyId>,
}

impl BlockEnv {
    fn new(scope: ScopeId) -> Self {
        Self {
            scope,
            vars: FxHashMap::default(),
        }
    }
}
