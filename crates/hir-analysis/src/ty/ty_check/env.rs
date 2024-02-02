use hir::hir_def::{
    scope_graph::ScopeId, Body, Expr, ExprId, Func, IdentId, Partial, PatId, Stmt, StmtId,
};
use rustc_hash::FxHashMap;

use super::TypedBody;
use crate::{
    ty::{
        ty_def::{InvalidCause, TyId},
        ty_lower::lower_hir_ty,
        unify::UnificationTable,
    },
    HirAnalysisDb,
};

pub(super) struct TyCheckEnv<'db> {
    db: &'db dyn HirAnalysisDb,
    body: Body,

    pat_ty: FxHashMap<PatId, TyId>,
    expr_ty: FxHashMap<ExprId, TyId>,

    var_env: Vec<BlockEnv>,

    pending_vars: FxHashMap<IdentId, PatId>,
}

impl<'db> TyCheckEnv<'db> {
    pub(super) fn new_with_func(db: &'db dyn HirAnalysisDb, func: Func) -> Result<Self, ()> {
        let hir_db = db.as_hir_db();
        let Some(body) = func.body(hir_db) else {
            return Err(());
        };

        let mut env = Self {
            db,
            body,
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
            var_env: vec![],
            pending_vars: FxHashMap::default(),
        };

        env.enter_block(body.expr(hir_db))?;

        let Some(params) = func.params(hir_db).to_opt() else {
            return Err(());
        };

        for (i, param) in params.data(hir_db).iter().enumerate() {
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
            let var = VarKind::Param(i, ty);

            env.var_env.last_mut().unwrap().register_var(name, var);
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

    /// Register a pending binding which will be added when `flush_pending_vars`
    /// is called.
    pub(super) fn register_pending_binding(&mut self, name: IdentId, pat: PatId) {
        self.pending_vars.insert(name, pat);
    }

    /// Flush pending bindings to the current scope environment.
    pub(super) fn flush_pending_bindings(&mut self) {
        let var_env = self.var_env.last_mut().unwrap();
        for (name, pat) in self.pending_vars.drain() {
            var_env.register_var(name, VarKind::Local(pat));
        }
    }

    pub(super) fn finish(mut self, table: &mut UnificationTable) -> TypedBody {
        self.expr_ty
            .values_mut()
            .for_each(|ty| *ty = ty.apply_subst(self.db, table));

        self.pat_ty
            .values_mut()
            .for_each(|ty| *ty = ty.apply_subst(self.db, table));

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

    pub(super) fn scope(&self) -> ScopeId {
        self.var_env.last().unwrap().scope
    }
}

pub(super) struct BlockEnv {
    pub(super) scope: ScopeId,
    pub(super) vars: FxHashMap<IdentId, VarKind>,
}

impl BlockEnv {
    fn new(scope: ScopeId) -> Self {
        Self {
            scope,
            vars: FxHashMap::default(),
        }
    }

    fn register_var(&mut self, name: IdentId, var: VarKind) {
        self.vars.insert(name, var);
    }
}

#[derive(Clone, Copy)]
pub(super) enum VarKind {
    Local(PatId),
    Param(usize, TyId),
}
