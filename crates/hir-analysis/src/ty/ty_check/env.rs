use hir::hir_def::{
    scope_graph::ScopeId, Body, BodyKind, Expr, ExprId, Func, IdentId, Partial, PatId, Stmt, StmtId,
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
            var_env: vec![BlockEnv::new(func.scope(), 0)],
            pending_vars: FxHashMap::default(),
        };

        env.enter_scope(body.expr(hir_db));

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

            if !ty.is_star_kind(db) {
                ty = TyId::invalid(db, InvalidCause::Other);
            }
            let var = LocalBinding::Param(i, ty);

            env.var_env.last_mut().unwrap().register_var(name, var);
        }

        Ok(env)
    }

    /// Returns a function if the `body` being checked has `BodyKind::FuncBody`.
    /// If the `body` has `BodyKind::Anonymous`, returns None
    pub(super) fn func(&self) -> Option<Func> {
        match self.body.body_kind(self.db.as_hir_db()) {
            BodyKind::FuncBody => self.var_env.first()?.scope.item().try_into().ok(),
            BodyKind::Anonymous => None,
        }
    }

    pub(super) fn body(&self) -> Body {
        self.body
    }

    pub(super) fn lookup_binding_ty(&self, binding: LocalBinding) -> TyId {
        match binding {
            LocalBinding::Local(pat) => self
                .pat_ty
                .get(&pat)
                .copied()
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),

            LocalBinding::Param(_, ty) => ty,
        }
    }

    pub(super) fn enter_scope(&mut self, block: ExprId) {
        let new_scope = match block.data(self.db.as_hir_db(), self.body) {
            Partial::Present(Expr::Block(_)) => ScopeId::Block(self.body, block),
            _ => self.scope(),
        };

        let var_env = BlockEnv::new(new_scope, self.var_env.len());
        self.var_env.push(var_env);
    }

    pub(super) fn leave_scope(&mut self) {
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
            var_env.register_var(name, LocalBinding::Local(pat));
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

    pub(super) fn current_block_idx(&self) -> usize {
        self.var_env.last().unwrap().idx
    }

    pub(super) fn get_block(&self, idx: usize) -> &BlockEnv {
        &self.var_env[idx]
    }
}

pub(super) struct BlockEnv {
    pub(super) scope: ScopeId,
    pub(super) vars: FxHashMap<IdentId, LocalBinding>,
    idx: usize,
}

impl BlockEnv {
    pub(super) fn lookup_var(&self, var: IdentId) -> Option<LocalBinding> {
        self.vars.get(&var).copied()
    }

    fn new(scope: ScopeId, idx: usize) -> Self {
        Self {
            scope,
            vars: FxHashMap::default(),
            idx,
        }
    }

    fn register_var(&mut self, name: IdentId, var: LocalBinding) {
        self.vars.insert(name, var);
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) enum LocalBinding {
    Local(PatId),
    Param(usize, TyId),
}
