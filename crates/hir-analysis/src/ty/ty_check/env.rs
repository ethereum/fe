use hir::{
    hir_def::{
        scope_graph::ScopeId, Body, BodyKind, Expr, ExprId, Func, IdentId, Partial, Pat, PatId,
        Stmt, StmtId,
    },
    span::DynLazySpan,
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
    expr_ty: FxHashMap<ExprId, TypedExpr>,

    var_env: Vec<BlockEnv>,

    pending_vars: FxHashMap<IdentId, LocalBinding>,

    loop_stack: Vec<StmtId>,
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
            loop_stack: Vec::new(),
        };

        env.enter_scope(body.expr(hir_db));

        let Some(params) = func.params(hir_db).to_opt() else {
            return Err(());
        };

        for (idx, param) in params.data(hir_db).iter().enumerate() {
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
            let var = LocalBinding::Param {
                idx,
                ty,
                is_mut: param.is_mut,
            };

            env.var_env.last_mut().unwrap().register_var(name, var);
        }

        Ok(env)
    }

    pub(super) fn typed_expr(&self, expr: ExprId) -> Option<TypedExpr> {
        self.expr_ty.get(&expr).copied()
    }

    pub(super) fn binding_def_span(&self, binding: LocalBinding) -> DynLazySpan {
        binding.def_span(self)
    }

    pub(super) fn binding_name(&self, binding: LocalBinding) -> IdentId {
        binding.binding_name(self)
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
            LocalBinding::Local { pat, .. } => self
                .pat_ty
                .get(&pat)
                .copied()
                .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other)),

            LocalBinding::Param { ty, .. } => ty,
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

    pub(super) fn enter_loop(&mut self, stmt: StmtId) {
        self.loop_stack.push(stmt);
    }

    pub(super) fn leave_loop(&mut self) {
        self.loop_stack.pop();
    }

    pub(super) fn current_loop(&self) -> Option<StmtId> {
        self.loop_stack.last().copied()
    }

    pub(super) fn type_expr(&mut self, expr: ExprId, typed: TypedExpr) {
        self.expr_ty.insert(expr, typed);
    }

    pub(super) fn type_pat(&mut self, pat: PatId, ty: TyId) {
        self.pat_ty.insert(pat, ty);
    }

    /// Register a pending binding which will be added when `flush_pending_vars`
    /// is called.
    pub(super) fn register_pending_binding(&mut self, name: IdentId, binding: LocalBinding) {
        self.pending_vars.insert(name, binding);
    }

    /// Flush pending bindings to the current scope environment.
    pub(super) fn flush_pending_bindings(&mut self) {
        let var_env = self.var_env.last_mut().unwrap();
        for (name, binding) in self.pending_vars.drain() {
            var_env.register_var(name, binding);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct TypedExpr {
    ty: TyId,
    is_mut: bool,
    binding: Option<LocalBinding>,
}

impl TypedExpr {
    pub(super) fn new(ty: TyId, is_mut: bool) -> Self {
        Self {
            ty,
            is_mut,
            binding: None,
        }
    }

    pub(super) fn new_binding_ref(ty: TyId, is_mut: bool, binding: LocalBinding) -> Self {
        Self {
            ty,
            is_mut,
            binding: Some(binding),
        }
    }

    pub(super) fn binding(&self) -> Option<LocalBinding> {
        self.binding
    }

    pub(super) fn swap_ty(&mut self, ty: TyId) -> TyId {
        std::mem::replace(&mut self.ty, ty)
    }

    pub(super) fn invalid(db: &dyn HirAnalysisDb) -> Self {
        Self {
            ty: TyId::invalid(db, InvalidCause::Other),
            is_mut: true,
            binding: None,
        }
    }

    pub(super) fn ty(&self) -> TyId {
        self.ty
    }

    pub(super) fn is_mutable(&self, _db: &dyn HirAnalysisDb) -> bool {
        // TODO: We need to check both type mutability and expr mutability when we have
        // `&mut` type.
        self.is_mut
    }

    pub(super) fn apply_subst(self, db: &dyn HirAnalysisDb, table: &mut UnificationTable) -> Self {
        Self {
            ty: self.ty.apply_subst(db, table),
            is_mut: self.is_mut,
            binding: self.binding,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum LocalBinding {
    Local { pat: PatId, is_mut: bool },
    Param { idx: usize, ty: TyId, is_mut: bool },
}

impl LocalBinding {
    pub(super) fn local(pat: PatId, is_mut: bool) -> Self {
        Self::Local { pat, is_mut }
    }

    pub(super) fn is_mut(&self) -> bool {
        match self {
            LocalBinding::Local { is_mut, .. } | LocalBinding::Param { is_mut, .. } => *is_mut,
        }
    }

    pub(super) fn binding_name(&self, env: &TyCheckEnv<'_>) -> IdentId {
        let hir_db = env.db.as_hir_db();
        match self {
            Self::Local { pat, .. } => {
                let Partial::Present(Pat::Path(Partial::Present(path), ..)) =
                    pat.data(hir_db, env.body())
                else {
                    unreachable!();
                };
                *path.last_segment(hir_db).unwrap()
            }

            Self::Param { idx, .. } => {
                let func = env.func().unwrap();
                let Partial::Present(func_params) = func.params(hir_db) else {
                    unreachable!();
                };

                func_params.data(hir_db)[*idx].name().unwrap()
            }
        }
    }

    fn def_span(&self, env: &TyCheckEnv<'_>) -> DynLazySpan {
        match self {
            LocalBinding::Local { pat, .. } => pat.lazy_span(env.body).into(),
            LocalBinding::Param { idx, .. } => {
                let func = env.func().unwrap();
                func.lazy_span()
                    .params_moved()
                    .param(*idx)
                    .name_moved()
                    .into()
            }
        }
    }
}
