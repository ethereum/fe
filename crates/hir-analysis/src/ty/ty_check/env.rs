use hir::{
    hir_def::{
        prim_ty::PrimTy, scope_graph::ScopeId, Body, BodyKind, Expr, ExprId, Func, IdentId,
        IntegerId, Partial, Pat, PatId, Stmt, StmtId,
    },
    span::DynLazySpan,
};
use rustc_hash::FxHashMap;

use super::{Callable, TypedBody};
use crate::{
    ty::{
        canonical::{Canonical, Canonicalized},
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
        diagnostics::{BodyDiag, FuncBodyDiag},
        fold::{TyFoldable, TyFolder},
        func_def::{lower_func, FuncDef},
        trait_def::TraitInstId,
        trait_resolution::{
            constraint::collect_func_def_constraints, is_goal_satisfiable, GoalSatisfiability,
            PredicateListId,
        },
        ty_def::{InvalidCause, TyData, TyId, TyVarSort},
        ty_lower::lower_hir_ty,
        unify::UnificationTable,
    },
    HirAnalysisDb,
};

pub(super) struct TyCheckEnv<'db> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,

    pat_ty: FxHashMap<PatId, TyId<'db>>,
    expr_ty: FxHashMap<ExprId, ExprProp<'db>>,
    callables: FxHashMap<ExprId, Callable<'db>>,

    pending_confirmations: Vec<(TraitInstId<'db>, DynLazySpan<'db>)>,

    var_env: Vec<BlockEnv<'db>>,
    pending_vars: FxHashMap<IdentId<'db>, LocalBinding<'db>>,
    loop_stack: Vec<StmtId>,
}

impl<'db> TyCheckEnv<'db> {
    pub(super) fn new_with_func(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> Result<Self, ()> {
        let hir_db = db.as_hir_db();
        let Some(body) = func.body(hir_db) else {
            return Err(());
        };

        let mut env = Self {
            db,
            body,
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
            callables: FxHashMap::default(),
            pending_confirmations: Vec::new(),
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

    pub(super) fn typed_expr(&self, expr: ExprId) -> Option<ExprProp<'db>> {
        self.expr_ty.get(&expr).copied()
    }

    pub(super) fn binding_def_span(&self, binding: LocalBinding<'db>) -> DynLazySpan<'db> {
        binding.def_span(self)
    }

    pub(super) fn register_callable(&mut self, expr: ExprId, callable: Callable<'db>) {
        if self.callables.insert(expr, callable).is_some() {
            panic!("callable is already registered for the given expr")
        }
    }
    pub(super) fn binding_name(&self, binding: LocalBinding<'db>) -> IdentId<'db> {
        binding.binding_name(self)
    }

    /// Returns a function if the `body` being checked has `BodyKind::FuncBody`.
    /// If the `body` has `BodyKind::Anonymous`, returns None
    pub(super) fn func(&self) -> Option<FuncDef<'db>> {
        let func = match self.body.body_kind(self.db.as_hir_db()) {
            BodyKind::FuncBody => self.var_env.first()?.scope.item().try_into().ok(),
            BodyKind::Anonymous => None,
        }?;

        lower_func(self.db, func)
    }

    pub(super) fn assumptions(&self) -> PredicateListId<'db> {
        match self.func() {
            Some(func) => collect_func_def_constraints(self.db, func, true).instantiate_identity(),
            None => PredicateListId::empty_list(self.db),
        }
    }

    pub(super) fn body(&self) -> Body<'db> {
        self.body
    }

    pub(super) fn lookup_binding_ty(&self, binding: LocalBinding<'db>) -> TyId<'db> {
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

    pub(super) fn type_expr(&mut self, expr: ExprId, typed: ExprProp<'db>) {
        self.expr_ty.insert(expr, typed);
    }

    pub(super) fn type_pat(&mut self, pat: PatId, ty: TyId<'db>) {
        self.pat_ty.insert(pat, ty);
    }

    /// Registers a new pending binding.
    ///
    /// This function adds a binding to the list of pending variables. If a
    /// binding with the same name already exists, it returns the existing
    /// binding. Otherwise, it returns `None`.
    ///
    /// To flush pending bindings to the designated scope, call
    /// [`flush_pending_bindings`] in the scope.
    ///
    /// # Arguments
    ///
    /// * `name` - The identifier of the variable.
    /// * `binding` - The local binding to be registered.
    ///
    /// # Returns
    ///
    /// * `Some(LocalBinding)` if a binding with the same name already exists.
    /// * `None` if the binding was successfully registered.
    pub(super) fn register_pending_binding(
        &mut self,
        name: IdentId<'db>,
        binding: LocalBinding<'db>,
    ) -> Option<LocalBinding<'db>> {
        self.pending_vars.insert(name, binding)
    }

    /// Flushes all pending variable bindings into the current variable
    /// environment.
    ///
    /// This function moves all pending bindings from the `pending_vars` map
    /// into the latest `BlockEnv` in `var_env`. After this operation, the
    /// `pending_vars` map will be empty.
    pub(super) fn flush_pending_bindings(&mut self) {
        let var_env = self.var_env.last_mut().unwrap();
        for (name, binding) in self.pending_vars.drain() {
            var_env.register_var(name, binding);
        }
    }

    pub(super) fn register_confirmation(&mut self, inst: TraitInstId<'db>, span: DynLazySpan<'db>) {
        self.pending_confirmations.push((inst, span))
    }

    /// Completes the type checking environment by finalizing pending trait
    /// confirmations, folding types with the unification table, and collecting
    /// diagnostics.
    ///
    /// # Arguments
    ///
    /// * `table` - A mutable reference to the unification table used for type
    ///   unification.
    ///
    /// # Returns
    ///
    /// * A tuple containing the `TypedBody` and a vector of `FuncBodyDiag`.
    ///
    /// The `TypedBody` includes the body of the function, pattern types,
    /// expression types, and callables, all of which have been folded with
    /// the unification table.
    ///
    /// The vector of `FuncBodyDiag` contains diagnostics related to function
    /// bodies, such as ambiguous trait instances.
    pub(super) fn finish(
        mut self,
        table: &mut UnificationTable<'db>,
        sink: &mut Vec<FuncBodyDiag<'db>>,
    ) -> TypedBody<'db> {
        let mut prober = Prober { table };
        self.perform_pending_confirmation(&mut prober, sink);

        self.expr_ty
            .values_mut()
            .for_each(|ty| *ty = ty.fold_with(&mut prober));

        self.pat_ty
            .values_mut()
            .for_each(|ty| *ty = ty.fold_with(&mut prober));

        let callables = self
            .callables
            .into_iter()
            .map(|(expr, callable)| (expr, callable.fold_with(&mut prober)))
            .collect();

        TypedBody {
            body: Some(self.body),
            pat_ty: self.pat_ty,
            expr_ty: self.expr_ty,
            callables,
        }
    }

    pub(super) fn expr_data(&self, expr: ExprId) -> &'db Partial<Expr<'db>> {
        expr.data(self.db.as_hir_db(), self.body)
    }

    pub(super) fn stmt_data(&self, stmt: StmtId) -> &'db Partial<Stmt<'db>> {
        stmt.data(self.db.as_hir_db(), self.body)
    }

    pub(super) fn scope(&self) -> ScopeId<'db> {
        self.var_env.last().unwrap().scope
    }

    pub(super) fn current_block_idx(&self) -> usize {
        self.var_env.last().unwrap().idx
    }

    pub(super) fn get_block(&self, idx: usize) -> &BlockEnv<'db> {
        &self.var_env[idx]
    }

    /// Performs pending trait confirmations and collects diagnostics.
    ///
    /// This function attempts to satisfy all pending trait confirmations by
    /// iteratively probing and unifying trait instances until a fixed point
    /// is reached. If any trait instance remains ambiguous, a diagnostic is
    /// generated and added to the diagnostics vector.
    ///
    /// # Arguments
    ///
    /// * `prober` - A mutable reference to the [`Prober`] used for type
    ///   unification and probing.
    ///
    /// # Returns
    ///
    /// * A vector of `FuncBodyDiag` containing diagnostics related to ambiguous
    ///   trait instances.
    fn perform_pending_confirmation(
        &self,
        prober: &mut Prober<'db, '_>,
        sink: &mut Vec<FuncBodyDiag<'db>>,
    ) {
        let assumptions = self.assumptions();
        let mut changed = true;
        let hir_db = self.db.as_hir_db();
        let ingot = self.body().top_mod(hir_db).ingot(hir_db);
        // Try to perform confirmation until all pending confirmations reaches to
        // the fixed point.
        while changed {
            changed = false;
            for (inst, _) in &self.pending_confirmations {
                let inst = inst.fold_with(prober);
                let canonical_inst = Canonicalized::new(self.db, inst);
                if let GoalSatisfiability::Satisfied(solution) =
                    is_goal_satisfiable(self.db, ingot, canonical_inst.value, assumptions)
                {
                    let solution = canonical_inst.extract_solution(prober.table, *solution);
                    prober.table.unify(inst, solution).unwrap();

                    // We need compare old and new inst in a canonical form since a new inst might
                    // introduce new type variable in some cases.
                    // In other word, we need to check ⍺-equivalence to know whether the
                    // confirmation step move forward.
                    let new_canonical_inst = Canonical::new(self.db, inst.fold_with(prober.table));
                    changed |= new_canonical_inst != canonical_inst.value;
                }
            }
        }

        // Finds ambiguous trait inst and emits diags.
        for (inst, span) in &self.pending_confirmations {
            let inst = inst.fold_with(prober);
            let canonical_inst = Canonicalized::new(self.db, inst);
            match is_goal_satisfiable(self.db, ingot, canonical_inst.value, assumptions) {
                GoalSatisfiability::NeedsConfirmation(ambiguous) => {
                    let insts = ambiguous
                        .iter()
                        .map(|solution| canonical_inst.extract_solution(prober.table, *solution))
                        .collect();

                    if !inst.self_ty(self.db).has_var(self.db) {
                        let diag = BodyDiag::ambiguous_trait_inst(self.db, span.clone(), insts);
                        sink.push(diag.into())
                    }
                }

                _ => {
                    // WF is checked by `TyCheckerFinalizer`
                }
            }
        }
    }
}

pub(super) struct BlockEnv<'db> {
    pub(super) scope: ScopeId<'db>,
    pub(super) vars: FxHashMap<IdentId<'db>, LocalBinding<'db>>,
    idx: usize,
}

impl<'db> BlockEnv<'db> {
    pub(super) fn lookup_var(&self, var: IdentId<'db>) -> Option<LocalBinding<'db>> {
        self.vars.get(&var).copied()
    }

    fn new(scope: ScopeId<'db>, idx: usize) -> Self {
        Self {
            scope,
            vars: FxHashMap::default(),
            idx,
        }
    }

    fn register_var(&mut self, name: IdentId<'db>, var: LocalBinding<'db>) {
        self.vars.insert(name, var);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprProp<'db> {
    pub ty: TyId<'db>,
    pub is_mut: bool,
    pub(crate) binding: Option<LocalBinding<'db>>,
}

impl<'db> ExprProp<'db> {
    pub(super) fn new(ty: TyId<'db>, is_mut: bool) -> Self {
        Self {
            ty,
            is_mut,
            binding: None,
        }
    }

    pub(super) fn new_binding_ref(ty: TyId<'db>, is_mut: bool, binding: LocalBinding<'db>) -> Self {
        Self {
            ty,
            is_mut,
            binding: Some(binding),
        }
    }

    pub(super) fn binding(&self) -> Option<LocalBinding<'db>> {
        self.binding
    }

    pub(super) fn swap_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
        std::mem::replace(&mut self.ty, ty)
    }

    pub(super) fn invalid(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            ty: TyId::invalid(db, InvalidCause::Other),
            is_mut: true,
            binding: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LocalBinding<'db> {
    Local {
        pat: PatId,
        is_mut: bool,
    },
    Param {
        idx: usize,
        ty: TyId<'db>,
        is_mut: bool,
    },
}

impl<'db> LocalBinding<'db> {
    pub(super) fn local(pat: PatId, is_mut: bool) -> Self {
        Self::Local { pat, is_mut }
    }

    pub(super) fn is_mut(&self) -> bool {
        match self {
            LocalBinding::Local { is_mut, .. } | LocalBinding::Param { is_mut, .. } => *is_mut,
        }
    }

    pub(super) fn binding_name(&self, env: &TyCheckEnv<'db>) -> IdentId<'db> {
        let hir_db = env.db.as_hir_db();
        match self {
            Self::Local { pat, .. } => {
                let Partial::Present(Pat::Path(Partial::Present(path), ..)) =
                    pat.data(hir_db, env.body())
                else {
                    unreachable!();
                };
                path.ident(hir_db).to_opt().unwrap()
            }

            Self::Param { idx, .. } => {
                let func = env.func().unwrap();
                let Partial::Present(func_params) =
                    func.hir_func_def(env.db).unwrap().params(hir_db)
                else {
                    unreachable!();
                };

                func_params.data(hir_db)[*idx].name().unwrap()
            }
        }
    }

    fn def_span(&self, env: &TyCheckEnv<'db>) -> DynLazySpan<'db> {
        match self {
            LocalBinding::Local { pat, .. } => pat.lazy_span(env.body).into(),
            LocalBinding::Param { idx, .. } => {
                let hir_func = env.func().unwrap().hir_func_def(env.db).unwrap();
                hir_func
                    .lazy_span()
                    .params_moved()
                    .param(*idx)
                    .name_moved()
                    .into()
            }
        }
    }
}

struct Prober<'db, 'a> {
    table: &'a mut UnificationTable<'db>,
}

impl<'db> TyFolder<'db> for Prober<'db, '_> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.table.db()
    }

    fn fold_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
        let ty = self.table.fold_ty(ty);
        let TyData::TyVar(var) = ty.data(self.db()) else {
            return ty.super_fold_with(self);
        };

        // String type variable fallback.
        if let TyVarSort::String(len) = var.sort {
            let ty = TyId::new(self.db(), TyData::TyBase(PrimTy::String.into()));
            let len = EvaluatedConstTy::LitInt(IntegerId::new(self.db().as_hir_db(), len.into()));
            let len =
                ConstTyData::Evaluated(len, ty.applicable_ty(self.db()).unwrap().const_ty.unwrap());
            let len = TyId::const_ty(self.db(), ConstTyId::new(self.db(), len));
            TyId::app(self.db(), ty, len)
        } else {
            ty.super_fold_with(self)
        }
    }
}
