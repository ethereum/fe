//! Builtin callable-shape implementations for closure types.
//!
//! A closure implements the core callable shape traits of its parameter
//! modes, `Fn_<modes>` and `FnMut_<modes>` (`Fn0`/`FnMut0` without
//! parameters, `Fn<T, U>` for `Fn(own T)`), without HIR `impl` items: `Out`
//! is its result and `E` the effects its body uses. Its body only reads its
//! captures, so the `FnMut` implementation is the `Fn` one. The trait solver,
//! normalizer, method selection and callee resolution derive the
//! relationship from here.

use common::indexmap::IndexMap;
use cranelift_entity::EntityRef;
use salsa::Update;

use super::{
    corelib::resolve_core_trait,
    trait_def::{ImplementorId, ImplementorOrigin, TraitInstId},
    ty_check::{BodyOwner, EffectParamSite, infer_body},
    ty_def::{ClosureTy, TyId},
};
use crate::{
    analysis::HirAnalysisDb,
    core::semantic::EffectRequirement,
    hir_def::{
        Body, ClosureDef, Cond, CondId, Expr, ExprId, Func, IdentId, Partial, Stmt, StmtId, Trait,
        params::{FuncParamMode, callable_shape_name},
        scope_graph::ScopeId,
    },
};

/// A core callable shape trait: whether its receiver is `mut self` and its
/// parameters' modes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallableShape {
    pub(crate) mut_receiver: bool,
    pub(crate) modes: Vec<FuncParamMode>,
}

impl CallableShape {
    /// The shape `trait_` is, if it is a core callable shape trait.
    pub(crate) fn of<'db>(db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> Option<Self> {
        let name = trait_.name(db).to_opt()?.data(db);
        let (mut_receiver, rest) = match name.strip_prefix("FnMut") {
            Some(rest) => (true, rest),
            None => (false, name.strip_prefix("Fn")?),
        };
        let modes = match rest {
            "" => vec![FuncParamMode::Own],
            "0" => Vec::new(),
            letters => letters
                .strip_prefix('_')?
                .chars()
                .map(|letter| match letter {
                    'o' => Some(FuncParamMode::Own),
                    'v' => Some(FuncParamMode::View),
                    'm' => Some(FuncParamMode::Mut),
                    _ => None,
                })
                .collect::<Option<_>>()?,
        };
        let shape = Self {
            mut_receiver,
            modes,
        };
        (shape.trait_(db, trait_.scope()) == Some(trait_)).then_some(shape)
    }

    fn trait_<'db>(&self, db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Option<Trait<'db>> {
        let name = callable_shape_name(self.mut_receiver, &self.modes);
        resolve_core_trait(db, scope, &["functional", &name])
    }

    /// Whether the result is the trait's last argument (`Fn<T, U>`) rather
    /// than its associated `Out`.
    pub(crate) fn result_is_arg(&self) -> bool {
        !self.mut_receiver && self.modes == [FuncParamMode::Own]
    }
}

/// The instance of `trait_` that `closure` implements, if `trait_` is the
/// callable shape trait of the closure's parameter modes.
pub(crate) fn closure_shape_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    closure: ClosureTy<'db>,
    trait_: Trait<'db>,
) -> Option<TraitInstId<'db>> {
    let shape = CallableShape::of(db, trait_)?;
    if shape.modes != closure.modes(db) {
        return None;
    }
    let mut args = vec![TyId::closure(db, closure)];
    args.extend(closure.param_tys(db));
    let mut bindings = IndexMap::new();
    if shape.result_is_arg() {
        args.push(closure.ret_ty(db));
    } else {
        bindings.insert(out_ident(db), closure.ret_ty(db));
    }
    Some(TraitInstId::new(db, trait_, args, bindings))
}

/// Both callable shape instances `closure` implements, `Fn` and `FnMut`.
pub(crate) fn closure_shape_insts<'db>(
    db: &'db dyn HirAnalysisDb,
    closure: ClosureTy<'db>,
) -> impl Iterator<Item = TraitInstId<'db>> {
    [false, true].into_iter().filter_map(move |mut_receiver| {
        let shape = CallableShape {
            mut_receiver,
            modes: closure.modes(db),
        };
        let trait_ = shape.trait_(db, closure.def(db).body.scope())?;
        closure_shape_inst(db, closure, trait_)
    })
}

/// The builtin implementor of `goal` when its self type is a closure that
/// implements its trait.
pub(crate) fn closure_implementor<'db>(
    db: &'db dyn HirAnalysisDb,
    goal: TraitInstId<'db>,
) -> Option<ImplementorId<'db>> {
    let closure = goal.self_ty(db).as_closure(db)?;
    let inst = closure_shape_inst(db, closure, goal.def(db))?;
    let types = inst
        .assoc_type_bindings(db)
        .iter()
        .map(|(name, ty)| (*name, *ty))
        .collect::<IndexMap<_, _>>();
    Some(ImplementorId::new(
        db,
        TraitInstId::new_simple(db, inst.def(db), inst.args(db).clone()),
        Vec::new(),
        types,
        ImplementorOrigin::Closure,
    ))
}

/// `Out` of the callable shape trait `trait_` for the closure `self_ty`.
pub(crate) fn closure_out_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    self_ty: TyId<'db>,
    trait_: Trait<'db>,
    name: IdentId<'db>,
) -> Option<TyId<'db>> {
    let closure = self_ty.as_closure(db)?;
    let shape = CallableShape::of(db, trait_)?;
    (name == out_ident(db) && !shape.result_is_arg() && shape.modes == closure.modes(db))
        .then(|| closure.ret_ty(db))
}

pub(crate) fn out_ident<'db>(db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
    IdentId::new(db, "Out".to_string())
}

/// The innermost closure whose body each expression and statement of a body
/// is part of; `None` for the item body itself. A closure expression is part
/// of the region it appears in.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub(crate) struct ClosureRegions {
    exprs: Vec<Option<ExprId>>,
    stmts: Vec<Option<ExprId>>,
}

impl ClosureRegions {
    pub(crate) fn expr(&self, expr: ExprId) -> Option<ExprId> {
        self.exprs[expr.index()]
    }

    pub(crate) fn stmt(&self, stmt: StmtId) -> Option<ExprId> {
        self.stmts[stmt.index()]
    }
}

#[salsa::tracked(return_ref)]
pub(crate) fn closure_regions<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> ClosureRegions {
    let mut walk = RegionWalk {
        db,
        body,
        regions: ClosureRegions {
            exprs: vec![None; body.exprs(db).len()],
            stmts: vec![None; body.stmts(db).len()],
        },
    };
    walk.expr(body.expr(db), None);
    walk.regions
}

struct RegionWalk<'db> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    regions: ClosureRegions,
}

impl<'db> RegionWalk<'db> {
    fn expr(&mut self, expr: ExprId, region: Option<ExprId>) {
        self.regions.exprs[expr.index()] = region;
        let Partial::Present(data) = expr.data(self.db, self.body) else {
            return;
        };
        match data {
            Expr::Closure { body, .. } => self.expr(*body, Some(expr)),
            Expr::Lit(_) | Expr::Path(_) | Expr::UnsupportedMacroCall => {}
            Expr::Block(stmts, _) => stmts.iter().for_each(|&stmt| self.stmt(stmt, region)),
            Expr::Bin(lhs, rhs, _) | Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
                self.expr(*lhs, region);
                self.expr(*rhs, region);
            }
            Expr::Un(inner, _)
            | Expr::Cast(inner, _)
            | Expr::Try(inner)
            | Expr::Field(inner, _)
            | Expr::ArrayRep(inner, _) => self.expr(*inner, region),
            Expr::Call(callee, args) => {
                self.expr(*callee, region);
                args.iter().for_each(|arg| self.expr(arg.expr, region));
            }
            Expr::MethodCall(receiver, _, _, args) => {
                self.expr(*receiver, region);
                args.iter().for_each(|arg| self.expr(arg.expr, region));
            }
            Expr::Assert(args) => args.iter().for_each(|arg| self.expr(arg.expr, region)),
            Expr::RecordInit(_, fields) => fields
                .iter()
                .for_each(|field| self.expr(field.expr, region)),
            Expr::Tuple(elems) | Expr::Array(elems) => {
                elems.iter().for_each(|&elem| self.expr(elem, region))
            }
            Expr::If(cond, then, else_) => {
                self.cond(*cond, region);
                self.expr(*then, region);
                if let Some(else_) = else_ {
                    self.expr(*else_, region);
                }
            }
            Expr::Match(scrutinee, arms) => {
                self.expr(*scrutinee, region);
                if let Partial::Present(arms) = arms {
                    arms.iter().for_each(|arm| self.expr(arm.body, region));
                }
            }
            Expr::With(bindings, body) => {
                bindings
                    .iter()
                    .for_each(|binding| self.expr(binding.value, region));
                self.expr(*body, region);
            }
        }
    }

    fn stmt(&mut self, stmt: StmtId, region: Option<ExprId>) {
        self.regions.stmts[stmt.index()] = region;
        let Partial::Present(data) = stmt.data(self.db, self.body) else {
            return;
        };
        match data {
            Stmt::Let(_, _, init, else_) => {
                for expr in init.iter().chain(else_) {
                    self.expr(*expr, region);
                }
            }
            Stmt::For(_, iterable, driver, body, _) => {
                for expr in [Some(iterable), driver.as_ref(), Some(body)]
                    .into_iter()
                    .flatten()
                {
                    self.expr(*expr, region);
                }
            }
            Stmt::While(cond, body) => {
                self.cond(*cond, region);
                self.expr(*body, region);
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.expr(*expr, region);
                }
            }
            Stmt::Yield(expr) | Stmt::Expr(expr) => self.expr(*expr, region),
            Stmt::Continue | Stmt::Break => {}
        }
    }

    fn cond(&mut self, cond: CondId, region: Option<ExprId>) {
        let Partial::Present(data) = cond.data(self.db, self.body) else {
            return;
        };
        match data {
            Cond::Expr(expr) | Cond::Let(_, expr) => self.expr(*expr, region),
            Cond::Bin(lhs, rhs, _) => {
                self.cond(*lhs, region);
                self.cond(*rhs, region);
            }
        }
    }
}

/// The closure whose body a call of `func`, a method of the callable shape
/// trait instance `inst`, runs, as typed in its parent's generic body, with
/// the receiver mode of that body.
pub(crate) fn callee_closure<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    inst: TraitInstId<'db>,
) -> Option<(ClosureTy<'db>, FuncParamMode)> {
    let closure = inst.self_ty(db).as_closure(db)?;
    let shape = CallableShape::of(db, inst.def(db))?;
    if func.name(db).to_opt()?.data(db) != "call" {
        return None;
    }
    let receiver = if shape.mut_receiver {
        FuncParamMode::Mut
    } else {
        FuncParamMode::View
    };
    Some((closure, receiver))
}

/// `closure` as its parent's typed body has it, generic over the parent's
/// parameters: the type a closure body's owner names, whose instances take
/// the parent's arguments from the closure type they are called on.
pub(crate) fn closure_template_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    closure: ClosureTy<'db>,
) -> ClosureTy<'db> {
    parent_closure_ty(db, closure.def(db)).unwrap_or(closure)
}

fn parent_closure_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    def: ClosureDef<'db>,
) -> Option<ClosureTy<'db>> {
    let parent = BodyOwner::from_body(db, def.body)?;
    infer_body(db, parent)
        .1
        .expr_ty(db, def.expr)
        .as_closure(db)
}

/// The effect requirements of the body of the closure at `def`: its row,
/// numbered in order.
pub(crate) fn closure_effect_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    def: ClosureDef<'db>,
) -> Vec<EffectRequirement<'db>> {
    let Some(closure) = parent_closure_ty(db, def) else {
        return Vec::new();
    };
    closure
        .effects(db)
        .iter()
        .enumerate()
        .map(|(idx, component)| EffectRequirement {
            binding_name: component.name,
            key: component.key.clone(),
            is_mut: component.is_mut,
            binding_site: EffectParamSite::Closure(def),
            binding_idx: idx as u32,
            binding_ty: component.key_syntax,
        })
        .collect()
}
