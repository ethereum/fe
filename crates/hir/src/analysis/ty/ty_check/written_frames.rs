//! The trait slots opened where a constant body is written. "Opened"
//! means a slot any frame answers there: a `with` block, a named implementation's own slot, and later a module frame.
//!
//! A constant written in a body (a constant generic argument, an array
//! length) is lowered as its own anonymous body, and lowering evaluates it
//! with the ordinary implementation. Before it does, evaluation asks which
//! slots the frames around the body open (the `with` blocks around it, the
//! bindings before it in its own header, a named implementation's own slot)
//! and refuses a read of one: the constant would mean one thing in the type
//! and another in the code around it. This reads only syntax, signatures and
//! name resolution, so it adds no cycle through the check of the enclosing
//! body.

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{PathRes, resolve_path},
        ty::{
            corelib::evidence_goal,
            trait_def::{TraitInstId, named_impls_for_trait},
            trait_resolution::PredicateListId,
        },
    },
    hir_def::{
        Body, BodyKind, Expr, ExprId, GenericArg, GenericArgListId, ItemKind, Partial, PatId,
        PathId, PathKind, TypeId as HirTyId, TypeKind as HirTyKind, scope_graph::ScopeId,
    },
    visitor::{Visitor, VisitorCtxt, walk_body, walk_expr},
};

/// The slots opened where a constant body is written, keyed as frames key
/// them (`SlotKey`), in the enclosing body's scope.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, salsa::Update)]
pub(crate) struct WrittenFrames<'db> {
    slots: Vec<TraitInstId<'db>>,
    /// A `with (e)` around the body whose evidence type is not written down:
    /// any slot with a named implementation may be open.
    unknown: bool,
    /// Where the keys were normalized.
    context: Option<(ScopeId<'db>, PredicateListId<'db>)>,
    /// The body is written in a named implementation's declaration (a
    /// method signature, an effect clause, an associated type or row), not
    /// in a body: no check of a body reports its refusal, so the read itself
    /// gives the diagnostic.
    declaration: bool,
}

impl<'db> WrittenFrames<'db> {
    /// The slots the frames around the body open.
    pub(crate) fn slots(&self) -> &[TraitInstId<'db>] {
        &self.slots
    }

    /// Whether a frame around the body opens a slot not written down.
    pub(crate) fn unknown(&self) -> bool {
        self.unknown
    }

    /// Whether the body is written in a named implementation's declaration.
    pub(crate) fn in_declaration(&self) -> bool {
        self.declaration
    }

    /// Whether no frame opens anything here (`has_frames` for written
    /// constants).
    pub(crate) fn is_empty(&self) -> bool {
        self.slots.is_empty() && !self.unknown
    }

    /// Whether a read of `slot` here could see a choice a frame opened.
    pub(crate) fn opens(&self, db: &'db dyn HirAnalysisDb, slot: TraitInstId<'db>) -> bool {
        let Some((scope, assumptions)) = self.context else {
            return false;
        };
        let key = super::choice::slot_key(db, slot, scope, assumptions).inst();
        if self.slots.contains(&key) {
            return true;
        }
        self.unknown && has_named_implementation(db, scope, key)
    }
}

/// Whether `slot`'s trait has a named implementation where `scope` can see
/// one: only such a slot can be opened.
pub(crate) fn has_named_implementation<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    slot: TraitInstId<'db>,
) -> bool {
    let trait_ = slot.def(db);
    let ingot = scope.top_mod(db).ingot(db);
    let (primary, secondary) = crate::analysis::ty::trait_resolution::TraitSolveCx::search_ingots_for_trait_inst_with_origin(
        db, ingot, slot,
    );
    [Some(ingot), Some(primary), secondary]
        .into_iter()
        .flatten()
        .any(|search_ingot| !named_impls_for_trait(db, search_ingot, trait_).is_empty())
}

/// Where in its function's body a constant body is written.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, salsa::Update)]
enum Place {
    /// Inside this expression's own syntax.
    Expr(ExprId),
    /// In the key of this `with` expression's binding.
    WithKey(ExprId, usize),
}

/// The slots opened where `body` is written. A body not written inside a
/// function's body (a signature, an item) has none.
#[salsa::tracked(return_ref)]
pub(crate) fn written_frames<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> WrittenFrames<'db> {
    let Some((func_body, outermost)) = enclosing_func_body(db, body) else {
        return declaration_frames(db, body).unwrap_or_default();
    };
    let layout = body_layout(db, func_body);
    let Some(place) = layout
        .places
        .iter()
        .find_map(|(written, place)| (*written == outermost).then_some(*place))
    else {
        return WrittenFrames::default();
    };
    layout.frames_at(db, func_body, place)
}

/// The frames of a constant body written in a named implementation's
/// declaration: inside `impl T for S as N`, the slot `T<S>` is N, so
/// the declaration reads that slot's constants as an opened slot's.
fn declaration_frames<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> Option<WrittenFrames<'db>> {
    let mut bodies = vec![body];
    let mut at = ScopeId::from_item(ItemKind::Body(body)).parent(db);
    while let Some(scope) = at {
        match scope {
            ScopeId::Item(ItemKind::ImplTrait(implementation)) => {
                implementation.selection_name(db)?;
                // A `where` condition reads the environment where the
                // implementation is declared, not its own slot.
                let conditions = crate::hir_def::WhereClauseOwner::ImplTrait(implementation)
                    .where_clause(db)
                    .const_predicates(db);
                if bodies.iter().any(|body| conditions.contains(body)) {
                    return None;
                }
                let inst = implementation.trait_inst(db)?;
                let scope = implementation.scope();
                let assumptions = PredicateListId::empty_list(db);
                let key = super::choice::slot_key(db, inst, scope, assumptions).inst();
                return Some(WrittenFrames {
                    slots: vec![key],
                    unknown: false,
                    context: Some((scope, assumptions)),
                    declaration: true,
                });
            }
            ScopeId::Item(ItemKind::Body(inner)) => {
                bodies.push(inner);
                at = scope.parent(db);
            }
            ScopeId::Item(ItemKind::Func(_)) | ScopeId::Block(..) => {
                at = scope.parent(db);
            }
            _ => return None,
        }
    }
    None
}

/// The frames of a path written at `scope` in a named implementation's
/// declaration (a method signature, an effect clause, an associated type or
/// row), outside any body: inside `impl T for S as N`, the slot `T<S>` is N
/// there. A path inside a body is read by that body's check instead.
pub(crate) fn declaration_frames_at<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> Option<WrittenFrames<'db>> {
    let mut at = Some(scope);
    while let Some(scope) = at {
        match scope {
            ScopeId::Item(ItemKind::ImplTrait(implementation)) => {
                implementation.selection_name(db)?;
                let inst = implementation.trait_inst(db)?;
                let scope = implementation.scope();
                let assumptions = PredicateListId::empty_list(db);
                let key = super::choice::slot_key(db, inst, scope, assumptions).inst();
                return Some(WrittenFrames {
                    slots: vec![key],
                    unknown: false,
                    context: Some((scope, assumptions)),
                    declaration: true,
                });
            }
            ScopeId::Block(..) | ScopeId::Item(ItemKind::Body(_)) => return None,
            ScopeId::Item(ItemKind::Mod(_) | ItemKind::TopMod(_)) => return None,
            _ => at = scope.parent(db),
        }
    }
    None
}

/// The function-level body `body` is written in, and the constant body
/// directly inside it that contains `body` (itself, unless nested).
fn enclosing_func_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> Option<(Body<'db>, Body<'db>)> {
    let mut outermost = body;
    let mut at = ScopeId::from_item(ItemKind::Body(body)).parent(db);
    while let Some(scope) = at {
        match scope {
            ScopeId::Block(enclosing, _) if enclosing.body_kind(db) == BodyKind::FuncBody => {
                return Some((enclosing, outermost));
            }
            ScopeId::Block(enclosing, _) => {
                outermost = enclosing;
                at = scope.parent(db);
            }
            ScopeId::Item(ItemKind::Body(enclosing)) => {
                if enclosing.body_kind(db) == BodyKind::FuncBody {
                    return Some((enclosing, outermost));
                }
                outermost = enclosing;
                at = scope.parent(db);
            }
            // A `const` item written in a block: its body is placed where the
            // item is (`body_layout`).
            ScopeId::Item(ItemKind::Const(_)) => {
                at = scope.parent(db);
            }
            _ => return None,
        }
    }
    None
}

/// Every constant body written directly in `func_body`, in any position:
/// the constant arguments and array lengths of its paths, types, patterns
/// and `with` keys.
pub(crate) fn written_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    func_body: Body<'db>,
) -> Vec<Body<'db>> {
    body_layout(db, func_body)
        .places
        .iter()
        .map(|(body, _)| *body)
        .collect()
}

/// For one function body: where each constant body and each item other than
/// a `const` item directly in it is written, and each expression's parent.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, salsa::Update)]
struct BodyLayout<'db> {
    places: Vec<(Body<'db>, Place)>,
    items: Vec<(ItemKind<'db>, Place)>,
    parents: Vec<(ExprId, ExprId)>,
}

#[salsa::tracked(return_ref)]
fn body_layout<'db>(db: &'db dyn HirAnalysisDb, func_body: Body<'db>) -> BodyLayout<'db> {
    struct Collect<'db> {
        db: &'db dyn HirAnalysisDb,
        exprs: Vec<ExprId>,
        layout: BodyLayout<'db>,
    }
    impl<'db> Visitor<'db> for Collect<'db> {
        fn visit_expr(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, crate::visitor::prelude::LazyExprSpan<'db>>,
            expr: ExprId,
            expr_data: &Expr<'db>,
        ) {
            if let Some(parent) = self.exprs.last() {
                self.layout.parents.push((expr, *parent));
            }
            // The visitor does not walk a header's keys; their constant
            // bodies are placed here, with their binding.
            if let Expr::With(bindings, _) = expr_data {
                for (index, binding) in bindings.iter().enumerate() {
                    if let Some(Partial::Present(key)) = binding.key_path {
                        let mut bodies = Vec::new();
                        path_bodies(self.db, key, &mut bodies);
                        self.layout.places.extend(
                            bodies
                                .into_iter()
                                .map(|body| (body, Place::WithKey(expr, index))),
                        );
                    }
                }
            }
            self.exprs.push(expr);
            walk_expr(self, ctxt, expr);
            self.exprs.pop();
        }

        fn visit_body(
            &mut self,
            _: &mut VisitorCtxt<'db, crate::visitor::prelude::LazyBodySpan<'db>>,
            body: Body<'db>,
        ) {
            // A constant body inside an expression's syntax; its own place
            // is that expression's, so it is not walked into.
            if let Some(at) = self.exprs.last() {
                self.layout.places.push((body, Place::Expr(*at)));
            }
        }

        fn visit_item(
            &mut self,
            _: &mut VisitorCtxt<'db, crate::visitor::prelude::LazyItemSpan<'db>>,
            item: ItemKind<'db>,
        ) {
            // A `const` item written inside the body reads the frames written
            // around it in its value (refused where it reads a slot one
            // answers); its declared type, and any other item,
            // is refused where it reads one (`refused_items`). Items are not
            // walked.
            let Some(at) = self.exprs.last() else {
                return;
            };
            match item {
                ItemKind::Const(constant) => {
                    if let Some(body) = constant.body(self.db).to_opt() {
                        self.layout.places.push((body, Place::Expr(*at)));
                    }
                    self.layout.items.push((item, Place::Expr(*at)));
                }
                ItemKind::Use(_) | ItemKind::Body(_) => {}
                _ => self.layout.items.push((item, Place::Expr(*at))),
            }
        }
    }
    let mut collect = Collect {
        db,
        exprs: Vec::new(),
        layout: BodyLayout::default(),
    };
    walk_body(
        &mut collect,
        &mut VisitorCtxt::with_body(db, func_body),
        func_body,
    );
    collect.layout
}

impl<'db> BodyLayout<'db> {
    fn parent(&self, expr: ExprId) -> Option<ExprId> {
        self.parents
            .iter()
            .find_map(|(child, parent)| (*child == expr).then_some(*parent))
    }

    /// The frames open at `place`: a named implementation's own slot, and
    /// for each `with` around it the openings in force there (all of them
    /// in its block; those before it in its header).
    fn frames_at(
        &self,
        db: &'db dyn HirAnalysisDb,
        func_body: Body<'db>,
        place: Place,
    ) -> WrittenFrames<'db> {
        let scope = func_body.scope();
        let assumptions = crate::analysis::ty::const_ty::assumptions_for_body(db, func_body);
        let mut frames = WrittenFrames {
            slots: Vec::new(),
            unknown: false,
            context: Some((scope, assumptions)),
            declaration: false,
        };
        let open = |inst: TraitInstId<'db>, frames: &mut WrittenFrames<'db>| {
            let key = super::choice::slot_key(db, inst, scope, assumptions).inst();
            if !frames.slots.contains(&key) {
                frames.slots.push(key);
            }
        };
        // Inside `impl T for S as N`, the slot `T<S>` is N for the body.
        let mut item = scope.parent_item(db);
        while let Some(ItemKind::Func(func)) = item {
            item = func.scope().parent_item(db);
        }
        if let Some(ItemKind::ImplTrait(implementation)) = item
            && implementation.selection_name(db).is_some()
            && let Some(inst) = implementation.trait_inst(db)
        {
            open(inst, &mut frames);
        }
        let (mut child, header_limit) = match place {
            Place::Expr(expr) => (expr, None),
            Place::WithKey(with, index) => (with, Some(index)),
        };
        let mut at = match header_limit {
            Some(_) => Some(child),
            None => self.parent(child),
        };
        let mut limit = header_limit;
        while let Some(expr) = at {
            if let Partial::Present(Expr::With(bindings, block)) = expr.data(db, func_body) {
                let count = match limit {
                    Some(index) => index,
                    None if child == *block => bindings.len(),
                    None => bindings
                        .iter()
                        .position(|binding| binding.value == child)
                        .unwrap_or(0),
                };
                for binding in &bindings[..count] {
                    if binding.key_path.is_some() {
                        continue;
                    }
                    match opened_by(db, func_body, binding.value) {
                        Some(insts) => insts.into_iter().for_each(|inst| open(inst, &mut frames)),
                        None => frames.unknown = true,
                    }
                }
            }
            limit = None;
            child = expr;
            at = self.parent(expr);
        }
        frames
    }
}

/// The items written in `func_body` that read a slot a frame around them
/// answers, each with the first such slot. An item is its own owner: the
/// frames around it do not reach it, and it reads the ordinary
/// implementation, so it is refused as a `const` item is, until items can
/// read the frames around them.
pub(crate) fn refused_items<'db>(
    db: &'db dyn HirAnalysisDb,
    func_body: Body<'db>,
) -> Vec<(ItemKind<'db>, TraitInstId<'db>)> {
    let layout = body_layout(db, func_body);
    let mut refused = Vec::new();
    for (item, place) in &layout.items {
        let frames = layout.frames_at(db, func_body, *place);
        if frames.is_empty() {
            continue;
        }
        if let Some(slot) = item_slot_reads(db, *item)
            .into_iter()
            .find(|slot| frames.opens(db, *slot))
        {
            refused.push((*item, slot));
        }
    }
    refused
}

/// Every slot `item` reads the ordinary implementation of, at any depth: the
/// trait uses its function bodies and constant bodies record, and the
/// constants its written types name (unbraced ones included), as a
/// constant's read-time refusal reads them. A path's generic arguments are
/// types or constant bodies, so paths outside types (trait references,
/// bounds) are read through them. A `const` item's value is refused by its
/// own check (`checked_body`), so only its declared type is read here.
fn item_slot_reads<'db>(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> Vec<TraitInstId<'db>> {
    use crate::{
        analysis::ty::ty_lower::hir_ty_const_slot_reads,
        visitor::{
            prelude::{LazyBodySpan, LazyTySpan},
            walk_item, walk_type,
        },
    };
    struct Reads<'db> {
        db: &'db dyn HirAnalysisDb,
        /// A `const` item's value, which its own check refuses.
        value: Option<Body<'db>>,
        reads: Vec<TraitInstId<'db>>,
    }
    impl<'db> Visitor<'db> for Reads<'db> {
        fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>, ty: HirTyId<'db>) {
            let assumptions = PredicateListId::empty_list(self.db);
            self.reads.extend(hir_ty_const_slot_reads(
                self.db,
                ty,
                ctxt.scope(),
                assumptions,
            ));
            walk_type(self, ctxt, ty);
        }

        fn visit_body(&mut self, ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, body: Body<'db>) {
            if self.value == Some(body) {
                return;
            }
            let db = self.db;
            let uses = match ScopeId::from_item(ItemKind::Body(body)).parent_item(db) {
                Some(ItemKind::Func(func)) if body.body_kind(db) == BodyKind::FuncBody => {
                    let typed = &super::infer_body(db, super::BodyOwner::func(db, func)).1;
                    super::choice::body_slot_uses(db, typed)
                }
                _ => written_body_uses(db, body, None),
            };
            // A use a frame inside the item decided does not read the
            // ordinary implementation.
            self.reads.extend(
                uses.into_iter()
                    .filter(|(_, choice)| {
                        !matches!(
                            choice,
                            Some(crate::analysis::ty::trait_choice::Choice::Known(known))
                                if !known.is_ordinary()
                        )
                    })
                    .map(|(goal, _)| goal),
            );
            walk_body(self, ctxt, body);
        }
    }
    let mut reads = Reads {
        db,
        value: match item {
            ItemKind::Const(constant) => constant.body(db).to_opt(),
            _ => None,
        },
        reads: Vec::new(),
    };
    walk_item(&mut reads, &mut VisitorCtxt::with_item(db, item), item);
    reads.reads
}

/// The slots a `with` binding's value opens, read from what is written:
/// a named implementation's name, a local, parameter or constant item with
/// an evidence type. `None` when the slot is not written down.
fn opened_by<'db>(
    db: &'db dyn HirAnalysisDb,
    func_body: Body<'db>,
    value: ExprId,
) -> Option<Vec<TraitInstId<'db>>> {
    let Partial::Present(Expr::Path(Partial::Present(path))) = value.data(db, func_body) else {
        return None;
    };
    let lex = super::env::body_lexical_env(db, func_body);
    let body_scope = func_body.scope();
    let expr_scope = lex.expr_scope(value).unwrap_or(body_scope);
    let assumptions = crate::analysis::ty::const_ty::assumptions_for_body(db, func_body);
    use crate::analysis::name_resolution::path_resolver::{ImplNameRes, resolve_impl_name};
    match resolve_impl_name(db, *path, expr_scope) {
        ImplNameRes::Found(implementation) => {
            if path.is_bare_ident(db) && lex.expr_binding(value).is_some() {
                return Some(Vec::new());
            }
            return Some(implementation.trait_inst(db).into_iter().collect());
        }
        ImplNameRes::NotVisible | ImplNameRes::Ambiguous => return Some(Vec::new()),
        ImplNameRes::NotFound => {}
    }
    if path.is_bare_ident(db) {
        use super::env::LexicalBinding;
        let ty = match lex.expr_binding(value)? {
            LexicalBinding::Local { pat } => {
                return local_evidence_goal(db, func_body, lex, pat).map(|goal| vec![goal]);
            }
            LexicalBinding::Param { idx } => match super::BodyOwner::from_body(db, func_body)? {
                super::BodyOwner::Func(func, _) => *func.arg_tys(db).get(idx)?.skip_binder(),
                super::BodyOwner::ContractInit { contract } => {
                    let param = contract.init(db)?.params(db).data(db).get(idx)?;
                    lower_written_hir_ty(db, param.ty, contract.scope(), assumptions)?
                }
                _ => return None,
            },
            LexicalBinding::ClosureParam { closure, idx } => {
                let Partial::Present(Expr::Closure { params, .. }) = closure.data(db, func_body)
                else {
                    return None;
                };
                let param = params.data(db).get(idx)?;
                let closure_scope = lex.expr_scope(closure).unwrap_or(expr_scope);
                lower_written_hir_ty(db, param.ty, closure_scope, assumptions)?
            }
            LexicalBinding::EffectParam => return None,
            LexicalBinding::Item(ScopeId::Item(ItemKind::Const(constant))) => {
                let mut bodies = Vec::new();
                ty_bodies(db, constant.hir_ty(db).into(), &mut bodies);
                if !bodies.is_empty() {
                    return None;
                }
                match resolve_path(db, *path, expr_scope, assumptions, true) {
                    Ok(PathRes::Const(_, ty)) => ty,
                    _ => return None,
                }
            }
            LexicalBinding::Item(_) => return None,
        };
        return evidence_goal(db, body_scope, ty).map(|goal| vec![goal]);
    }
    match resolve_path(db, *path, expr_scope, assumptions, true) {
        Ok(PathRes::Const(_, ty)) => evidence_goal(db, body_scope, ty).map(|goal| vec![goal]),
        _ => None,
    }
}

fn lower_written_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Partial<HirTyId<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<crate::analysis::ty::ty_def::TyId<'db>> {
    let Partial::Present(ty) = ty else {
        return None;
    };
    let mut bodies = Vec::new();
    ty_bodies(db, Partial::Present(ty), &mut bodies);
    bodies
        .is_empty()
        .then(|| crate::analysis::ty::ty_lower::lower_hir_ty(db, ty, scope, assumptions))
}

/// The goal of the evidence the local pattern `pat` holds: the
/// `Evidence<G>` its `let` annotation writes, or else the one its
/// initializer's callee returns (`prove<G>()`, or a function declared to
/// return it), read through blocks and `with` blocks. `None` when the
/// goal is not written down.
fn local_evidence_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    func_body: Body<'db>,
    lex: &super::env::BodyLexicalEnv<'db>,
    pat: PatId,
) -> Option<TraitInstId<'db>> {
    use crate::hir_def::Stmt;
    let let_binding = lex.let_binding(pat)?;
    let scope = func_body.scope();
    let assumptions = crate::analysis::ty::const_ty::assumptions_for_body(db, func_body);
    // A written constant in the type is not lowered here: its evaluation
    // reads these frames.
    if let Some(annotation) = let_binding.annotation {
        let ty = lower_written_hir_ty(
            db,
            Partial::Present(annotation),
            let_binding.init_scope,
            assumptions,
        )?;
        return evidence_goal(db, scope, ty);
    }
    let mut value = let_binding.init?;
    let callee = loop {
        match value.data(db, func_body) {
            Partial::Present(Expr::Block(stmts, _)) => {
                match stmts.last().map(|stmt| stmt.data(db, func_body)) {
                    Some(Partial::Present(Stmt::Expr(tail))) => value = *tail,
                    _ => return None,
                }
            }
            Partial::Present(Expr::With(_, block)) => value = *block,
            Partial::Present(Expr::Call(callee, _)) => break *callee,
            _ => return None,
        }
    };
    let Partial::Present(Expr::Path(Partial::Present(path))) = callee.data(db, func_body) else {
        return None;
    };
    let mut bodies = Vec::new();
    path_bodies(db, *path, &mut bodies);
    if !bodies.is_empty() {
        return None;
    }
    let callee_scope = lex.expr_scope(callee).unwrap_or(let_binding.init_scope);
    let Ok(PathRes::Func(func_ty)) = resolve_path(db, *path, callee_scope, assumptions, true)
    else {
        return None;
    };
    let (base, args) = func_ty.decompose_ty_app(db);
    let crate::analysis::ty::ty_def::TyData::TyBase(crate::analysis::ty::ty_def::TyBase::Func(
        crate::hir_def::CallableDef::Func(func),
    )) = base.data(db)
    else {
        return None;
    };
    // `prove<G>()` returns evidence for its written goal; a function with no
    // generic parameters returns the evidence its signature declares.
    if crate::analysis::ty::corelib::lib_func_matches(db, *func, "core::obligations::prove") {
        let goal = match path.generic_args(db).data(db).first() {
            Some(GenericArg::Type(arg)) => arg
                .ty
                .to_opt()
                .map(|ty| crate::analysis::ty::ty_lower::lower_hir_ty(db, ty, callee_scope, assumptions)),
            _ => None,
        };
        return match goal.map(|goal| goal.data(db)) {
            Some(crate::analysis::ty::ty_def::TyData::ConstraintTerm(goal)) => Some(*goal),
            _ => None,
        };
    }
    if !args.is_empty() {
        return None;
    }
    let ret = crate::hir_def::CallableDef::Func(*func)
        .ret_ty(db)
        .instantiate_identity();
    if ret.has_param(db) || ret.has_var(db) {
        return None;
    }
    evidence_goal(db, scope, ret)
}

/// The constant bodies written in a path's generic arguments, at any depth.
fn path_bodies<'db>(db: &'db dyn HirAnalysisDb, path: PathId<'db>, out: &mut Vec<Body<'db>>) {
    let mut segment = Some(path);
    while let Some(path) = segment {
        match path.kind(db) {
            PathKind::Ident { .. } => {}
            PathKind::QualifiedType { type_, trait_ } => {
                ty_bodies(db, Partial::Present(type_), out);
                if let Partial::Present(trait_path) = trait_.path(db) {
                    path_bodies(db, trait_path, out);
                }
            }
        }
        arg_bodies(db, path.generic_args(db), out);
        segment = path.parent(db);
    }
}

fn ty_bodies<'db>(db: &'db dyn HirAnalysisDb, ty: Partial<HirTyId<'db>>, out: &mut Vec<Body<'db>>) {
    let Partial::Present(ty) = ty else {
        return;
    };
    match ty.data(db) {
        HirTyKind::Ptr(inner) | HirTyKind::Mode(_, inner) => ty_bodies(db, *inner, out),
        HirTyKind::Tuple(tuple) => {
            for elem in tuple.data(db) {
                ty_bodies(db, *elem, out);
            }
        }
        HirTyKind::Array(elem, len) => {
            ty_bodies(db, *elem, out);
            if let Partial::Present(body) = len {
                out.push(*body);
            }
        }
        HirTyKind::Path(path) => {
            if let Partial::Present(path) = path {
                path_bodies(db, *path, out);
            }
        }
        HirTyKind::Never => {}
    }
}

fn arg_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    args: GenericArgListId<'db>,
    out: &mut Vec<Body<'db>>,
) {
    for arg in args.data(db) {
        match arg {
            GenericArg::Type(arg) => ty_bodies(db, arg.ty, out),
            GenericArg::Const(arg) => {
                if let Partial::Present(body) = arg.value {
                    out.push(body);
                }
            }
            GenericArg::AssocType(arg) => ty_bodies(db, arg.ty, out),
        }
    }
}

/// Whether a read of the trait constant `use_` reads a slot `frames` open:
/// its own slot, or one its constant body reads (`const_use_reads`), such as
/// a supertrait's constant read through `Self`.
pub(crate) fn opens_constant_read<'db>(
    db: &'db dyn HirAnalysisDb,
    frames: &WrittenFrames<'db>,
    use_: crate::analysis::ty::assoc_const::AssocConstUse<'db>,
) -> bool {
    // With no frame, nothing is opened; the constant's body is not read,
    // which would select the implementation an ordinary declaration is
    // still lowering (a cycle).
    if frames.is_empty() {
        return false;
    }
    frames.opens(db, use_.inst())
        || super::choice::const_use_reads(db, super::ConstRef::TraitConst(use_))
            .into_iter()
            .any(|(goal, _)| frames.opens(db, goal))
}

/// The slots a read of the constant `cref` reads, for the written-syntax
/// scan: a trait constant's own, and its constant body's.
pub(crate) fn constant_read_slots<'db>(
    db: &'db dyn HirAnalysisDb,
    cref: super::ConstRef<'db>,
) -> Vec<crate::analysis::ty::trait_def::TraitInstId<'db>> {
    let mut slots = Vec::new();
    for (goal, _) in super::choice::const_use_reads(db, cref) {
        if !slots.contains(&goal) {
            slots.push(goal);
        }
    }
    slots
}

/// The slot uses of `body`, a constant written in a function's body, checked
/// as the anonymous constant it is, of type `expected` (`usize` where the
/// position gives none): what the read-time refusal and type reach read.
pub(crate) fn written_body_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: Option<crate::analysis::ty::ty_def::TyId<'db>>,
) -> Vec<(
    crate::analysis::ty::trait_def::TraitInstId<'db>,
    Option<crate::analysis::ty::trait_choice::Choice<'db>>,
)> {
    use crate::analysis::ty::ty_def::{PrimTy, TyBase, TyData, TyId};
    let expected =
        expected.unwrap_or_else(|| TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Usize))));
    let typed = &super::infer_body(db, super::BodyOwner::AnonConstBody { body, expected }).1;
    super::choice::body_slot_uses(db, typed)
}

/// Whether the check of `body`, a constant written in a function's body,
/// refused a type it writes for reading a slot the frames around it open.
fn written_body_refuses_a_type<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: Option<crate::analysis::ty::ty_def::TyId<'db>>,
) -> bool {
    use crate::analysis::ty::{
        diagnostics::{BodyDiag, FuncBodyDiag},
        ty_def::{PrimTy, TyBase, TyData, TyId},
    };
    let expected =
        expected.unwrap_or_else(|| TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Usize))));
    super::infer_body(db, super::BodyOwner::AnonConstBody { body, expected })
        .0
        .iter()
        .any(|diag| {
            matches!(
                diag,
                FuncBodyDiag::Body(
                    BodyDiag::ChoiceDependentType { .. } | BodyDiag::ChoiceCarried { .. }
                )
            )
        })
}

/// Whether evaluating `body`, a constant written in a function's body, would
/// read a slot a frame around it opened. Evaluation refuses it: the
/// constant would read the ordinary implementation while the code around it
/// reads the choice.
pub(crate) fn refuses_read<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: Option<crate::analysis::ty::ty_def::TyId<'db>>,
) -> bool {
    let frames = written_frames(db, body);
    if frames.is_empty() {
        return false;
    }
    // A constant written inside this one that is refused makes this one
    // refused (`{ idn<{ T::P }>() }`): its value would be computed from a
    // read with no one meaning.
    if written_bodies(db, body)
        .into_iter()
        .any(|inner| refuses_read(db, inner, None))
    {
        return true;
    }
    // A type-level read written in the body (an unbraced constant path in a
    // type position, a constant argument) is refused by the body's own check
    // under those frames (`refuse_written_const_args`), which reports it.
    if written_body_refuses_a_type(db, body, expected) {
        return true;
    }
    // The body is checked with those slots unsupplied (`seed_written_frames`):
    // a use it decided that way, or one of an opened slot, is refused.
    written_body_uses(db, body, expected)
        .into_iter()
        .any(|(goal, choice)| {
            choice
                == Some(crate::analysis::ty::trait_choice::Choice::Slot(
                    crate::analysis::ty::trait_choice::SlotRef::Unsupplied,
                ))
                || frames.opens(db, goal)
        })
}

/// Whether `const_ty` is a constant refused where it is read: a trait
/// constant read unsupplied, or a constant body left unevaluated because a
/// frame around it opens a slot it reads. Such a constant stands for a
/// refusal the body reports (`unreported_choice_dependent_consts`), so it is
/// compared like an invalid type and adds no second diagnostic.
pub(crate) fn refused_at_read<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: crate::analysis::ty::const_ty::ConstTyId<'db>,
) -> bool {
    use crate::analysis::ty::{
        const_expr::ConstExpr,
        const_ty::ConstTyData,
        trait_choice::{Choice, SlotRef},
    };
    match const_ty.data(db) {
        ConstTyData::Abstract(expr, _) => matches!(
            expr.data(db),
            ConstExpr::TraitConst(assoc)
                if assoc.choice() == Some(Choice::Slot(SlotRef::Unsupplied))
        ),
        ConstTyData::UnEvaluated { body, ty, .. } => refuses_read(db, *body, *ty),
        _ => false,
    }
}
