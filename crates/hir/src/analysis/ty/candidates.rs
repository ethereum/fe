//! Finding and deciding trait candidates, in one place.
//!
//! A question (which impl answers a projection, which trait's constant or
//! associated type a name means, which method a call means, which provider
//! supplies an effect) has candidates. They are found in one step that never
//! proves anything, so it cannot reach a limit: [`impl_candidates`] filters
//! impls by self type, trait header and item name. Only candidates that pass
//! the question's filters are proved, by [`all_hold`]: each where clause holds,
//! does not hold, or is unknown because deciding it reached a normalization
//! limit.
//!
//! A limit is an unknown, not an answer and not a failure (law 5 of the
//! limits design). Unknowns combine as:
//! - a candidate with a where clause that does not hold does not hold, even
//!   if another of its clauses is unknown ("no and unknown is no");
//! - one candidate that holds answers a question that only needs one ("yes
//!   or unknown is yes"); the trait solver applies this to goals without
//!   inference variables;
//! - a choice among candidates is made only if it is the same whatever the
//!   unknown candidates turn out to be. [`decide`] evaluates the question's
//!   own policy with each unknown candidate counted as holding and as not
//!   holding; if the answers differ, the answer is the limit, reported once
//!   where the question arises.

use common::indexmap::{IndexMap, IndexSet};
use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    canonical::Canonical,
    fold::{TyFoldable, TyFolder},
    normalize::NormalizationLimit,
    trait_def::{
        ImplementorId, TraitInstId, contract_virtual_impls, impl_self_ty_may_match,
        impls_for_trait_and_ty, impls_for_ty, ingot_trait_env, is_std_evm_contract_trait_def,
    },
    trait_lower::complete_impl_assoc_ty,
    trait_resolution::{
        CanonicalGoalQuery, GoalSatisfiability, PredicateListId, TraitSolveCx,
        is_goal_query_satisfiable,
    },
    ty_def::{TyData, TyId},
    unify::UnificationTable,
    visitor::{TyVisitable, TyVisitor, walk_ty},
};
use crate::{
    Ingot,
    analysis::HirAnalysisDb,
    hir_def::{IdentId, Trait},
};

/// Whether a candidate applies.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Holds {
    Yes,
    /// The trait solver could not decide a where clause (several answers, or
    /// one of its own budgets ran out), and the question keeps the candidate
    /// for a later check.
    Undecided,
    No,
    /// Deciding a where clause reached a limit, and no other clause rules the
    /// candidate out.
    Unknown(NormalizationLimit),
}

/// How a where clause the solver cannot decide counts for a question.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Count {
    Yes,
    No,
    /// The candidate is kept as [`Holds::Undecided`].
    Kept,
}

/// How a question counts where clauses that are neither proved nor refuted
/// for reasons other than a limit. These differ between questions, as they
/// did before limits were unknowns.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Counting {
    /// A clause with several answers, or whose search stopped on a solver
    /// budget.
    pub(crate) undecided: Count,
    /// A clause that contains an error type.
    pub(crate) invalid: Count,
    /// Whether a clause's unique solution binds the candidate's parameters
    /// for the clauses after it.
    pub(crate) bind: bool,
}

impl Counting {
    /// Every clause is proved.
    pub(crate) const PROVED: Self = Self {
        undecided: Count::No,
        invalid: Count::No,
        bind: false,
    };
    /// No clause is refuted.
    pub(crate) const POSSIBLE: Self = Self {
        undecided: Count::Yes,
        invalid: Count::No,
        bind: false,
    };
    /// Method lookup: undecided clauses are confirmed where the method is
    /// used.
    pub(crate) const METHOD: Self = Self {
        undecided: Count::Kept,
        invalid: Count::Kept,
        bind: true,
    };
    /// Inherent constants: an error type does not rule an impl out.
    pub(crate) const INHERENT: Self = Self {
        undecided: Count::No,
        invalid: Count::Yes,
        bind: false,
    };
}

/// Whether the where clauses `constraints` of a candidate hold, with the
/// candidate instantiated in `table`.
///
/// A clause that does not hold decides, so the search goes on after an
/// unknown clause; when several clauses are unknown, the limit named is the
/// first by [`NormalizationLimit::priority`].
pub(crate) fn all_hold<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    table: &mut UnificationTable<'db>,
    constraints: &[TraitInstId<'db>],
    counting: Counting,
) -> Holds {
    let mut unknown = None;
    let mut kept = false;
    for &constraint in constraints {
        let constraint = constraint.fold_with(db, table);
        let query = CanonicalGoalQuery::in_context(db, constraint, solve_cx);
        let count = match is_goal_query_satisfiable(db, solve_cx, &query) {
            Ok(GoalSatisfiability::Satisfied(solution)) => {
                if counting.bind {
                    // A unique solution can bind parameters the header does
                    // not determine, such as a trait argument fixed only by
                    // this clause.
                    let solved = query.extract_solution(table, solution).inst;
                    let _ = table.unify(constraint, solved);
                }
                Count::Yes
            }
            Ok(GoalSatisfiability::NeedsConfirmation { .. }) => counting.undecided,
            Ok(GoalSatisfiability::ContainsInvalid) => counting.invalid,
            Ok(GoalSatisfiability::UnSat(_)) => Count::No,
            Err(limit) => {
                unknown = Some(NormalizationLimit::join(unknown, limit));
                continue;
            }
        };
        match count {
            Count::Yes => {}
            Count::No => return Holds::No,
            Count::Kept => kept = true,
        }
    }
    match unknown {
        Some(limit) => Holds::Unknown(limit),
        None if kept => Holds::Undecided,
        None => Holds::Yes,
    }
}

/// Whether every part of a conjunction holds, when asking a part may reach a
/// limit. A part that does not hold decides, so the parts after an unknown one
/// are still asked ("no and unknown is no"); otherwise one unknown part, the
/// first by [`NormalizationLimit::priority`], makes the conjunction unknown.
/// No part is asked after one that does not hold.
///
/// This is the one place the rule for a conjunction is written. A loop over
/// parts that returns at the first limit answers "limit" where a later part
/// does not hold.
pub(crate) fn all_of(
    parts: impl IntoIterator<Item = Result<bool, NormalizationLimit>>,
) -> Result<bool, NormalizationLimit> {
    let mut unknown = None;
    for part in parts {
        match part {
            Ok(true) => {}
            Ok(false) => return Ok(false),
            Err(limit) => unknown = Some(NormalizationLimit::join(unknown, limit)),
        }
    }
    unknown.map_or(Ok(true), Err)
}

/// Which trait a question is about.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TraitFilter<'db> {
    /// Any trait (the item decides).
    Any,
    /// This complete trait reference: the self type and every argument.
    Header(Canonical<TraitInstId<'db>>),
}

/// The item a question needs the candidate's trait to declare.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Item<'db> {
    AssocTy(IdentId<'db>),
    Const(IdentId<'db>),
    Method(IdentId<'db>),
}

impl<'db> Item<'db> {
    fn declared_by(self, db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> bool {
        match self {
            Item::AssocTy(name) => trait_.assoc_ty(db, name).is_some(),
            Item::Const(name) => trait_.const_(db, name).is_some(),
            Item::Method(name) => trait_.method_defs(db).contains_key(&name),
        }
    }
}

/// A question about the impls of a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Question<'db> {
    /// The type whose impls are asked for. With a [`TraitFilter::Header`] it
    /// is the header's self type.
    pub(crate) self_ty: Canonical<TyId<'db>>,
    pub(crate) trait_: TraitFilter<'db>,
    pub(crate) item: Item<'db>,
    /// The ingots whose impls are searched, in order.
    pub(crate) ingots: [Option<Ingot<'db>>; 2],
}

impl<'db> Question<'db> {
    /// A question about the complete trait reference `header`.
    pub(crate) fn header(
        db: &'db dyn HirAnalysisDb,
        header: Canonical<TraitInstId<'db>>,
        item: Item<'db>,
        ingots: [Option<Ingot<'db>>; 2],
    ) -> Self {
        let mut table = UnificationTable::new(db);
        let self_ty = header.extract_identity(&mut table).self_ty(db);
        Self {
            self_ty: Canonical::new(db, self_ty),
            trait_: TraitFilter::Header(header),
            item,
            ingots,
        }
    }
}

/// The impls `question` may select: those whose self type, trait header and
/// trait's items match it. Nothing is proved, so no limit is reached here.
pub(crate) fn impl_candidates<'db>(
    db: &'db dyn HirAnalysisDb,
    question: Question<'db>,
) -> Vec<ImplementorId<'db>> {
    let mut found: IndexSet<ImplementorId<'db>> = IndexSet::default();
    for ingot in question.ingots.into_iter().flatten() {
        found.extend(impl_candidates_in(db, ingot, question));
    }
    found.into_iter().collect()
}

fn impl_candidates_in<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    question: Question<'db>,
) -> Vec<ImplementorId<'db>> {
    let mut table = UnificationTable::new(db);
    let (ty, header, trait_def) = match question.trait_ {
        TraitFilter::Header(header) => {
            let header = header.extract_identity(&mut table);
            (header.self_ty(db), Some(header), Some(header.def(db)))
        }
        TraitFilter::Any => (question.self_ty.extract_identity(&mut table), None, None),
    };
    if ty.has_invalid(db) || ty.base_ty(db).is_never(db) {
        return Vec::new();
    }

    let env = ingot_trait_env(db, ingot);
    let mut raw_impls = match trait_def {
        Some(trait_def) => env.impls_for_trait(db, trait_def),
        None => env.impls_for_self_key(db, ty.base_ty(db)),
    };
    if ty.as_contract(db).is_some()
        && trait_def.is_none_or(|trait_def| is_std_evm_contract_trait_def(db, trait_def))
    {
        raw_impls.extend(contract_virtual_impls(db, ingot).iter().copied());
    }

    raw_impls
        .into_iter()
        .filter(|&impl_| {
            if !impl_self_ty_may_match(db, impl_.self_ty(db), ty)
                || !question.item.declared_by(db, impl_.trait_def(db))
            {
                return false;
            }
            let snapshot = table.snapshot();
            let inst = table.instantiate_with_fresh_vars(impl_);
            // The whole header is matched, every trait argument included, so
            // an impl for other trait arguments is never a candidate.
            let matches = match header {
                Some(header) => table.unify(inst.trait_(db), header).is_ok(),
                None => {
                    let impl_ty = table.instantiate_to_term(inst.self_ty(db));
                    let ty = table.instantiate_to_term(ty);
                    table.unify(impl_ty, ty).is_ok()
                }
            };
            table.rollback_to(snapshot);
            matches
        })
        .collect()
}

/// Whether the impl `candidate` of `question` applies: its header is matched
/// again and its where clauses are proved.
pub(crate) fn impl_holds<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    question: Question<'db>,
    candidate: ImplementorId<'db>,
    counting: Counting,
) -> Holds {
    let mut table = UnificationTable::new(db);
    let inst = table.instantiate_with_fresh_vars(candidate);
    if !header_matches(db, &mut table, question, inst) {
        return Holds::No;
    }
    all_hold(
        db,
        solve_cx,
        &mut table,
        inst.constraints(db).list(db),
        counting,
    )
}

/// Whether the impl `candidate` defines the associated type `name` for
/// `question`, for normalization: its header matches and no where clause is
/// refuted ([`Counting::POSSIBLE`]).
///
/// The clauses are proved as any goal, without assuming anything. A clause
/// that leads back to the projection being defined, directly as
/// `impl Tr for S where <S as Tr>::Out: Show`, through another impl's
/// projection, or through another trait's impl, cannot be proved that way:
/// proving it needs the projection it helps define. Only then, found by
/// [`may_need_projection`], the projection is read as the candidate's own
/// definition while its clauses are proved: a definition of the solver's
/// question, which resolves the projection and never proves a goal, so
/// `S: Tr` itself is still proved by an impl or an assumption, inductively.
pub(crate) fn impl_defines<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    question: Question<'db>,
    candidate: ImplementorId<'db>,
    name: IdentId<'db>,
) -> Holds {
    let candidate = complete_impl_assoc_ty(db, candidate, name).unwrap_or(candidate);
    let mut table = UnificationTable::new(db);
    let inst = table.instantiate_with_fresh_vars(candidate);
    if !header_matches(db, &mut table, question, inst) {
        return Holds::No;
    }
    let constraints: Vec<TraitInstId<'db>> = inst
        .constraints(db)
        .list(db)
        .iter()
        .map(|constraint| constraint.fold_with(db, &mut table))
        .collect();
    if constraints.is_empty() {
        return Holds::Yes;
    }
    let trait_inst = inst.trait_(db).fold_with(db, &mut table);
    let definition = inst
        .types(db)
        .get(&name)
        .map(|&ty| ty.fold_with(db, &mut table));
    let complete = |ty: TyId<'db>| !ty.has_var(db) && !ty.has_invalid(db);
    let solve_cx = match definition {
        Some(definition)
            if complete(definition) && trait_inst.args(db).iter().all(|&arg| complete(arg)) =>
        {
            let projection = TyId::assoc_ty(db, trait_inst.trait_ref(db), name);
            if definition != projection
                && may_need_projection(
                    db,
                    solve_cx,
                    PredicateListId::new(db, constraints.clone()),
                    projection,
                )
            {
                let defined = TraitInstId::new(
                    db,
                    trait_inst.def(db),
                    trait_inst.args(db).to_vec(),
                    [(name, definition)].into_iter().collect::<IndexMap<_, _>>(),
                );
                let mut definitions = solve_cx.definitions().list(db).to_vec();
                definitions.push(defined);
                solve_cx.with_definitions(PredicateListId::new(db, definitions))
            } else {
                solve_cx
            }
        }
        _ => solve_cx,
    };
    all_hold(db, solve_cx, &mut table, &constraints, Counting::POSSIBLE)
}

/// Whether the header of `inst`, an instantiated candidate, matches
/// `question`.
fn header_matches<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    question: Question<'db>,
    inst: ImplementorId<'db>,
) -> bool {
    match question.trait_ {
        TraitFilter::Header(header) => {
            let header = header.extract_identity(table);
            table.unify(inst.trait_(db), header).is_ok()
        }
        TraitFilter::Any => {
            let ty = question.self_ty.extract_identity(table);
            let impl_ty = table.instantiate_to_term(inst.self_ty(db));
            let ty = table.instantiate_to_term(ty);
            table.unify(impl_ty, ty).is_ok()
        }
    }
}

/// How many goals and projections [`may_need_projection`] visits before it
/// answers that the projection may be needed.
const PROJECTION_SEARCH_NODES: usize = 1024;

/// Whether proving `goals` may meet the projection `target` again. The
/// search follows what the trait solver may do, without proving anything:
/// a goal's types, the where clauses of the impls whose header may match it
/// and its supertraits, and a projection's types, the where clauses of the
/// impls that may define it and their definitions. It may answer yes where
/// the proof would not meet `target` (an impl that is ruled out later, or a
/// search past [`PROJECTION_SEARCH_NODES`]), never no where it would.
#[salsa::tracked]
pub(crate) fn may_need_projection<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    goals: PredicateListId<'db>,
    target: TyId<'db>,
) -> bool {
    enum Node<'db> {
        Goal(TraitInstId<'db>),
        Projection(TyId<'db>),
    }
    let mut search = ProjectionSearch {
        db,
        wildcards: FxHashSet::default(),
    };
    let mut seen_goals = FxHashSet::default();
    let mut seen_projections = FxHashSet::default();
    let mut pending: Vec<Node<'db>> = goals.list(db).iter().copied().map(Node::Goal).collect();
    for &assumption in solve_cx.assumptions().list(db) {
        for ty in assumption
            .args(db)
            .iter()
            .chain(assumption.assoc_type_bindings(db).values())
        {
            pending.extend(projections_in(db, *ty).into_iter().map(Node::Projection));
        }
    }
    while let Some(node) = pending.pop() {
        if seen_goals.len() + seen_projections.len() > PROJECTION_SEARCH_NODES {
            return true;
        }
        match node {
            Node::Goal(goal) => {
                if !seen_goals.insert(goal) {
                    continue;
                }
                for ty in goal
                    .args(db)
                    .iter()
                    .chain(goal.assoc_type_bindings(db).values())
                {
                    pending.extend(projections_in(db, *ty).into_iter().map(Node::Projection));
                }
                for (clauses, _) in search.matching_impls(solve_cx, goal, None) {
                    pending.extend(clauses.into_iter().map(Node::Goal));
                }
                for super_trait in goal.def(db).super_traits(db) {
                    pending.push(Node::Goal(super_trait.instantiate(db, goal.args(db))));
                }
            }
            Node::Projection(projection) => {
                if !seen_projections.insert(projection) {
                    continue;
                }
                if search.same_projection(projection, target) {
                    return true;
                }
                let TyData::AssocTy(assoc) = projection.data(db) else {
                    continue;
                };
                let trait_inst = assoc.trait_.as_predicate(db);
                for ty in trait_inst.args(db) {
                    pending.extend(projections_in(db, *ty).into_iter().map(Node::Projection));
                }
                for (clauses, definition) in
                    search.matching_impls(solve_cx, trait_inst, Some(assoc.name))
                {
                    pending.extend(clauses.into_iter().map(Node::Goal));
                    if let Some(definition) = definition {
                        pending.extend(
                            projections_in(db, definition)
                                .into_iter()
                                .map(Node::Projection),
                        );
                    }
                }
            }
        }
    }
    false
}

/// The projections in `ty`, nested ones included.
fn projections_in<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Vec<TyId<'db>> {
    struct Projections<'db> {
        db: &'db dyn HirAnalysisDb,
        found: Vec<TyId<'db>>,
    }
    impl<'db> TyVisitor<'db> for Projections<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_ty(&mut self, ty: TyId<'db>) {
            if matches!(ty.data(self.db), TyData::AssocTy(_)) {
                self.found.push(ty);
            }
            walk_ty(self, ty);
        }
    }
    let mut projections = Projections {
        db,
        found: Vec::new(),
    };
    ty.visit_with(&mut projections);
    projections.found
}

/// Matching impl headers against goals by their written structure, as
/// [`may_need_projection`] does.
struct ProjectionSearch<'db> {
    db: &'db dyn HirAnalysisDb,
    /// The parameters of impls that a match left open: they stand for any
    /// type, as the fresh variables the solver gives them do.
    wildcards: FxHashSet<TyId<'db>>,
}

impl<'db> ProjectionSearch<'db> {
    /// Whether `ty` may become any type: a projection, an inference variable,
    /// an open impl parameter, or a type the solver does not compare.
    fn is_wildcard(&self, ty: TyId<'db>) -> bool {
        matches!(
            ty.data(self.db),
            TyData::AssocTy(_)
                | TyData::QualifiedTy(_)
                | TyData::TyVar(_)
                | TyData::ConstTy(_)
                | TyData::Invalid(_)
        ) || self.wildcards.contains(&ty)
    }

    /// Whether `pattern`, with the impl parameters `params` free, may match
    /// `ty`; binds the parameters it fixes in `subst`.
    fn bind(
        &self,
        pattern: TyId<'db>,
        ty: TyId<'db>,
        params: &[TyId<'db>],
        subst: &mut FxHashMap<TyId<'db>, TyId<'db>>,
    ) -> bool {
        let db = self.db;
        if params.contains(&pattern) {
            return match subst.get(&pattern) {
                Some(&bound) => self.may_equal(bound, ty),
                None => {
                    subst.insert(pattern, ty);
                    true
                }
            };
        }
        if self.is_wildcard(ty) || self.is_wildcard(pattern) {
            return true;
        }
        match (pattern.data(db), ty.data(db)) {
            (TyData::TyApp(pattern_abs, pattern_arg), TyData::TyApp(abs, arg)) => {
                self.bind(*pattern_abs, *abs, params, subst)
                    && self.bind(*pattern_arg, *arg, params, subst)
            }
            _ => pattern == ty,
        }
    }

    /// Whether `a` and `b` may be the same type.
    fn may_equal(&self, a: TyId<'db>, b: TyId<'db>) -> bool {
        self.bind(a, b, &[], &mut FxHashMap::default())
    }

    /// Whether `projection` may be `target` once its types are resolved.
    fn same_projection(&self, projection: TyId<'db>, target: TyId<'db>) -> bool {
        let db = self.db;
        let (TyData::AssocTy(a), TyData::AssocTy(b)) = (projection.data(db), target.data(db))
        else {
            return false;
        };
        a.name == b.name
            && a.trait_.def(db) == b.trait_.def(db)
            && a.trait_
                .args(db)
                .iter()
                .zip(b.trait_.args(db))
                .all(|(&a, &b)| self.may_equal(a, b))
    }

    /// The impls whose header may match `goal`: the where clauses of each,
    /// with the parameters the match fixes substituted, and its definition
    /// of `name`, if asked.
    fn matching_impls(
        &mut self,
        solve_cx: TraitSolveCx<'db>,
        goal: TraitInstId<'db>,
        name: Option<IdentId<'db>>,
    ) -> Vec<(Vec<TraitInstId<'db>>, Option<TyId<'db>>)> {
        let db = self.db;
        let (primary, secondary) = solve_cx.search_ingots_for_trait_inst(db, goal);
        let mut found = Vec::new();
        for ingot in [Some(primary), secondary].into_iter().flatten() {
            for implementor in ingot_trait_env(db, ingot).impls_for_trait(db, goal.def(db)) {
                let implementor = match name {
                    Some(name) => {
                        complete_impl_assoc_ty(db, implementor, name).unwrap_or(implementor)
                    }
                    None => implementor,
                };
                let params = implementor.params(db).to_vec();
                let mut subst = FxHashMap::default();
                let header = implementor.trait_(db);
                if !header
                    .args(db)
                    .iter()
                    .zip(goal.args(db))
                    .all(|(&pattern, &ty)| self.bind(pattern, ty, &params, &mut subst))
                {
                    continue;
                }
                for &param in &params {
                    if !subst.contains_key(&param) {
                        self.wildcards.insert(param);
                    }
                }
                let mut folder = ParamSubst { subst: &subst };
                let clauses = implementor
                    .constraints(db)
                    .list(db)
                    .iter()
                    .map(|clause| clause.fold_with(db, &mut folder))
                    .collect();
                let definition = name
                    .and_then(|name| implementor.types(db).get(&name).copied())
                    .map(|ty| ty.fold_with(db, &mut folder));
                found.push((clauses, definition));
            }
        }
        found
    }
}

/// Replaces impl parameters by the types a match fixed.
struct ParamSubst<'a, 'db> {
    subst: &'a FxHashMap<TyId<'db>, TyId<'db>>,
}

impl<'db> TyFolder<'db> for ParamSubst<'_, 'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match self.subst.get(&ty) {
            Some(&replacement) => replacement,
            None => ty.super_fold_with(db, self),
        }
    }
}

/// The impls of `receiver` (of `trait_`, if given) whose trait declares the
/// method `name`, for method lookup. The self type is matched; nothing is
/// proved.
pub(crate) fn method_impl_candidates<'db>(
    db: &'db dyn HirAnalysisDb,
    receiver: Canonical<TyId<'db>>,
    trait_: Option<Trait<'db>>,
    name: IdentId<'db>,
    ingots: [Option<Ingot<'db>>; 2],
) -> Vec<ImplementorId<'db>> {
    let item = Item::Method(name);
    ingots
        .into_iter()
        .flatten()
        .flat_map(|ingot| match trait_ {
            Some(trait_def) => impls_for_trait_and_ty(db, ingot, trait_def, receiver),
            None => impls_for_ty(db, ingot, receiver),
        })
        .copied()
        .filter(|implementor| item.declared_by(db, implementor.trait_def(db)))
        .collect()
}

/// The most settings [`decide_grouped`] tries. Each unknown candidate doubles
/// them, so a question with more unknown candidates than this allows, none of
/// them counted together with another, has the first limit as its answer.
const MAX_SETTINGS: usize = 64;

/// What a policy answers once the unknown candidates are counted.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Decided<R> {
    /// The answer is the same in every setting.
    Same(R),
    /// The answer depends on the unknown candidates. `results` holds the
    /// answer in each setting tried; the last counts every unknown
    /// candidate as holding. Only if `exhaustive` is every setting there:
    /// past the cap on settings, `results` is that last answer alone, which
    /// says nothing about the others.
    Differs {
        results: Vec<R>,
        limit: NormalizationLimit,
        exhaustive: bool,
    },
}

impl<R> Decided<R> {
    /// The answer, or the limit it depends on.
    pub(crate) fn or_limit(self) -> Result<R, NormalizationLimit> {
        match self {
            Decided::Same(result) => Ok(result),
            Decided::Differs { limit, .. } => Err(limit),
        }
    }
}

/// Decides a question whose candidates include `unknown` ones (their limits,
/// in candidate order). `policy` answers the question when each unknown
/// candidate holds or not, as given by its `bool`.
pub(crate) fn decide<R: PartialEq>(
    unknown: &[NormalizationLimit],
    mut policy: impl FnMut(&[bool]) -> R,
) -> Decided<R> {
    let groups: Vec<Vec<NormalizationLimit>> = unknown.iter().map(|&limit| vec![limit]).collect();
    decide_grouped(&groups, |counts| {
        let setting: Vec<bool> = counts.iter().map(|&count| count == 1).collect();
        policy(&setting)
    })
}

/// Decides a question like [`decide`] when its policy cannot tell some of the
/// unknown candidates apart, as an effect frame cannot tell apart providers
/// that are not the one named. `groups` holds the limits of each group of
/// interchangeable unknown candidates; `policy` is given how many of each
/// group hold, the first ones of the group. Which candidates of a group hold
/// may change the answer only by which one of them the policy chooses, and
/// that answer differs from the one without the chosen candidate anyway.
pub(crate) fn decide_grouped<R: PartialEq>(
    groups: &[Vec<NormalizationLimit>],
    mut policy: impl FnMut(&[usize]) -> R,
) -> Decided<R> {
    let Some(limit) = groups.iter().flatten().fold(None, |earlier, &limit| {
        Some(NormalizationLimit::join(earlier, limit))
    }) else {
        return Decided::Same(policy(&vec![0; groups.len()]));
    };
    let all: Vec<usize> = groups.iter().map(Vec::len).collect();
    let settings = all
        .iter()
        .try_fold(1usize, |settings, &len| settings.checked_mul(len + 1))
        .filter(|&settings| settings <= MAX_SETTINGS);
    let Some(settings) = settings else {
        return Decided::Differs {
            results: vec![policy(&all)],
            limit,
            exhaustive: false,
        };
    };
    let results: Vec<R> = (0..settings)
        .map(|mut index| {
            let counts: Vec<usize> = all
                .iter()
                .map(|&len| {
                    let count = index % (len + 1);
                    index /= len + 1;
                    count
                })
                .collect();
            policy(&counts)
        })
        .collect();
    if results.windows(2).all(|pair| pair[0] == pair[1]) {
        Decided::Same(results.into_iter().next_back().unwrap())
    } else {
        Decided::Differs {
            results,
            limit,
            exhaustive: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const NESTING: Result<bool, NormalizationLimit> = Err(NormalizationLimit::Nesting);
    const WORK: Result<bool, NormalizationLimit> = Err(NormalizationLimit::Work);

    #[test]
    fn a_part_that_does_not_hold_decides_wherever_it_is() {
        for parts in [
            vec![Ok(true), NESTING, Ok(false)],
            vec![NESTING, Ok(false), Ok(true)],
            vec![Ok(false), NESTING, WORK],
        ] {
            assert_eq!(all_of(parts), Ok(false));
        }
    }

    #[test]
    fn without_a_failure_an_unknown_part_makes_the_whole_unknown() {
        assert_eq!(all_of([Ok(true), NESTING, Ok(true)]), NESTING);
        // The limit named does not depend on the order of the parts.
        assert_eq!(all_of([WORK, NESTING]), NESTING);
        assert_eq!(all_of([NESTING, WORK]), NESTING);
        assert_eq!(all_of([]), Ok(true));
        assert_eq!(all_of([Ok(true), Ok(true)]), Ok(true));
    }

    #[test]
    fn no_part_is_asked_after_one_that_does_not_hold() {
        let mut asked = 0;
        let parts = [Ok(true), Ok(false), Ok(true)]
            .into_iter()
            .inspect(|_| asked += 1);
        assert_eq!(all_of(parts), Ok(false));
        assert_eq!(asked, 2);
    }
}
