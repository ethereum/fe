use camino::Utf8PathBuf;
use fe_hir::analysis::semantic::{
    SConst, SExpr, SStmtKind, get_or_build_semantic_instance, identity_semantic_instance_key,
    root_semantic_instance_key, typed_body_template,
};
use fe_hir::analysis::ty::evidence::{Evidence, EvidenceId};
use fe_hir::analysis::ty::trait_resolution::{PredicateListId, TraitSolveCx};
use fe_hir::analysis::ty::ty_check::{BodyOwner, Callable, ConstRef, TypedBody, check_func_body};
use fe_hir::hir_def::{Expr, Func, ItemKind, Partial, TopLevelMod};
use fe_hir::test_db::HirAnalysisTestDb;

const SOURCE: &str = r#"
struct Wrap<T> {
    t: T,
}

impl<T: Copy> Copy for Wrap<T> {}

trait Grow {
    fn grow(self) -> u256
}

impl<T: Copy> Grow for T {
    fn grow(self) -> u256 {
        1
    }
}

fn wrapped<T: Copy>(_ x: T) -> u256 {
    (Wrap { t: x }).grow()
}

fn bound<T: Grow>(_ x: T) -> u256 {
    x.grow()
}

fn outer<T: Copy>(_ x: T) -> u256 {
    bound(Wrap { t: x })
}

fn root() -> u256 {
    outer(1 as u256)
}

trait HasD {
    const D: u256
}

impl<T: Copy> HasD for Wrap<T> {
    const D: u256 = 3
}

trait HasE {
    const E: u256 = 2
}

impl HasE for u256 {}

fn root_d() -> u256 {
    Wrap<u256>::D
}

fn generic_e<T: HasE>(_ x: T) -> u256 {
    T::E
}

fn root_e() -> u256 {
    generic_e(1 as u256)
}

trait HasK {
    const K: u256
}

impl<T: Copy> HasK for T {
    const K: u256 = 1
}

fn wrapped_k<T: Copy>(_ x: T) -> u256 {
    Wrap<T>::K
}

fn bound_k<T: HasK>(_ x: T) -> u256 {
    T::K
}

fn pair<T: Grow, U: Grow>(_ x: T, _ y: U) -> u256 {
    x.grow() + y.grow()
}

fn half<U: Grow>(_ y: U) -> u256 {
    pair(1 as u256, y)
}

fn relay<U: Grow + Copy>(_ x: U) -> u256 {
    bound(x)
}
"#;

fn func<'db>(db: &'db HirAnalysisTestDb, top_mod: TopLevelMod<'db>, name: &str) -> Func<'db> {
    top_mod
        .children_non_nested(db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|func_name| func_name.data(db) == name) =>
            {
                Some(func)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

/// `func`'s checked body and its only method call.
fn method_call<'db>(
    db: &'db HirAnalysisTestDb,
    func: Func<'db>,
) -> (TypedBody<'db>, Callable<'db>) {
    let typed_body = check_func_body(db, func).1.clone();
    let body = func.body(db).expect("missing body");
    let call = body
        .exprs(db)
        .keys()
        .find(|expr| matches!(expr.data(db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing method call");
    let callable = typed_body
        .callable_expr(call)
        .expect("missing callable")
        .clone();
    (typed_body, callable)
}

/// The checked body's assumption a proof leaf names.
fn assumption<'db>(
    db: &'db HirAnalysisTestDb,
    typed_body: &TypedBody<'db>,
    evidence: EvidenceId<'db>,
) -> String {
    let Evidence::Assumption(index) = evidence.data(db) else {
        panic!("expected an assumption: {:?}", evidence.data(db));
    };
    typed_body.assumptions().list(db)[*index as usize].pretty_print(db, true)
}

/// The proofs of an implementation's conditions.
fn conds<'db>(db: &'db HirAnalysisTestDb, evidence: EvidenceId<'db>) -> &'db [EvidenceId<'db>] {
    let Evidence::Impl { conds, .. } = evidence.data(db) else {
        panic!("expected an implementation: {:?}", evidence.data(db));
    };
    conds
}

#[test]
fn a_proof_names_the_body_assumptions_it_uses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("a_proof_names_the_body_assumptions_it_uses.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `Wrap<T>: Grow` holds by the blanket implementation, whose condition
    // `Wrap<T>: Copy` holds by `Copy for Wrap<T>`, whose condition is the
    // body's own bound.
    let (typed_body, callable) = method_call(&db, func(&db, top_mod, "wrapped"));
    let evidence = callable.evidence().expect("the call's goal is proved");
    let [wrap_copy] = conds(&db, evidence) else {
        panic!("the blanket implementation has one condition");
    };
    let [t_copy] = conds(&db, *wrap_copy) else {
        panic!("`Copy for Wrap<T>` has one condition");
    };
    assert_eq!(assumption(&db, &typed_body, *t_copy), "T: Copy");

    let (typed_body, callable) = method_call(&db, func(&db, top_mod, "bound"));
    let evidence = callable.evidence().expect("the call's goal is proved");
    assert_eq!(assumption(&db, &typed_body, evidence), "T: Grow");
}

/// The body `func`'s only method call runs, selected in a context with no
/// assumptions, and whether selecting with the body's assumptions reaches a
/// body.
fn grow_call_bodies<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>) -> (Option<Func<'db>>, bool) {
    let (typed_body, callable) = method_call(db, func);
    let without_assumptions = callable
        .selected_method(
            db,
            TraitSolveCx::new(db, func.scope()).with_assumptions(PredicateListId::empty_list(db)),
        )
        .and_then(|method| method.body());
    let with_assumptions = callable
        .selected_method(
            db,
            TraitSolveCx::new(db, func.scope()).with_assumptions(typed_body.assumptions()),
        )
        .is_some_and(|method| method.body().is_some());
    (without_assumptions, with_assumptions)
}

/// Whether `func`'s only trait-constant use reaches a defining body where
/// nothing is assumed, and where the body's assumptions are.
fn k_use_bodies(db: &HirAnalysisTestDb, func: Func<'_>) -> (bool, bool) {
    let typed_body = check_func_body(db, func).1.clone();
    let Some(ConstRef::TraitConst { use_ }) = typed_body.const_refs().next() else {
        panic!("missing trait constant use");
    };
    let without_assumptions = ConstRef::TraitConst {
        use_: use_.with_env(func.scope(), PredicateListId::empty_list(db)),
    };
    let with_assumptions = ConstRef::TraitConst {
        use_: use_.with_env(func.scope(), typed_body.assumptions()),
    };
    (
        without_assumptions.trait_const_body(db).is_some(),
        with_assumptions.trait_const_body(db).is_some(),
    )
}

#[test]
fn checker_selection_answers_a_call_without_the_body_context() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("checker_selection_answers_a_call_without_the_body_context.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `Wrap<T>: Grow` holds only because `T: Copy` is assumed. The checker
    // selected the blanket implementation, so the call reaches its body even
    // where nothing is assumed.
    let (body, _) = grow_call_bodies(&db, func(&db, top_mod, "wrapped"));
    let body = body.expect("the recorded blanket implementation answers the call");
    assert!(matches!(
        body.scope().parent_item(&db),
        Some(ItemKind::ImplTrait(_))
    ));
}

#[test]
fn bound_dispatch_is_selected_where_the_body_is_instantiated() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("bound_dispatch_is_selected_where_the_body_is_instantiated.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `T: Grow` holds by the body's own bound: no implementation is chosen
    // until `T` is known.
    let (without_assumptions, with_assumptions) =
        grow_call_bodies(&db, func(&db, top_mod, "bound"));
    assert_eq!(without_assumptions, None);
    assert!(!with_assumptions);
}

#[test]
fn checker_selection_answers_a_constant_without_the_body_context() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("checker_selection_answers_a_constant_without_the_body_context.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // As for a call: the blanket implementation the checker selected
    // defines `Wrap<T>::K` even where `T: Copy` is not assumed.
    assert_eq!(
        k_use_bodies(&db, func(&db, top_mod, "wrapped_k")),
        (true, true)
    );
    // `T::K` holds by the body's bound: it has no defining body until `T` is
    // known.
    assert_eq!(
        k_use_bodies(&db, func(&db, top_mod, "bound_k")),
        (false, false)
    );
}

#[test]
fn an_instance_receives_its_callers_proofs_of_its_bounds() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("an_instance_receives_its_callers_proofs_of_its_bounds.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `outer` proves `bound`'s `T: Grow` for `Wrap<T>` from its own
    // `T: Copy`: the call records that proof.
    let outer_fn = func(&db, top_mod, "outer");
    let outer_body = check_func_body(&db, outer_fn).1.clone();
    let outer_call = outer_fn
        .body(&db)
        .and_then(|body| {
            body.exprs(&db)
                .keys()
                .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::Call(..))))
        })
        .expect("missing call");
    let recorded = outer_body
        .callable_expr(outer_call)
        .and_then(Callable::bound_evidence)
        .and_then(|bound| bound.entries(&db).iter().flatten().next().copied())
        .expect("the call records its proof of `T: Grow`");
    let [wrap_copy] = conds(&db, recorded) else {
        panic!("the blanket implementation has one condition");
    };
    let [t_copy] = conds(&db, *wrap_copy) else {
        panic!("`Copy for Wrap<T>` has one condition");
    };
    assert_eq!(assumption(&db, &outer_body, *t_copy), "T: Copy");

    // In `outer<u256>`, which `root` makes, that proof names `u256: Copy`;
    // `bound<Wrap<u256>>` receives it, so its call `x.grow()`, checked
    // against the bound, carries the blanket implementation with its
    // conditions proved.
    let root = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(func(&db, top_mod, "root"), None))
            .expect("missing root key"),
    );
    let outer = root
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(outer_fn, None))
        .map(|callee| get_or_build_semantic_instance(&db, callee.key))
        .expect("`root` calls `outer`");
    let bound_fn = func(&db, top_mod, "bound");
    let bound = outer
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(bound_fn, None))
        .expect("`outer` calls `bound`");
    let body = bound_fn.body(&db).expect("missing body");
    let call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing method call");
    let evidence = bound
        .key
        .typed_body(&db)
        .callable_expr(call)
        .and_then(Callable::evidence)
        .expect("the call's goal is proved");
    let [wrap_copy] = conds(&db, evidence) else {
        panic!("the blanket implementation has one condition");
    };
    let [u256_copy] = conds(&db, *wrap_copy) else {
        panic!("`Copy for Wrap<T>` has one condition");
    };
    assert!(conds(&db, *u256_copy).is_empty());
}

/// Whether the body defining the only trait-constant use in `body` receives
/// a closed proof of each assumption it is checked under.
fn const_body_evidence(db: &HirAnalysisTestDb, body: &TypedBody<'_>) -> Vec<bool> {
    let const_ref = body
        .const_refs()
        .next()
        .expect("missing trait constant use");
    let (_, _, _, evidence) = const_ref
        .trait_const_body(db)
        .expect("the use has a defining body");
    evidence
        .entries(db)
        .iter()
        .map(|entry| entry.is_some_and(|evidence| evidence.is_closed(db)))
        .collect()
}

#[test]
fn a_constant_body_receives_proofs_of_its_assumptions() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("a_constant_body_receives_proofs_of_its_assumptions.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `Wrap<u256>::D` is the implementation's value, checked under its
    // condition `T: Copy`: the selection's proof of the condition supplies
    // it.
    let root_d = typed_body_template(&db, BodyOwner::Func(func(&db, top_mod, "root_d"), None));
    assert_eq!(const_body_evidence(&db, &root_d.body), vec![true]);

    // `T::E` in `generic_e` is the trait's default once `T` is known: in
    // `generic_e<u256>`, which `root_e` makes, the use's proof of
    // `Self: HasE`, the instance's own `u256: HasE`, supplies it.
    let root_e = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(func(&db, top_mod, "root_e"), None))
            .expect("missing root key"),
    );
    let generic_e_fn = func(&db, top_mod, "generic_e");
    let generic_e = root_e
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(generic_e_fn, None))
        .expect("`root_e` calls `generic_e`");
    assert!(const_body_evidence(&db, generic_e.key.typed_body(&db)).contains(&true));
}

#[test]
fn an_instance_proves_the_bounds_its_arguments_close() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("an_instance_proves_the_bounds_its_arguments_close.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `half` leaves `pair`'s closed bound `u256: Grow` to the instance and
    // proves `U: Grow` by its own bound. In `pair<u256, U>`, which `half`'s
    // body makes, `x.grow()` so carries the blanket implementation, and
    // `y.grow()` still holds by `half`'s bound.
    let half = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func(&db, top_mod, "half"), None)),
    );
    let pair_fn = func(&db, top_mod, "pair");
    let pair = half
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(pair_fn, None))
        .expect("`half` calls `pair`");
    let body = pair_fn.body(&db).expect("missing body");
    let mut proofs: Vec<_> = body
        .exprs(&db)
        .keys()
        .filter(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .map(|call| {
            let evidence = pair
                .key
                .typed_body(&db)
                .callable_expr(call)
                .and_then(Callable::evidence)
                .expect("the call's goal is proved");
            match evidence.data(&db) {
                Evidence::Impl { .. } if evidence.is_closed(&db) => "closed implementation",
                Evidence::Impl { .. } => "implementation",
                Evidence::Assumption(_) => "assumption",
                Evidence::Param(_) => "param",
                Evidence::Unsupplied => "unsupplied",
            }
        })
        .collect();
    proofs.sort();
    assert_eq!(proofs, ["assumption", "closed implementation"]);
}

#[test]
fn a_generic_instance_dispatches_through_the_bound_its_caller_proved() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("a_generic_instance_dispatches_through_the_bound_its_caller_proved.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `relay` proves `bound`'s `T: Grow` for `U` by its own bound. In
    // `bound<U>`, which `relay`'s body makes, `x.grow()` is dispatched
    // through that bound, although the blanket implementation also applies
    // where `U: Copy` is assumed: no body is chosen until `U` is known.
    let relay_fn = func(&db, top_mod, "relay");
    let relay = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(relay_fn, None)),
    );
    let bound_fn = func(&db, top_mod, "bound");
    let bound = relay
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(bound_fn, None))
        .expect("`relay` calls `bound`");
    let body = bound_fn.body(&db).expect("missing body");
    let call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing method call");
    let relay_assumptions = check_func_body(&db, relay_fn).1.assumptions();
    let method = bound
        .key
        .typed_body(&db)
        .callable_expr(call)
        .expect("missing callable")
        .selected_method(
            &db,
            TraitSolveCx::new(&db, relay_fn.scope()).with_assumptions(relay_assumptions),
        )
        .expect("the bound answers the call");
    assert_eq!(method.body(), None);
}

#[test]
fn named_choice_dispatches_through_generic_helper_via_evidence_id() {
    use fe_hir::analysis::ty::trait_def::ImplementorOrigin;

    const NAMED_SLICE_SOURCE: &str = r#"
use core::obligations::{Evidence, prove}

trait Rate {
    const BASE: u256
    const DEF_BASE: u256 = Self::BASE + 1
    const fn rate(self) -> u256
    const fn total(self) -> u256 {
        self.rate() + Self::BASE + Self::DEF_BASE
    }
}

impl Rate for u256 {
    const BASE: u256 = 10
    const fn rate(self) -> u256 { 100 }
}

impl Rate for u256 as Wholesale {
    const BASE: u256 = 20
    const fn rate(self) -> u256 { 200 }
}

const fn helper<T: Rate>(_ x: T) -> u256 {
    x.rate() + x.total() + T::BASE + T::DEF_BASE
}

const fn helper_ev(_ ev: Evidence<Rate<u256>>, _ x: u256) -> u256 {
    with (ev) {
        helper(x)
    }
}

struct Boxed<T> {
    pub x: T,
}

trait Quote {
    const Q: u256
    const DEF_Q: u256 = Self::Q + 2
    const fn quote(self) -> u256
    const fn full_quote(self) -> u256 {
        self.quote() + Self::Q + Self::DEF_Q
    }
}

impl<T: Rate> Quote for Boxed<T> {
    const Q: u256 = T::BASE
    const fn quote(self) -> u256 {
        (self.x).total()
    }
}

const fn before_val() -> u256 {
    helper(1 as u256)
}

const fn during_val() -> u256 {
    with (Wholesale) {
        helper(1 as u256)
    }
}

const fn after_val() -> u256 {
    helper(1 as u256)
}

const fn scoped_sequence() -> u256 {
    let before = helper(1 as u256)
    let during = with (Wholesale) {
        helper(1 as u256)
    }
    let after = helper(1 as u256)
    before * 1000000 + during * 1000 + after
}

const fn ev_ord_val() -> u256 {
    let ev = prove<Rate<u256>>()
    helper_ev(ev, 1 as u256)
}

const fn ev_alt_val() -> u256 {
    let ev = with (Wholesale) {
        prove<Rate<u256>>()
    }
    helper_ev(ev, 1 as u256)
}

const fn cond_alt_val() -> u256 {
    let b = Boxed { x: 1 as u256 }
    with (Wholesale) {
        b.quote() + b.full_quote() + <Boxed<u256> as Quote>::Q + <Boxed<u256> as Quote>::DEF_Q
    }
}

struct CheckBefore<const N: u256> where { N == 242 } {}
struct CheckDuring<const N: u256> where { N == 482 } {}
struct CheckAfter<const N: u256> where { N == 242 } {}
struct CheckSeq<const N: u256> where { N == 242482242 } {}
struct CheckEvOrd<const N: u256> where { N == 242 } {}
struct CheckEvAlt<const N: u256> where { N == 482 } {}
struct CheckCondAlt<const N: u256> where { N == 566 } {}

fn verify(
    _ a: CheckBefore<{ before_val() }>,
    _ b: CheckDuring<{ during_val() }>,
    _ c: CheckAfter<{ after_val() }>,
    _ d: CheckSeq<{ scoped_sequence() }>,
    _ e: CheckEvOrd<{ ev_ord_val() }>,
    _ f: CheckEvAlt<{ ev_alt_val() }>,
    _ g: CheckCondAlt<{ cond_alt_val() }>,
) {}
"#;

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("named_choice_dispatches_through_generic_helper_via_evidence_id.fe"),
        NAMED_SLICE_SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let impls = top_mod.all_impl_traits(&db);
    let ord_rate_impl = impls[0];
    let wholesale_impl = impls[1];

    let helper_fn = func(&db, top_mod, "helper");
    let before_fn = func(&db, top_mod, "before_val");
    let during_fn = func(&db, top_mod, "during_val");
    let after_fn = func(&db, top_mod, "after_val");
    let seq_fn = func(&db, top_mod, "scoped_sequence");

    let before_inst = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(before_fn, None)).expect("before root key"),
    );
    let during_inst = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(during_fn, None)).expect("during root key"),
    );
    let after_inst = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(after_fn, None)).expect("after root key"),
    );
    let seq_inst = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(seq_fn, None)).expect("seq root key"),
    );

    let before_key = before_inst
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(helper_fn, None))
        .expect("before_val calls helper")
        .key;
    let during_key = during_inst
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(helper_fn, None))
        .expect("during_val calls helper")
        .key;
    let after_key = after_inst
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(helper_fn, None))
        .expect("after_val calls helper")
        .key;

    let helper_callees: Vec<_> = seq_inst
        .callees(&db)
        .iter()
        .filter(|callee| callee.key.owner(&db) == BodyOwner::Func(helper_fn, None))
        .map(|callee| callee.key)
        .collect();
    assert_eq!(
        helper_callees,
        vec![before_key, during_key],
        "scoped_sequence's 3 calls (before, during `with (Wholesale)`, after) deduplicate to 2 distinct SemanticInstanceKeys"
    );

    // 1. Scope restoration and canonical key stability:
    // `before` and `after` outside `with (Wholesale)` produce the exact same canonical
    // `SemanticInstanceKey` with `EvidenceList::none`, whereas `during` inside `with (Wholesale)`
    // has a distinct `SemanticInstanceKey` carrying the named `EvidenceId`.
    assert_eq!(
        before_key, after_key,
        "ordinary calls before and after `with (Wholesale)` must share one canonical SemanticInstanceKey"
    );
    assert_ne!(
        before_key, during_key,
        "named choice call inside `with (Wholesale)` must have a distinct SemanticInstanceKey"
    );
    assert!(
        before_key.evidence(&db).entries(&db).is_empty()
            && before_key.evidence(&db).params(&db).is_empty(),
        "ordinary concrete instance key carries EvidenceList::none"
    );

    // 2. Verify `during_key` carries `Evidence::Impl` for `Wholesale` (`!is_ordinary`) in `EvidenceList::entries`:
    let during_entries = during_key.evidence(&db).entries(&db);
    let wholesale_ev = during_entries
        .iter()
        .flatten()
        .copied()
        .next()
        .expect("during_key must carry EvidenceId for T: Rate");
    assert!(
        !wholesale_ev.is_ordinary(&db),
        "Wholesale EvidenceId must be marked non-ordinary"
    );
    match wholesale_ev.data(&db) {
        Evidence::Impl {
            selected, conds, ..
        } => {
            assert_eq!(
                selected.origin(&db),
                ImplementorOrigin::Hir(wholesale_impl),
                "EvidenceId must directly select `impl Rate for u256 as Wholesale`"
            );
            assert!(conds.is_empty());
        }
        other => panic!("expected Evidence::Impl for Wholesale, got {other:?}"),
    }

    // 3. Inside `during_key.typed_body(&db)`, `x.rate()` has its `Assumption(T: Rate)` rebased
    // to `wholesale_ev`, and `selected_method` dispatches to `Wholesale::rate`, while inside
    // `before_key.typed_body(&db)`, `x.rate()` carries ordinary `ord_rate_impl` and dispatches
    // to the ordinary `Rate::rate`:
    let helper_body = helper_fn.body(&db).expect("helper body");
    let first_method_call = helper_body
        .exprs(&db)
        .keys()
        .find(|expr| {
            matches!(
                expr.data(&db, helper_body),
                Partial::Present(Expr::MethodCall(..))
            )
        })
        .expect("method call in helper");

    let before_call_ev = before_key
        .typed_body(&db)
        .callable_expr(first_method_call)
        .and_then(Callable::evidence)
        .expect("before call evidence");
    assert!(before_call_ev.is_ordinary(&db));
    let before_method = before_key
        .typed_body(&db)
        .callable_expr(first_method_call)
        .unwrap()
        .selected_method(&db, TraitSolveCx::new(&db, helper_fn.scope()))
        .and_then(|m| m.body())
        .expect("before selected method body");
    assert_eq!(
        before_method.scope().parent_item(&db),
        Some(ItemKind::ImplTrait(ord_rate_impl))
    );

    let during_call_ev = during_key
        .typed_body(&db)
        .callable_expr(first_method_call)
        .and_then(Callable::evidence)
        .expect("during call evidence");
    assert_eq!(during_call_ev, wholesale_ev);
    let during_method = during_key
        .typed_body(&db)
        .callable_expr(first_method_call)
        .unwrap()
        .selected_method(&db, TraitSolveCx::new(&db, helper_fn.scope()))
        .and_then(|m| m.body())
        .expect("during selected method body");
    assert_eq!(
        during_method.scope().parent_item(&db),
        Some(ItemKind::ImplTrait(wholesale_impl))
    );

    // 4. Verify `Evidence<Rate<u256>>` parameter transport through `EvidenceList::params`:
    let ev_alt_fn = func(&db, top_mod, "ev_alt_val");
    let ev_alt_inst = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(ev_alt_fn, None)).expect("ev_alt root key"),
    );
    let helper_ev_fn = func(&db, top_mod, "helper_ev");
    let helper_ev_callee = ev_alt_inst
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(helper_ev_fn, None))
        .expect("ev_alt_val calls helper_ev");
    let params = helper_ev_callee.key.evidence(&db).params(&db);
    assert_eq!(params.len(), 1);
    assert_eq!(params[0].0, 0);
    assert_eq!(params[0].1, wholesale_ev);

    // 5. Verify pending `Evidence::Param(0)` on `helper_ev`'s unspecialized callee `helper<u256>`
    // keeps `T::BASE` and `T::DEF_BASE` abstract (`SymbolicTraitConst`) rather than falling back
    // to the ordinary `impl Rate for u256` constant:
    let helper_ev_root_inst = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(helper_ev_fn, None))
            .expect("helper_ev root key"),
    );
    let helper_param_key = helper_ev_root_inst
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(helper_fn, None))
        .expect("helper_ev root calls helper<u256>")
        .key;
    let param_ev = helper_param_key
        .evidence(&db)
        .entries(&db)
        .iter()
        .flatten()
        .copied()
        .next()
        .expect("helper_param_key carries EvidenceId for T: Rate");
    assert_eq!(param_ev.data(&db), &Evidence::Param(0));

    let helper_param_body = helper_param_key.typed_body(&db);
    let trait_const_refs: Vec<_> = helper_param_body.const_refs().collect();
    assert_eq!(trait_const_refs.len(), 2, "T::BASE and T::DEF_BASE");
    for cref in trait_const_refs {
        match cref {
            ConstRef::TraitConst { use_ } => {
                assert_eq!(use_.evidence(), Some(param_ev));
                assert!(
                    cref.trait_const_body(&db).is_none(),
                    "pending Evidence::Param(0) must not resolve to a concrete trait const body"
                );
            }
            other => panic!("expected ConstRef::TraitConst, got {other:?}"),
        }
    }

    let helper_param_inst = get_or_build_semantic_instance(&db, helper_param_key);
    let mut folded_values = 0;
    let mut symbolic_consts = 0;
    for block in &helper_param_inst.body(&db).blocks {
        for stmt in &block.stmts {
            if let SStmtKind::Assign {
                expr: SExpr::Const(sconst),
                ..
            } = &stmt.kind
            {
                match sconst {
                    SConst::Value(_) | SConst::Ref(_) => folded_values += 1,
                    SConst::Description(_) => symbolic_consts += 1,
                    _ => {}
                }
            }
        }
    }
    assert_eq!(
        folded_values, 0,
        "neither T::BASE nor T::DEF_BASE may fold to ordinary `u256: Rate` while Evidence::Param(0) is pending"
    );
    assert_eq!(
        symbolic_consts, 2,
        "both T::BASE and T::DEF_BASE must remain abstract in the unspecialized body"
    );
}
