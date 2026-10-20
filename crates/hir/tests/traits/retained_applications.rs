use common::indexmap::IndexMap;
use fe_hir::{
    analysis::ty::{
        trait_def::{CheckedApplicationError, TraitInstId, prove_selected_hir_application},
        trait_resolution::TraitSolveCx,
        ty_def::TyId,
    },
    hir_def::IdentId,
    test_db::{HirAnalysisTestDb, find_func},
};

#[test]
fn public_checked_application_preserves_scope_and_revalidates_full_goal() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "public_checked_application.fe".into(),
        r#"
trait Produce { type Output }
struct Subject {}
impl Produce for Subject { type Output = u256 }
fn request(_ subject: Subject) {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let request = find_func(&db, module, "request");
    let subject = request.params(&db).next().unwrap().ty(&db);
    let subject = subject.as_view(&db).unwrap_or(subject);
    let trait_ = module.all_traits(&db)[0];
    let goal = TraitInstId::new_simple(&db, trait_, vec![subject]);
    let cx = TraitSolveCx::new(&db, request.scope());
    let checked = prove_selected_hir_application(&db, cx, module.all_impl_traits(&db)[0], goal)
        .expect("checked ordinary source must expose its application");
    assert_eq!(checked.goal(), goal);
    assert_eq!(checked.origin_context().scope(), request.scope());
    assert_ne!(checked.origin_context().scope(), module.scope());
    let output = IdentId::new(&db, "Output");
    assert_eq!(checked.associated_type(output), Some(TyId::u256(&db)));
    let mut bindings = IndexMap::new();
    bindings.insert(output, TyId::bool(&db));
    let wrong = TraitInstId::new(&db, trait_, vec![subject], bindings, goal.mode(&db));
    assert_eq!(
        checked.revalidate(cx, wrong).unwrap_err(),
        CheckedApplicationError::NotProven
    );
    let moved = checked
        .revalidate(TraitSolveCx::new(&db, module.scope()), goal)
        .expect("an unconditional application needs no inherited caller premise");
    assert_eq!(moved.origin_context().scope(), module.scope());
    assert_eq!(moved.associated_type(output), Some(TyId::u256(&db)));
}

#[test]
fn public_checked_application_rejects_unrequested_conditional_associated_output() {
    for (name, output, source_valid) in [
        ("plain", "u256", true),
        ("conditional_good", "Bounded<1>", true),
        ("conditional_bad", "Bounded<0>", false),
    ] {
        let source = r#"
trait Pick { type Out }
struct Key {}
struct Bounded<const N: usize> where { N > 0 } {}
impl Pick for Key { type Out = __OUT__ }
fn probe(_ key: Key) {}
"#
        .replace("__OUT__", output);
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(format!("{name}.fe").into(), &source);
        let (module, _) = db.top_mod(file);
        if source_valid {
            db.assert_no_diags(module);
        }
        let probe = find_func(&db, module, "probe");
        let subject = probe.params(&db).next().unwrap().ty(&db);
        let subject = subject.as_view(&db).unwrap_or(subject);
        // No output equality in the requested goal: the candidate's own
        // associated definition must still enter the admission receipt.
        let goal = TraitInstId::new_simple(&db, module.all_traits(&db)[0], vec![subject]);
        let result = prove_selected_hir_application(
            &db,
            TraitSolveCx::new(&db, probe.scope()),
            module.all_impl_traits(&db)[0],
            goal,
        );
        if name == "plain" {
            assert!(result.is_ok());
        } else {
            assert_eq!(
                result.unwrap_err(),
                CheckedApplicationError::UnsupportedConditions
            );
        }
    }
}

#[test]
fn normalized_prerequisite_cannot_hide_conditional_nominal_admission() {
    for output in ["u256", "Bounded<1>"] {
        let source = format!(
            "trait Supply {{ type Out }}\n\
             trait Marker<A> {{}}\n\
             trait Run {{}}\n\
             struct Key {{}}\n\
             struct Bounded<const N: usize> where {{ N > 0 }} {{}}\n\
             impl Supply for Key {{ type Out = {output} }}\n\
             impl<A> Marker<A> for Key {{}}\n\
             impl Run for Key where Key: Marker<<Key as Supply>::Out> {{}}\n\
             fn probe(_ key: Key) {{}}"
        );
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("normalized-prerequisite.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        // Both declarations are valid source. The checked application API has
        // a narrower admission boundary for conditional nominal applications.
        db.assert_no_diags(module);
        let probe = find_func(&db, module, "probe");
        let ty = probe.params(&db).next().unwrap().ty(&db);
        let ty = ty.as_view(&db).unwrap_or(ty);
        let goal = TraitInstId::new_simple(&db, module.all_traits(&db)[2], vec![ty]);
        let result = prove_selected_hir_application(
            &db,
            TraitSolveCx::new(&db, probe.scope()),
            module.all_impl_traits(&db)[2],
            goal,
        );
        if output == "u256" {
            result.expect("plain normalized prerequisite remains admitted");
        } else {
            assert_eq!(
                result.unwrap_err(),
                CheckedApplicationError::UnsupportedConditions
            );
        }
    }
}

#[test]
fn ordinary_call_records_known_choice_and_prerequisite_bound_evidence() {
    use fe_hir::{
        analysis::ty::{
            trait_choice::{Choice, Delivery, Resolved, SlotChoices, SlotRef, body_deliveries},
            ty_check::{TypedBody, check_func_body},
        },
        hir_def::CallableDef,
    };

    fn calls_of<'a, 'db>(
        db: &'db HirAnalysisTestDb,
        typed: &'a TypedBody<'db>,
    ) -> Vec<&'a fe_hir::analysis::ty::ty_check::Callable<'db>> {
        let body = typed.body().unwrap();
        body.exprs(db)
            .keys()
            .filter_map(|expr| typed.callable_expr(expr))
            .collect()
    }

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "ordinary_known_choice_skeleton.fe".into(),
        r#"
trait Inner {
    const fn inner() -> u32
}

impl Inner for u32 {
    const fn inner() -> u32 { 11 }
}

struct Box<T> {
    pub val: T,
}

trait Outer {
    const fn outer(self) -> u32
}

impl<T: Inner> Outer for Box<T> {
    const fn outer(self) -> u32 {
        T::inner()
    }
}

const fn run<M: Outer>(_ m: M) -> u32 {
    m.outer()
}

const fn wrap<U: Inner>(_ x: U) -> u32 {
    run(Box { val: x })
}

const fn caller() -> u32 {
    let one: u32 = 1
    let a = u32::inner()
    let b = (Box { val: one }).outer()
    let c = run(Box { val: one })
    let d = wrap(one)
    a + b + c + d
}

struct Verify<const N: u32> where { N == 44 } {}
fn verify(_ v: Verify<{ caller() }>) {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);

    let impls = module.all_impl_traits(&db);
    let inner_u32_impl = impls[0];
    let outer_box_impl = impls[1];

    // 1. Check `caller`: direct concrete call, conditional concrete impl call,
    // and generic bound call `run(Box { val: 1u32 })`.
    let caller_fn = find_func(&db, module, "caller");
    let (_, caller_typed) = check_func_body(&db, caller_fn);
    let caller_calls: Vec<_> = calls_of(&db, caller_typed)
        .into_iter()
        .filter(|callable| {
            let CallableDef::Func(func) = callable.callable_def() else {
                return false;
            };
            matches!(
                func.name(&db).to_opt().map(|id| id.data(&db).as_str()),
                Some("inner" | "outer" | "run" | "wrap")
            )
        })
        .collect();
    assert_eq!(caller_calls.len(), 4);

    // Call 0: `u32::inner()` records `KnownChoice::ordinary` for `impl Inner for u32` with no bounds.
    let inner_call = caller_calls[0];
    let Some(Choice::Known(inner_known)) = inner_call.choice() else {
        panic!(
            "expected KnownChoice on u32::inner(), got {:?}",
            inner_call.choice()
        );
    };
    assert!(inner_known.is_ordinary());
    assert_eq!(inner_known.implementation(), inner_u32_impl);
    assert!(inner_known.bounds().is_none());

    // Call 1: `(Box { val: 1u32 }).outer()` records `KnownChoice::ordinary` for `impl<T: Inner> Outer for Box<T>`
    // with prerequisite `bounds` mapping `Inner<T> => Choice::Known(impl Inner for u32)`.
    let outer_call = caller_calls[1];
    let Some(Choice::Known(outer_known)) = outer_call.choice() else {
        panic!(
            "expected KnownChoice on Box.outer(), got {:?}",
            outer_call.choice()
        );
    };
    assert!(outer_known.is_ordinary());
    assert_eq!(outer_known.implementation(), outer_box_impl);
    let outer_bounds = outer_known
        .bounds()
        .expect("conditional impl must record prerequisite bounds");
    assert_eq!(outer_bounds.items(&db).len(), 1);
    assert_eq!(outer_bounds.items(&db)[0].1, Choice::Known(inner_known));

    // Call 2: `run(Box { val: 1u32 })` delivers `outer_known` (carrying `inner_known`) to `run`'s `M: Outer` bound.
    let run_call = caller_calls[2];
    assert_eq!(run_call.delivered().len(), 1);
    assert_eq!(run_call.delivered()[0].1, Choice::Known(outer_known));

    // Follow `run`'s body: `m.outer()` resolves `SlotRef::Bound(Outer<M>)` to `Resolved::Known(outer_known)`,
    // and `body_deliveries` unpacks `outer_known.bounds()` into `<Box<T> as Outer>::outer`'s `Inner<T>` slot
    // without running the trait solver.
    let run_fn = find_func(&db, module, "run");
    let (_, run_typed) = check_func_body(&db, run_fn);
    let m_outer_call = calls_of(&db, run_typed)[0];
    let CallableDef::Func(nominal_outer_fn) = m_outer_call.callable_def() else {
        panic!("expected Func callable");
    };
    let run_deliveries = body_deliveries(&db, None, run_call.delivered(), run_fn, run_fn);
    let run_choices = SlotChoices::from_resolved(
        run_deliveries
            .into_iter()
            .map(|(slot, choice)| (slot, SlotChoices::default().resolve_in(&db, Some(choice))))
            .collect(),
    );
    let chosen_at_m_outer = run_choices.resolve_in(&db, m_outer_call.choice());
    assert_eq!(chosen_at_m_outer, Resolved::Known(outer_known));

    let box_outer_fn = outer_box_impl.methods(&db).next().unwrap();
    let box_outer_deliveries = body_deliveries(
        &db,
        chosen_at_m_outer.known(),
        m_outer_call.delivered(),
        nominal_outer_fn,
        box_outer_fn,
    );
    let box_outer_choices = SlotChoices::from_resolved(
        box_outer_deliveries
            .into_iter()
            .map(|(slot, choice)| (slot, run_choices.resolve_in(&db, Some(choice))))
            .collect(),
    );
    let (_, box_outer_typed) = check_func_body(&db, box_outer_fn);
    let t_inner_call = calls_of(&db, box_outer_typed)[0];
    assert_eq!(
        box_outer_choices.resolve_in(&db, t_inner_call.choice()),
        Resolved::Known(inner_known)
    );

    // 2. Check `wrap<U: Inner>`: `run(Box { val: x })` records parameterized `KnownChoice` whose
    // prerequisite bound is `Choice::Slot(SlotRef::Bound(Inner<U>))`, and specializing with `wrap(1u32)`'s
    // `SlotChoices` yields a concrete `KnownChoice` carrying `inner_known`.
    let wrap_fn = find_func(&db, module, "wrap");
    let (_, wrap_typed) = check_func_body(&db, wrap_fn);
    let wrap_run_call = calls_of(&db, wrap_typed)[0];
    let Some((_, Choice::Known(param_outer_known))) = wrap_run_call.delivered().first() else {
        panic!("expected parameterized KnownChoice delivered in wrap");
    };
    assert!(param_outer_known.is_ordinary());
    assert_eq!(param_outer_known.implementation(), outer_box_impl);
    let param_bounds = param_outer_known
        .bounds()
        .expect("parameterized impl must record bound slot");
    assert!(matches!(
        param_bounds.items(&db).as_slice(),
        [(Delivery::Bound(_), Choice::Slot(SlotRef::Bound(_)))]
    ));

    let wrap_call = caller_calls[3];
    let wrap_deliveries = body_deliveries(&db, None, wrap_call.delivered(), wrap_fn, wrap_fn);
    let wrap_choices = SlotChoices::from_resolved(
        wrap_deliveries
            .into_iter()
            .map(|(slot, choice)| (slot, SlotChoices::default().resolve_in(&db, Some(choice))))
            .collect(),
    );
    let Resolved::Known(specialized_outer) =
        wrap_choices.resolve_in(&db, Some(Choice::Known(*param_outer_known)))
    else {
        panic!("expected Known after specializing parameterized KnownChoice");
    };
    let specialized_bounds = specialized_outer.bounds().expect("specialized bounds");
    assert_eq!(
        specialized_bounds.items(&db)[0].1,
        Choice::Known(inner_known)
    );
}

#[test]
fn ordinary_choice_records_coinductive_cycle_without_diverging() {
    use fe_hir::analysis::ty::ty_check::check_func_body;

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "coinductive_ordinary_choice.fe".into(),
        r#"
struct Node {}

trait Cycle {
    fn ping(self) -> u32
}

impl Cycle for Node
where
    Node: Cycle
{
    fn ping(self) -> u32 { 7 }
}

fn use_cycle(_ n: Node) -> u32 {
    n.ping()
}
"#,
    );
    let (module, _) = db.top_mod(file);
    let use_cycle = find_func(&db, module, "use_cycle");
    let (diags, _) = check_func_body(&db, use_cycle);
    assert!(
        !diags.is_empty(),
        "cyclic impl condition terminates cleanly without diverging"
    );
}

#[test]
fn conditional_impl_under_with_remains_refused() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "conditional_impl_under_with_refused.fe".into(),
        r#"
trait Inner {
    fn inner() -> u32
}

impl Inner for u32 {
    fn inner() -> u32 { 1 }
}

impl Inner for u32 as AltInner {
    fn inner() -> u32 { 2 }
}

struct Box<T> {
    pub val: T,
}

trait Outer {
    fn outer(self) -> u32
}

impl<T: Inner> Outer for Box<T> {
    fn outer(self) -> u32 {
        T::inner()
    }
}

fn run<M: Outer>(_ m: M) -> u32 {
    m.outer()
}

fn wrap<U: Inner>(_ x: U) -> u32 {
    run(Box { val: x })
}

fn ok_outside_with() -> u32 {
    let one: u32 = 1
    run(Box { val: one }) + wrap(one)
}

fn bad_direct() -> u32 {
    let one: u32 = 1
    with (AltInner) {
        run(Box { val: one })
    }
}

fn bad_through_generic() -> u32 {
    let one: u32 = 1
    with (AltInner) {
        wrap(one)
    }
}
"#,
    );
    let (module, _) = db.top_mod(file);
    let ok_func = find_func(&db, module, "ok_outside_with");
    let (ok_diags, _) = fe_hir::analysis::ty::ty_check::check_func_body(&db, ok_func);
    assert!(
        ok_diags.is_empty(),
        "expected ok_outside_with to have no diagnostics, got: {ok_diags:?}"
    );
    for func_name in ["bad_direct", "bad_through_generic"] {
        let func = find_func(&db, module, func_name);
        let (diags, _) = fe_hir::analysis::ty::ty_check::check_func_body(&db, func);
        let formatted = format!("{diags:?}");
        assert!(
            formatted.contains("conditional implementations under `with` are not supported yet"),
            "expected OpenedCondition refusal in `{func_name}`, got: {formatted}"
        );
    }
}

#[test]
fn ordinary_assoc_const_records_known_choice_and_prerequisite_bound_evidence() {
    use fe_hir::{
        analysis::ty::{
            trait_choice::{Choice, Delivery, SlotChoices, body_deliveries},
            ty_check::{ConstRef, check_func_body},
        },
        hir_def::CallableDef,
    };

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "ordinary_assoc_const_known_choice.fe".into(),
        r#"
trait Inner {
    const INNER: u256
}

impl Inner for u32 {
    const INNER: u256 = 7
}

trait Outer {
    const OUTER: u256
}

struct Box<T> {
    pub val: T,
}

impl<T: Inner> Outer for Box<T> {
    const OUTER: u256 = T::INNER + 3
}

impl<T: Inner> Box<T> {
    const INHERENT: u256 = T::INNER + 5
}

const fn read_outer<M: Outer>() -> u256 {
    M::OUTER
}

const fn generic_box<U: Inner>() -> u256 {
    read_outer<Box<U>>() + Box<U>::OUTER + Box<U>::INHERENT
}

const fn caller() -> u256 {
    let a = u32::INNER
    let b = Box<u32>::OUTER
    let c = read_outer<Box<u32>>()
    let d = generic_box<u32>()
    let e = Box<u32>::INHERENT
    a + b + c + d + e
}

struct Verify<const N: u256> where { N == 71 } {}
fn verify(_ v: Verify<{ caller() }>) {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);

    let impls = module.all_impl_traits(&db);
    let inner_u32_impl = impls[0];
    let outer_box_impl = impls[1];

    let caller = find_func(&db, module, "caller");
    let (_, typed) = check_func_body(&db, caller);

    let mut saw_inner = false;
    let mut saw_outer = false;
    let mut saw_inherent = false;

    for cref in typed.const_refs() {
        match cref {
            ConstRef::TraitConst(assoc) => match assoc.name().data(&db).as_str() {
                "INNER" => {
                    saw_inner = true;
                    let Some(Choice::Known(known)) = assoc.choice() else {
                        panic!(
                            "expected KnownChoice on u32::INNER, got {:?}",
                            assoc.choice()
                        );
                    };
                    assert!(known.is_ordinary());
                    assert_eq!(known.implementation(), inner_u32_impl);
                    assert!(known.bounds().is_none());
                }
                "OUTER" => {
                    saw_outer = true;
                    let Some(Choice::Known(known)) = assoc.choice() else {
                        panic!(
                            "expected KnownChoice on Box<u32>::OUTER, got {:?}",
                            assoc.choice()
                        );
                    };
                    assert!(known.is_ordinary());
                    assert_eq!(known.implementation(), outer_box_impl);
                    let bounds = known
                        .bounds()
                        .expect("expected prerequisite bounds on Box<u32>: Outer");
                    assert_eq!(bounds.items(&db).len(), 1);
                    let (Delivery::Bound(_), Choice::Known(prereq)) = bounds.items(&db)[0] else {
                        panic!(
                            "expected KnownChoice prerequisite bound, got {:?}",
                            bounds.items(&db)
                        );
                    };
                    assert!(prereq.is_ordinary());
                    assert_eq!(prereq.implementation(), inner_u32_impl);
                }
                other => panic!("unexpected trait const {other}"),
            },
            ConstRef::InherentConst(use_) => {
                assert_eq!(use_.name().data(&db).as_str(), "INHERENT");
                saw_inherent = true;
                let delivered = use_
                    .delivered()
                    .expect("expected delivered bounds on Box<u32>::INHERENT");
                assert_eq!(delivered.items(&db).len(), 1);
                let (Delivery::Bound(_), Choice::Known(prereq)) = delivered.items(&db)[0] else {
                    panic!(
                        "expected KnownChoice bound on Box<u32>::INHERENT, got {:?}",
                        delivered.items(&db)
                    );
                };
                assert!(prereq.is_ordinary());
                assert_eq!(prereq.implementation(), inner_u32_impl);
            }
            ConstRef::Const(_) => {}
        }
    }
    assert!(saw_inner && saw_outer && saw_inherent);

    // Also verify generic_box's parameterized AssocConstUse specializes cleanly
    // when given a concrete SlotChoices for U: Inner.
    let generic_box = find_func(&db, module, "generic_box");
    let (_, gb_typed) = check_func_body(&db, generic_box);

    let caller_call = typed
        .body()
        .unwrap()
        .exprs(&db)
        .keys()
        .filter_map(|expr| typed.callable_expr(expr))
        .find(|c| matches!(c.callable_def(), CallableDef::Func(f) if f == generic_box))
        .expect("expected call to generic_box in caller");
    let gb_slots = SlotChoices::from_resolved(
        body_deliveries(
            &db,
            caller_call.choice().and_then(|c| c.known()),
            caller_call.delivered(),
            generic_box,
            generic_box,
        )
        .into_iter()
        .map(|(slot, choice)| (slot, SlotChoices::default().resolve_in(&db, Some(choice))))
        .collect(),
    );

    let mut saw_gb_outer = false;
    for cref in gb_typed.const_refs() {
        if let ConstRef::TraitConst(assoc) = cref {
            saw_gb_outer = true;
            let specialized = assoc.specialize(&db, &gb_slots);
            let Some(Choice::Known(known)) = specialized.choice() else {
                panic!(
                    "expected specialized Choice::Known, got {:?}",
                    specialized.choice()
                );
            };
            assert!(known.is_ordinary());
            assert_eq!(known.implementation(), outer_box_impl);
            let bounds = known.bounds().expect("expected specialized bounds");
            let (Delivery::Bound(_bound_slot), Choice::Known(prereq)) = bounds.items(&db)[0] else {
                panic!(
                    "expected specialized KnownChoice bound, got {:?}",
                    bounds.items(&db)
                );
            };
            assert_eq!(prereq.implementation(), inner_u32_impl);
        }
    }
    assert!(saw_gb_outer);
}

#[test]
fn prove_and_named_choice_record_prerequisite_bound_evidence_through_evidence_param() {
    use fe_hir::{
        analysis::ty::{
            trait_choice::{Choice, Delivery, Resolved, SlotChoices, body_deliveries},
            ty_check::check_func_body,
        },
        hir_def::CallableDef,
    };

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "prove_and_named_choice_bounds.fe".into(),
        r#"
use core::obligations::{Evidence, prove}

trait Inner {
    const fn inner() -> u32
}

impl Inner for u32 {
    const fn inner() -> u32 { 7 }
}

struct Box<T> {
    pub val: T,
}

trait Outer {
    const fn outer(self) -> u32
}

impl<T: Inner> Outer for Box<T> {
    const fn outer(self) -> u32 {
        T::inner() + 3
    }
}

impl Outer for Box<u32> as AltOuter
where
    u32: Inner
{
    const fn outer(self) -> u32 {
        u32::inner() + 100
    }
}

const fn run_with_ev(_ ev: Evidence<Outer<Box<u32>>>, _ b: Box<u32>) -> u32 {
    with (ev) {
        b.outer()
    }
}

const fn caller() -> u32 {
    let one: u32 = 1
    let b = Box { val: one }
    let ev_ord = prove<Outer<Box<u32>>>()
    let ev_alt = with (AltOuter) {
        prove<Outer<Box<u32>>>()
    }
    run_with_ev(ev_ord, b) + run_with_ev(ev_alt, b)
}

struct Verify<const N: u32> where { N == 117 } {}
fn verify(_ v: Verify<{ caller() }>) {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);

    let impls = module.all_impl_traits(&db);
    let inner_u32_impl = impls[0];
    let outer_box_impl = impls[1];
    let alt_outer_impl = impls[2];

    let caller = find_func(&db, module, "caller");
    let (_, typed) = check_func_body(&db, caller);
    let run_with_ev = find_func(&db, module, "run_with_ev");

    let run_calls: Vec<_> = typed
        .body()
        .unwrap()
        .exprs(&db)
        .keys()
        .filter_map(|expr| typed.callable_expr(expr))
        .filter(|c| matches!(c.callable_def(), CallableDef::Func(f) if f == run_with_ev))
        .collect();
    assert_eq!(run_calls.len(), 2);

    // 1. `ev_ord = prove<Outer<Box<u32>>>()` selects generic `impl<T: Inner> Outer for Box<T>`,
    // records `!known.is_ordinary()`, carries `bounds: Some([Inner<T> => KnownChoice(impl Inner for u32)])`,
    // and when passed through `ev: Evidence<Outer<Box<u32>>>` to `run_with_ev`, unpacks `Inner<T>`
    // into `<Box<T> as Outer>::outer` without solver re-selection.
    let ord_call = run_calls[0];
    let Some((Delivery::Param(0), Choice::Known(ord_known))) =
        ord_call.delivered().first().copied()
    else {
        panic!(
            "expected Evidence param delivery Choice::Known, got {:?}",
            ord_call.delivered()
        );
    };
    assert!(!ord_known.is_ordinary());
    assert_eq!(ord_known.implementation(), outer_box_impl);
    let bounds = ord_known
        .bounds()
        .expect("expected prerequisite bounds on proved generic impl choice");
    assert_eq!(bounds.items(&db).len(), 1);
    let (Delivery::Bound(_), Choice::Known(prereq)) = bounds.items(&db)[0] else {
        panic!("expected KnownChoice bound, got {:?}", bounds.items(&db));
    };
    assert!(prereq.is_ordinary());
    assert_eq!(prereq.implementation(), inner_u32_impl);

    let (_, run_typed) = check_func_body(&db, run_with_ev);
    let b_outer_call = run_typed
        .body()
        .unwrap()
        .exprs(&db)
        .keys()
        .find_map(|expr| run_typed.callable_expr(expr))
        .expect("b.outer() call");
    let CallableDef::Func(nominal_outer) = b_outer_call.callable_def() else {
        panic!("expected Func callable");
    };
    let run_slots = SlotChoices::from_resolved(
        body_deliveries(&db, None, ord_call.delivered(), run_with_ev, run_with_ev)
            .into_iter()
            .map(|(slot, choice)| (slot, SlotChoices::default().resolve_in(&db, Some(choice))))
            .collect(),
    );
    let chosen_outer = run_slots.resolve_in(&db, b_outer_call.choice());
    assert_eq!(chosen_outer, Resolved::Known(ord_known));
    let box_outer_fn = outer_box_impl.methods(&db).next().unwrap();
    let box_outer_deliveries = body_deliveries(
        &db,
        chosen_outer.known(),
        b_outer_call.delivered(),
        nominal_outer,
        box_outer_fn,
    );
    assert_eq!(box_outer_deliveries.len(), 1);
    assert_eq!(box_outer_deliveries[0].1, Choice::Known(prereq));

    // 2. `ev_alt = with (AltOuter) { prove<Outer<Box<u32>>>() }` selects `AltOuter` (`!is_ordinary()`),
    // and inside `AltOuter::outer`, `u32::inner()` directly carries `Choice::Known(prereq)`.
    let alt_call = run_calls[1];
    let Some((Delivery::Param(0), Choice::Known(alt_known))) =
        alt_call.delivered().first().copied()
    else {
        panic!(
            "expected Evidence param delivery Choice::Known, got {:?}",
            alt_call.delivered()
        );
    };
    assert!(!alt_known.is_ordinary());
    assert_eq!(alt_known.implementation(), alt_outer_impl);
    let alt_outer_fn = alt_outer_impl.methods(&db).next().unwrap();
    let (_, alt_outer_typed) = check_func_body(&db, alt_outer_fn);
    let inner_call_in_alt = alt_outer_typed
        .body()
        .unwrap()
        .exprs(&db)
        .keys()
        .find_map(|expr| alt_outer_typed.callable_expr(expr))
        .expect("u32::inner() call inside AltOuter::outer");
    assert_eq!(inner_call_in_alt.choice(), Some(Choice::Known(prereq)));
}

#[test]
fn multi_hop_prerequisite_bounds_specialize_and_unpack_without_reselection() {
    use fe_hir::{
        analysis::ty::{
            trait_choice::{Choice, Delivery, Resolved, SlotChoices, SlotRef, body_deliveries},
            ty_check::check_func_body,
        },
        hir_def::CallableDef,
    };

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "multi_hop_prerequisite_bounds.fe".into(),
        r#"
trait Step0 {
    fn step0() -> u32
}

impl Step0 for u32 {
    fn step0() -> u32 { 7 }
}

struct Wrap<T> {
    val: T,
}

trait Step1 {
    fn step1() -> u32
}

impl<T: Step0> Step1 for Wrap<T> {
    fn step1() -> u32 {
        T::step0() + 10
    }
}

struct Pair<T> {
    inner: Wrap<T>,
}

trait Step2 {
    fn step2() -> u32
}

impl<T> Step2 for Pair<T>
where
    Wrap<T>: Step1
{
    fn step2() -> u32 {
        Wrap<T>::step1() + 100
    }
}

fn use_step2<X: Step2>() -> u32 {
    X::step2()
}

fn generic_caller<T: Step0>() -> u32 {
    use_step2<Pair<T>>()
}

pub fn concrete_caller() -> u32 {
    generic_caller<u32>()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let impls = top_mod.all_impl_traits(&db);
    let step0_u32 = impls[0];
    let step1_wrap = impls[1];
    let step2_pair = impls[2];

    let generic_caller = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|f| f.name(&db).to_opt().map(|n| n.data(&db).as_str()) == Some("generic_caller"))
        .expect("generic_caller");
    let concrete_caller = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|f| f.name(&db).to_opt().map(|n| n.data(&db).as_str()) == Some("concrete_caller"))
        .expect("concrete_caller");

    let (_, generic_typed) = check_func_body(&db, generic_caller);
    let use_step2_call = generic_typed
        .body()
        .unwrap()
        .exprs(&db)
        .keys()
        .find_map(|expr| generic_typed.callable_expr(expr))
        .expect("use_step2<Pair<T>>() call");

    // In `generic_caller<T: Step0>()`, `use_step2<Pair<T>>()` delivers `Step2<Pair<T>>`
    // with 2-hop prerequisite bounds: `Step2<Pair<T>> -> Step1<Wrap<T>> -> SlotRef::Bound(T: Step0)`.
    let Some((_, Choice::Known(step2_known))) = use_step2_call.delivered().first().copied() else {
        panic!("expected KnownChoice delivery on use_step2<Pair<T>>()");
    };
    assert!(step2_known.is_ordinary());
    assert_eq!(step2_known.implementation(), step2_pair);
    let hop1 = step2_known.bounds().expect("hop1 bounds on Step2");
    let Some((_, Choice::Known(step1_known))) = hop1.items(&db).first().copied() else {
        panic!("expected KnownChoice for Step1<Wrap<T>> in hop1");
    };
    assert!(step1_known.is_ordinary());
    assert_eq!(step1_known.implementation(), step1_wrap);
    let hop2 = step1_known.bounds().expect("hop2 bounds on Step1");
    assert!(matches!(
        hop2.items(&db).as_slice(),
        [(Delivery::Bound(_), Choice::Slot(SlotRef::Bound(_)))]
    ));

    // When `concrete_caller` calls `generic_caller<u32>()`, specializing `step2_known`
    // through `generic_caller`'s `SlotChoices` replaces the inner `SlotRef::Bound(T: Step0)`
    // at hop 2 with `Choice::Known(step0_u32)`!
    let (_, concrete_typed) = check_func_body(&db, concrete_caller);
    let concrete_call = concrete_typed
        .body()
        .unwrap()
        .exprs(&db)
        .keys()
        .find_map(|expr| concrete_typed.callable_expr(expr))
        .expect("generic_caller<u32>() call");
    let CallableDef::Func(nominal_generic) = concrete_call.callable_def() else {
        panic!("expected Func");
    };
    let caller_slots = SlotChoices::from_resolved(
        body_deliveries(
            &db,
            None,
            concrete_call.delivered(),
            nominal_generic,
            generic_caller,
        )
        .into_iter()
        .map(|(slot, choice)| (slot, SlotChoices::default().resolve_in(&db, Some(choice))))
        .collect(),
    );
    let Resolved::Known(spec_step2) =
        caller_slots.resolve_in(&db, Some(Choice::Known(step2_known)))
    else {
        panic!("expected Known after specializing 2-hop choice");
    };
    let spec_hop1 = spec_step2.bounds().expect("specialized hop1");
    let (_, Choice::Known(spec_step1)) = spec_hop1.items(&db)[0] else {
        panic!("expected Known at specialized hop1");
    };
    let spec_hop2 = spec_step1.bounds().expect("specialized hop2");
    let (_, Choice::Known(spec_step0)) = spec_hop2.items(&db)[0] else {
        panic!("expected Known at specialized hop2");
    };
    assert_eq!(spec_step0.implementation(), step0_u32);
}

#[test]
fn for_loop_inside_closure_records_choices_read_at_for_bound_and_evidence_refusals() {
    let diag_codes = |name: &str, source: &str| -> Vec<String> {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(name.into(), source);
        let (top_mod, _) = db.top_mod(file);
        fe_hir::analysis::initialize_analysis_pass()
            .run_on_module(&db, top_mod)
            .into_iter()
            .map(|d| d.to_complete(&db).error_code.to_string())
            .collect()
    };

    let bound_in_closure_loop = r#"
use core::iter::Collection

trait Price { fn amount() -> usize }
struct Item {}
impl Price for Item { fn amount() -> usize { 100 } }
impl Price for Item as Wholesale { fn amount() -> usize { 70 } }

struct Once<T> {
    vals: [usize; 1],
}

impl<T: Price> Collection for Once<T> {
    type Item = usize
    type Cursor = usize
    fn start(self) -> usize { 0 }
    fn has(self, _ c: usize) -> bool { c < 1 && T::amount() > 0 }
    fn next(self, _ c: usize) -> usize { c + 1 }
    fn at(mut? self, _ c: usize) -> mut? usize { mut? self.vals[0] }
}

fn sum_in_closure<T: Price>(_ o: Once<T>) -> usize {
    let f = || -> usize {
        var acc: usize = 0
        for x in o {
            acc = acc + x
        }
        acc
    }
    f()
}

pub fn pass_opened_into_closure_for_loop(_ o: Once<Item>) -> usize {
    with (Wholesale) {
        sum_in_closure(o)
    }
}
"#;
    let codes_bound = diag_codes("bound_in_closure_loop.fe", bound_in_closure_loop);
    assert!(
        codes_bound.contains(&"8-0122".to_string()),
        "expected 8-0122 refusal for passing `with` choice into a generic function whose closure runs a `for` loop reading the bound, got {codes_bound:?}"
    );

    let evidence_in_closure_loop = r#"
use core::iter::Collection
use core::obligations::Evidence

struct Arr2 {
    vals: [usize; 2],
}

impl Collection for Arr2 {
    type Item = usize
    type Cursor = usize
    fn start(self) -> usize { 0 }
    fn has(self, _ c: usize) -> bool { c < 2 }
    fn next(self, _ c: usize) -> usize { c + 1 }
    fn at(mut? self, _ c: usize) -> mut? usize { mut? self.vals[c] }
}

impl Collection for Arr2 as ReverseArr {
    type Item = usize
    type Cursor = usize
    fn start(self) -> usize { 0 }
    fn has(self, _ c: usize) -> bool { c < 2 }
    fn next(self, _ c: usize) -> usize { c + 1 }
    fn at(mut? self, _ c: usize) -> mut? usize { mut? self.vals[1 - c] }
}

pub fn closure_reads_evidence_in_for_loop(_ ev: Evidence<Collection<Arr2>>) -> usize {
    let arr = Arr2 { vals: [1, 2] }
    with (ev) {
        let f = || -> usize {
            var acc: usize = 0
            for x in arr {
                acc = acc * 10 + x
            }
            acc
        }
        f()
    }
}
"#;
    let codes_ev = diag_codes("evidence_in_closure_loop.fe", evidence_in_closure_loop);
    assert!(
        codes_ev.contains(&"8-0098".to_string()),
        "expected 8-0098 refusal for a closure running a `for` loop under an enclosing `Evidence` choice, got {codes_ev:?}"
    );
}
