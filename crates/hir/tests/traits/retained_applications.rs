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
        panic!("expected KnownChoice on u32::inner(), got {:?}", inner_call.choice());
    };
    assert!(inner_known.is_ordinary());
    assert_eq!(inner_known.implementation(), inner_u32_impl);
    assert!(inner_known.bounds().is_none());

    // Call 1: `(Box { val: 1u32 }).outer()` records `KnownChoice::ordinary` for `impl<T: Inner> Outer for Box<T>`
    // with prerequisite `bounds` mapping `Inner<T> => Choice::Known(impl Inner for u32)`.
    let outer_call = caller_calls[1];
    let Some(Choice::Known(outer_known)) = outer_call.choice() else {
        panic!("expected KnownChoice on Box.outer(), got {:?}", outer_call.choice());
    };
    assert!(outer_known.is_ordinary());
    assert_eq!(outer_known.implementation(), outer_box_impl);
    let outer_bounds = outer_known.bounds().expect("conditional impl must record prerequisite bounds");
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
    let param_bounds = param_outer_known.bounds().expect("parameterized impl must record bound slot");
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
    assert_eq!(specialized_bounds.items(&db)[0].1, Choice::Known(inner_known));
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
    use fe_hir::analysis::ty::ty_check::check_func_body;

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

impl AltInner: Inner for u32 {
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
    for func_name in ["bad_direct", "bad_through_generic"] {
        let func = find_func(&db, module, func_name);
        let (diags, _) = check_func_body(&db, func);
        assert!(
            !diags.is_empty(),
            "expected conditional impl under `with` to be refused in `{func_name}`"
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
                        panic!("expected KnownChoice on u32::INNER, got {:?}", assoc.choice());
                    };
                    assert!(known.is_ordinary());
                    assert_eq!(known.implementation(), inner_u32_impl);
                    assert!(known.bounds().is_none());
                }
                "OUTER" => {
                    saw_outer = true;
                    let Some(Choice::Known(known)) = assoc.choice() else {
                        panic!("expected KnownChoice on Box<u32>::OUTER, got {:?}", assoc.choice());
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
