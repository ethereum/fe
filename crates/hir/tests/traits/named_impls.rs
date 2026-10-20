use fe_hir::{
    analysis::ty::{
        trait_def::{
            CheckedApplicationError, ImplSelectionClass, TraitInstId, impl_selection_class,
            named_impl_candidates, prove_selected_hir_application, resolve_trait_method_instance,
        },
        trait_resolution::{GoalSatisfiability, Selection, TraitSolveCx, is_goal_satisfiable},
    },
    hir_def::IdentId,
    test_db::{HirAnalysisTestDb, find_func},
};

#[test]
fn named_declarations_do_not_enter_ordinary_discovery_or_coherence() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "named_declarations.fe".into(),
        r#"
trait Pick { fn value(self) -> u256 }
struct Subject {}
impl Pick for Subject { fn value(self) -> u256 { 1 } }
impl Pick for Subject as First { fn value(self) -> u256 { 2 } }
impl Pick for Subject as Second { fn value(self) -> u256 { 3 } }
fn request(_ subject: Subject) {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let items = module.all_impl_traits(&db);
    let request = find_func(&db, module, "request");
    let subject = request.params(&db).next().unwrap().ty(&db);
    let subject = subject.as_view(&db).unwrap_or(subject);
    let goal = TraitInstId::new_simple(&db, module.all_traits(&db)[0], vec![subject]);
    let cx = TraitSolveCx::new(&db, request.scope());
    assert_eq!(
        impl_selection_class(&db, items[0]),
        ImplSelectionClass::Ordinary
    );
    let method_name = IdentId::new(&db, "value");
    let Selection::Unique(ordinary) = resolve_trait_method_instance(
        &db,
        cx,
        goal,
        method_name,
        None,
        fe_hir::analysis::ty::trait_choice::OrdinarySelection::for_tests(),
    ) else {
        panic!("named alternatives must not alter ordinary selection")
    };
    assert_eq!(ordinary.body(), items[0].methods(&db).next());
    for (idx, name) in [(1, "First"), (2, "Second")] {
        let name = IdentId::new(&db, name);
        assert_eq!(items[idx].selection_name(&db), Some(name));
        assert_eq!(
            impl_selection_class(&db, items[idx]),
            ImplSelectionClass::Named(name)
        );
        assert_eq!(
            named_impl_candidates(&db, request.scope(), name),
            vec![items[idx]]
        );
        assert!(
            items[idx]
                .pretty_print(&db)
                .contains(&format!(" as {}", name.data(&db)))
        );
        let checked = prove_selected_hir_application(&db, cx, items[idx], goal).unwrap();
        let Selection::Unique(method) = checked.method(method_name) else {
            panic!("checked named application must retain its method")
        };
        assert_eq!(method.body(), items[idx].methods(&db).next());
    }
}

#[test]
fn named_only_declaration_needs_explicit_proof_and_cannot_borrow_another_root() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "named_prerequisites.fe".into(),
        r#"
trait Leaf {}
trait Pick {}
trait Alone {}
struct Subject {}
impl Pick for Subject {}
impl<T: Leaf> Pick for T as NeedsLeaf {}
impl Alone for Subject as Only {}
fn request(_ subject: Subject) {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let request = find_func(&db, module, "request");
    let subject = request.params(&db).next().unwrap().ty(&db);
    let subject = subject.as_view(&db).unwrap_or(subject);
    let traits = module.all_traits(&db);
    let cx = TraitSolveCx::new(&db, request.scope());
    let pick = TraitInstId::new_simple(&db, traits[1], vec![subject]);
    assert!(matches!(
        is_goal_satisfiable(&db, cx, pick),
        GoalSatisfiability::Satisfied(_)
    ));
    let named = named_impl_candidates(&db, request.scope(), IdentId::new(&db, "NeedsLeaf"))[0];
    assert_eq!(
        prove_selected_hir_application(&db, cx, named, pick).unwrap_err(),
        CheckedApplicationError::NotProven
    );
    let alone = TraitInstId::new_simple(&db, traits[2], vec![subject]);
    assert!(matches!(
        is_goal_satisfiable(&db, cx, alone),
        GoalSatisfiability::UnSat(_)
    ));
    let only = named_impl_candidates(&db, request.scope(), IdentId::new(&db, "Only"))[0];
    assert!(prove_selected_hir_application(&db, cx, only, alone).is_ok());
    assert!(named_impl_candidates(&db, request.scope(), IdentId::new(&db, "impl#1")).is_empty());
}

fn diag_codes(source: &str) -> Vec<String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("reproducer.fe".into(), source);
    let (module, _) = db.top_mod(file);
    fe_hir::analysis::initialize_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|diag| diag.to_complete(&db).error_code.to_string())
        .collect()
}

#[test]
fn for_loop_protocol_calls_refuse_evidence_in_receiver_type_param() {
    // R9C-1a (NoEvidence operand refusal on `for x in o`):
    // `Once::has` opens `with (self.e)` on type parameter `T`. Passing
    // `Once<Evidence<Price<Item>>>` to `for x in o` must be refused with 8-0098
    // just like explicit `o.has(0)`.
    let no_evidence_loop = r#"
use core::iter::Collection
use core::obligations::{Evidence, prove}

trait Price { fn amount() -> usize }
struct Item {}
impl Price for Item { fn amount() -> usize { 100 } }
impl Price for Item as Wholesale { fn amount() -> usize { 70 } }

struct Once<T> {
    vals: [usize; 1],
    e: T,
}

impl<T> Collection for Once<T> {
    type Item = usize
    type Cursor = usize

    fn start(self) -> usize { 0 }
    fn has(self, _ c: usize) -> bool {
        with (self.e) {
            c == 0 && Item::amount() == 70
        }
    }
    fn next(self, _ c: usize) -> usize { 1 }
    fn at(mut? self, _ c: usize) -> mut? usize { mut? self.vals[0] }
}

fn run() -> usize {
    let e: Evidence<Price<Item>> = with (Wholesale) { prove<Price<Item>>() }
    let o = Once { vals: [70], e }
    var n: usize = 0
    for x in o {
        n = n + x
    }
    n
}
"#;
    assert!(
        diag_codes(no_evidence_loop).contains(&"8-0098".to_string()),
        "expected 8-0098 for evidence passed through `for` loop receiver type param, got {:?}",
        diag_codes(no_evidence_loop)
    );
}

#[test]
fn for_loop_protocol_calls_refuse_type_reach_under_with() {
    // R9C-1a (TypeReach refusal on `for x in o` inside `with`):
    // `Once::start` forms `[usize; T::N]` from opened trait `Width`.
    let type_reach_loop = r#"
use core::iter::Collection

trait Width { const N: usize }
struct Item {}
impl Width for Item { const N: usize = 2 }
impl Width for Item as Wide { const N: usize = 5 }

struct Once<T> {
    vals: [usize; 8],
}

impl<T: Width> Collection for Once<T> {
    type Item = usize
    type Cursor = usize

    fn start(self) -> usize {
        let a: [usize; T::N] = [0; T::N]
        a[0]
    }
    fn has(self, _ c: usize) -> bool { c < 1 }
    fn next(self, _ c: usize) -> usize { c + 1 }
    fn at(mut? self, _ c: usize) -> mut? usize { mut? self.vals[c] }
}

fn run(_ o: Once<Item>) -> usize {
    with (Wide) {
        var got: usize = 0
        for x in o {
            got = x
        }
        got
    }
}
"#;
    assert!(
        diag_codes(type_reach_loop).contains(&"8-0122".to_string()),
        "expected 8-0122 for `for` loop protocol call reaching type-level const under `with`, got {:?}",
        diag_codes(type_reach_loop)
    );
}

#[test]
fn for_loop_protocol_calls_refuse_opened_condition_under_with() {
    // R9C-1a (OpenedCondition refusal on `for x in o` inside `with`):
    // `Once::start` relies on conditional `impl<T: Price> Bonus for T` under `with (Wholesale)`.
    let opened_condition_loop = r#"
use core::iter::Collection

trait Price { fn amount() -> usize }
trait Bonus { fn bonus() -> usize }
struct Item {}
impl Price for Item { fn amount() -> usize { 100 } }
impl Price for Item as Wholesale { fn amount() -> usize { 70 } }
impl<T: Price> Bonus for T { fn bonus() -> usize { T::amount() } }

fn call_bonus<T: Bonus>() -> usize { T::bonus() }

struct Once<T> {
    vals: [usize; 1],
}

impl<T: Price> Collection for Once<T> {
    type Item = usize
    type Cursor = usize

    fn start(self) -> usize {
        let _ = call_bonus<T>()
        0
    }
    fn has(self, _ c: usize) -> bool { c < 1 }
    fn next(self, _ c: usize) -> usize { c + 1 }
    fn at(mut? self, _ c: usize) -> mut? usize { mut? self.vals[0] }
}

fn run(_ o: Once<Item>) -> usize {
    with (Wholesale) {
        var got: usize = 0
        for x in o {
            got = x
        }
        got
    }
}
"#;
    assert!(
        diag_codes(opened_condition_loop).contains(&"8-0098".to_string()),
        "expected 8-0098 for `for` loop protocol call with opened condition under `with`, got {:?}",
        diag_codes(opened_condition_loop)
    );
}

#[test]
fn for_loop_generic_cursor_state_refuses_evidence_at_caller() {
    // R9C-1b (Generic loop cursor state `NoEvidence` across caller boundary):
    // `loop_generic<C: Collection>` has cursor state `<C as Collection>::Cursor`
    // which is only known to hold `Evidence` when instantiated with `Once` at the call site.
    let generic_cursor_state = r#"
use core::iter::Collection
use core::obligations::{Evidence, prove}

trait Price { fn amount() -> usize }
struct Item {}
impl Price for Item { fn amount() -> usize { 100 } }
impl Price for Item as Wholesale { fn amount() -> usize { 70 } }

struct Once {
    vals: [usize; 1],
}

impl Collection for Once {
    type Item = usize
    type Cursor = Evidence<Price<Item>>

    fn start(self) -> Evidence<Price<Item>> {
        with (Wholesale) { prove<Price<Item>>() }
    }
    fn has(self, _ c: Evidence<Price<Item>>) -> bool {
        with (c) { Item::amount() == 70 }
    }
    fn next(self, _ c: Evidence<Price<Item>>) -> Evidence<Price<Item>> { c }
    fn at(mut? self, _ c: Evidence<Price<Item>>) -> mut? usize { mut? self.vals[0] }
}

fn loop_generic<C: Collection<Item = usize>>(_ c: C) -> usize {
    var n: usize = 0
    for x in c {
        n = n + x
    }
    n
}

fn run() -> usize {
    loop_generic(Once { vals: [70] })
}
"#;
    assert!(
        diag_codes(generic_cursor_state).contains(&"8-0098".to_string()),
        "expected 8-0098 when instantiating generic `for` loop cursor state with Evidence, got {:?}",
        diag_codes(generic_cursor_state)
    );
}

#[test]
fn written_frames_resolves_block_local_const_shadowing_disjoint_let() {
    // R9C-2a: Block-local `const e` must not be misattributed to a disjoint block's `let e`.
    let block_const_shadow = r#"
use core::obligations::{Evidence, prove}

trait Price { const P: usize }
trait Width { const N: usize }
struct Item {}
impl Price for Item { const P: usize = 100 }
impl Price for Item as Wholesale { const P: usize = 70 }
impl Width for Item { const N: usize = 2 }
impl Width for Item as Wide { const N: usize = 5 }

fn run() -> (usize, usize, usize) {
    {
        let e: Evidence<Price<Item>> = prove<Price<Item>>()
        let _ = e
    }
    {
        const e: Evidence<Width<Item>> = with (Wide) { prove<Width<Item>>() }
        with (e) {
            const C: usize = Item::N
            fn inner() -> usize { Item::N }
            (Item::N, C, inner())
        }
    }
}
"#;
    assert!(
        diag_codes(block_const_shadow).contains(&"8-0098".to_string()),
        "expected 8-0098 when block-local `const e` is opened around written const/fn despite disjoint `let e`, got {:?}",
        diag_codes(block_const_shadow)
    );
}

#[test]
fn written_frames_resolves_closure_param_shadowing_outer_let() {
    // R9C-2b: Closure parameter `e: Evidence<Width<Item>>` must not be misattributed to an outer
    // `let e: Evidence<Price<Item>>` (which on 63cf87210 falsely refuses `[u8; Item::P]` with 8-0122).
    let closure_param_shadow = r#"
use core::obligations::{Evidence, prove}

trait Price { const P: usize }
trait Width { const N: usize }
struct Item {}
impl Price for Item { const P: usize = 2 }
impl Price for Item as Wholesale { const P: usize = 1 }
impl Width for Item { const N: usize = 2 }
impl Width for Item as Wide { const N: usize = 5 }

fn run() -> usize {
    let e: Evidence<Price<Item>> = prove<Price<Item>>()
    let _ = e
    let f = |_ e: Evidence<Width<Item>>| -> usize {
        with (e) {
            let arr: [u8; Item::P] = [0; 2]
            let _ = arr
            0
        }
    }
    f(with (Wide) { prove<Width<Item>>() })
}
"#;
    assert!(
        !diag_codes(closure_param_shadow).contains(&"8-0122".to_string()),
        "expected closure param `e: Evidence<Width<Item>>` not to open outer `let e: Evidence<Price<Item>>` around `[u8; Item::P]` (no 8-0122), got {:?}",
        diag_codes(closure_param_shadow)
    );
}

#[test]
fn written_frames_distinguishes_unit_variant_pat_from_local_binding() {
    // R9C-2c: Imported unit enum variant `e` in `match tag { e => ... }` before `let e` is a
    // variant pattern, not a local binding; on 63cf87210 `local_evidence_goal` matched the
    // `match`-arm `Pat::Path(e)` first, returned `None` (`frames.unknown = true`), and falsely
    // refused unrelated `Item::P` inside `with (e)` with `8-0098`.
    let unit_variant_pat = r#"
use core::obligations::{Evidence, prove}

enum Tag {
    e,
    Other,
}

trait Price { const P: usize }
trait Width { const N: usize }
struct Item {}
impl Price for Item { const P: usize = 100 }
impl Price for Item as Wholesale { const P: usize = 70 }
impl Width for Item { const N: usize = 2 }
impl Width for Item as Wide { const N: usize = 5 }

fn run(_ tag: Tag) -> usize {
    {
        use Tag::e
        match tag {
            e => {}
            Tag::Other => {}
        }
    }
    let e: Evidence<Width<Item>> = with (Wide) { prove<Width<Item>>() }
    with (e) {
        const P_OK: usize = Item::P
        P_OK
    }
}
"#;
    assert!(
        diag_codes(unit_variant_pat).is_empty(),
        "expected unit variant pattern `e` before `let e` not to make `with (e)` unknown and falsely refuse unrelated `const P_OK = Item::P`, got {:?}",
        diag_codes(unit_variant_pat)
    );
}

#[test]
fn named_only_assoc_const_under_with_ignores_out_of_scope_trait_with_same_const_name() {
    let src = r#"
pub struct Item {}

mod hidden {
    use super::Item
    pub trait Hidden {
        const PRICE: u32
    }
    impl Hidden for Item {
        const PRICE: u32 = 999
    }
}

trait Price {
    const PRICE: u32
}

impl Price for Item as Wholesale {
    const PRICE: u32 = 70
}

const fn check() -> u32 {
    with (Wholesale) {
        Item::PRICE
    }
}

struct Verify<const N: u32> where { N == 70 } {}
fn verify(_ v: Verify<{ check() }>) {}
"#;
    assert!(
        diag_codes(src).is_empty(),
        "expected `Item::PRICE` inside `with (Wholesale)` to select in-scope `Price` (70) despite out-of-scope `hidden::Hidden`, got {:?}",
        diag_codes(src)
    );
}

#[test]
fn no_evidence_debt_refuses_composite_and_projected_types_holding_evidence() {
    let src = r#"
use core::obligations::{Evidence, prove}

trait Price { fn p() -> u256 }
struct Item {}
impl Price for Item { fn p() -> u256 { 10 } }
impl Price for Item as Alt { fn p() -> u256 { 20 } }

struct Wrap<T> {
    pub x: T,
}

trait Holder {
    type Assoc
}

struct EvHolder {}
impl Holder for EvHolder {
    type Assoc = Evidence<Price<Item>>
}

fn call_closure_with_wrap<T>(_ w: Wrap<T>) {
    let f = |_ v: Wrap<T>| {}
    f(w)
}

fn open_wrap<T>(_ w: Wrap<T>) {
    with (w) {}
}

fn open_projected<H: Holder>(_ x: H::Assoc) {
    with (x) {}
}

pub fn leak_composite_to_closure() {
    let ev: Evidence<Price<Item>> = with (Alt) { prove<Price<Item>>() }
    call_closure_with_wrap(Wrap { x: ev })
}

pub fn leak_composite_to_with() {
    let ev: Evidence<Price<Item>> = with (Alt) { prove<Price<Item>>() }
    open_wrap(Wrap { x: ev })
}

pub fn leak_projected_to_with() {
    let ev: Evidence<Price<Item>> = with (Alt) { prove<Price<Item>>() }
    open_projected<EvHolder>(ev)
}
"#;
    let codes = diag_codes(src);
    assert_eq!(
        codes.iter().filter(|c| *c == "8-0098").count(),
        3,
        "expected 3 `8-0098` refusals for composite and projected types holding evidence, got {codes:?}"
    );
}

#[test]
fn nested_items_inside_with_refuse_transitive_type_slot_reads() {
    let src = r#"
trait Price { const P: usize }
struct Item {}
impl Price for Item { const P: usize = 2 }
impl Price for Item as Wholesale { const P: usize = 4 }

struct Holder<T: Price> {
    pub buf: [u8; T::P],
}

pub fn nested_fn_in_with() {
    with (Wholesale) {
        fn inner(_ h: Holder<Item>) {}
    }
}

pub fn nested_struct_in_with() {
    with (Wholesale) {
        struct Inner {
            pub h: Holder<Item>,
        }
    }
}

pub fn nested_const_in_with() {
    with (Wholesale) {
        const C: usize = {
            let _h = Holder<Item> { buf: [0; 2] }
            0
        }
    }
}
"#;
    let codes = diag_codes(src);
    assert_eq!(
        codes.iter().filter(|c| *c == "8-0098").count(),
        3,
        "expected 3 `8-0098` refusals for nested items inside `with` referencing choice-dependent `Holder<Item>`, got {codes:?}"
    );
}
