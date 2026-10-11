// Associated types with parameters checked against their plain twins,
// shared by the analysis test (`crates/hir/tests/traits/twin_families.rs`)
// and the command-line test (`crates/fe/tests/family_twin_table.rs`), which runs
// everything `fe check` runs, instance building and borrow checking
// included.
//
// A family application means exactly what a plain associated type with the
// same definition means, so the plain program is the oracle: each entry is
// one source with `⟨family|plain⟩` alternatives, and the two programs must
// give the same error codes, as many times each. `%P`, `%H` and `%A` stand
// for `Fac::P<u8>`, `Fac::H<u8>` and `Fac::A<u256>` (plain: `Fac::P`, ...);
// `%QP` and `%QH` for the qualified spellings, and `%BAD` and `%QBAD` for
// `Fac::K<Bad<Unmet>>`, whose definition ignores an argument that is not
// well formed. The prelude defines them.
//
// Metamorphic checks need no oracle: each family program gives the same
// codes with its items in reverse order (and, in the analysis test,
// in a database edited from program to program and in a fresh one).

const FAMILY_PRELUDE: &str = "struct Holder<T> { t: T }
trait Factory {
    type P<T>
    type H<T>
    type A<T>
    type K<T>
}
struct Fac {}
impl Factory for Fac {
    type P<T> = (bool, T)
    type H<T> = Holder<T>
    type A<T> = [T; 3]
    type K<T> = u8
}
";

const PLAIN_PRELUDE: &str = "struct Holder<T> { t: T }
trait Factory {
    type P
    type H
    type A
    type K
}
struct Fac {}
impl Factory for Fac {
    type P = (bool, u8)
    type H = Holder<u8>
    type A = [u256; 3]
    type K = u8
}
";

/// Codes that only an internal error or a crash produces.
const INTERNAL_CODES: &[&str] = &["16-0004", "17-0100", "crash"];

#[derive(Clone, Copy, Debug)]
enum Expect {
    /// The family program gives the codes of its plain twin.
    Agree,
    /// The family program gives exactly these codes (no plain twin exists).
    Exactly(&'static [&'static str]),
    /// Known to differ from the twin, or not yet decided, for the reason
    /// given. Both programs are still checked, and must not crash or give an
    /// internal error; the codes are not compared.
    Known(&'static str),
}

struct Twin {
    name: &'static str,
    expect: Expect,
    src: &'static str,
}

/// The family (`true`) or plain program of `src`.
fn variant(src: &str, family: bool) -> String {
    let mut out = String::new();
    let mut rest = src;
    while let Some(start) = rest.find('⟨') {
        out.push_str(&rest[..start]);
        let after = &rest[start + '⟨'.len_utf8()..];
        let end = after.find('⟩').expect("unclosed alternative");
        let (fam, plain) = after[..end].split_once('|').expect("alternative needs `|`");
        out.push_str(if family { fam } else { plain });
        rest = &after[end + '⟩'.len_utf8()..];
    }
    out.push_str(rest);
    let subst: &[(&str, &str, &str)] = &[
        (
            "%QBAD",
            "<Fac as Factory>::K<Bad<Unmet>>",
            "<Fac as Factory>::K",
        ),
        ("%BAD", "Fac::K<Bad<Unmet>>", "Fac::K"),
        ("%QP", "<Fac as Factory>::P<u8>", "<Fac as Factory>::P"),
        ("%QH", "<Fac as Factory>::H<u8>", "<Fac as Factory>::H"),
        ("%P", "Fac::P<u8>", "Fac::P"),
        ("%H", "Fac::H<u8>", "Fac::H"),
        ("%A", "Fac::A<u256>", "Fac::A"),
    ];
    for (key, fam, plain) in subst {
        out = out.replace(key, if family { fam } else { plain });
    }
    let prelude = if family {
        FAMILY_PRELUDE
    } else {
        PLAIN_PRELUDE
    };
    // `use` items stay first.
    let (uses, body): (Vec<&str>, Vec<&str>) = out.lines().partition(|l| l.starts_with("use "));
    format!("{}\n{prelude}{}\n", uses.join("\n"), body.join("\n"))
}

/// `src` with its top-level items in reverse order (`use` items first). An
/// item starts at a line in column 0 outside braces; attributes stay with
/// the item after them.
fn reversed(src: &str) -> String {
    let mut items: Vec<Vec<&str>> = Vec::new();
    let mut attrs: Vec<&str> = Vec::new();
    let mut depth = 0i32;
    for line in src.lines() {
        let starts = depth == 0
            && line
                .chars()
                .next()
                .is_some_and(|c| !c.is_whitespace() && !"})]".contains(c));
        if starts && line.starts_with("#[") {
            attrs.push(line);
        } else if starts {
            let mut item = std::mem::take(&mut attrs);
            item.push(line);
            items.push(item);
        } else if let Some(item) = items.last_mut() {
            item.push(line);
        }
        depth += line.matches('{').count() as i32 - line.matches('}').count() as i32;
    }
    let (uses, mut rest): (Vec<_>, Vec<_>) = items
        .into_iter()
        .partition(|item| item[0].starts_with("use "));
    rest.reverse();
    uses.into_iter()
        .chain(rest)
        .map(|item| item.join("\n"))
        .collect::<Vec<_>>()
        .join("\n")
        + "\n"
}

/// Checks `twin`; returns its status line, and whether it holds. With
/// `reorder`, the family program must also give the same codes with its
/// items in reverse order.
fn check_twin(twin: &Twin, codes: &dyn Fn(&str) -> Vec<String>, reorder: bool) -> (String, bool) {
    let family = variant(twin.src, true);
    let plain = variant(twin.src, false);
    let fam_codes = codes(&family);
    let plain_codes = codes(&plain);
    let mut notes = Vec::new();
    if reorder {
        let again = codes(&reversed(&family));
        if again != fam_codes {
            notes.push(format!("family in reverse order gives {again:?}"));
        }
    }
    let internal = |c: &[String]| c.iter().any(|code| INTERNAL_CODES.contains(&code.as_str()));
    let holds = match twin.expect {
        Expect::Agree => fam_codes == plain_codes,
        Expect::Known(_) => !internal(&fam_codes) && !internal(&plain_codes),
        Expect::Exactly(expected) => {
            let mut expected: Vec<String> = expected.iter().map(|c| c.to_string()).collect();
            expected.sort();
            fam_codes == expected
        }
    } && notes.is_empty();
    let status = match (twin.expect, holds) {
        (_, true) => format!("ok     {}", twin.name),
        (Expect::Known(reason), false) => format!(
            "CRASH  {} (known to differ: {reason}): family {fam_codes:?}, plain {plain_codes:?}",
            twin.name
        ),
        (expect, false) => format!(
            "FAIL   {} ({expect:?}): family {fam_codes:?}, plain {plain_codes:?} {}\n{family}",
            twin.name,
            notes.join("; ")
        ),
    };
    (status, holds)
}

/// Checks every entry, `threads` at a time, and fails with the status lines
/// of those that do not hold.
fn check_all_twins(codes: &(dyn Fn(&str) -> Vec<String> + Sync), threads: usize, reorder: bool) {
    let next = std::sync::atomic::AtomicUsize::new(0);
    let results = std::sync::Mutex::new(Vec::new());
    std::thread::scope(|scope| {
        for _ in 0..threads {
            scope.spawn(|| {
                loop {
                    let idx = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    let Some(twin) = TWINS.get(idx) else { break };
                    let result = check_twin(twin, codes, reorder);
                    results.lock().unwrap().push((idx, result));
                }
            });
        }
    });
    let mut results = results.into_inner().unwrap();
    results.sort_by_key(|(idx, _)| *idx);
    let mut failures = Vec::new();
    for (_, (status, holds)) in &results {
        if !holds {
            failures.push(status.clone());
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

macro_rules! twins {
    ($($name:ident: $expect:expr, $src:expr;)*) => {
        const TWINS: &[Twin] = &[$(Twin { name: stringify!($name), expect: $expect, src: $src },)*];
    };
}

use Expect::*;

const OWN_PARAMETER: &str = "an impl header that needs the impl's own parameter keeps the \
                             application after lowering, so it can be neither found nor checked \
                             for overlap; whether it is rejected at the impl or overlaps every \
                             header is not decided (the twin writes the type directly)";

twins! {
    // Positions where a type is written.
    signature: Agree, "fn f(_ x: %P) -> (bool, u8) { x }
fn g() -> %P { (true, 1) }";
    struct_field: Agree, "struct S { p: %P }
fn f(_ s: S) -> u8 { s.p.1 }";
    enum_payload: Agree, "enum E { A(%P), B }
fn mk() -> E { E::A((true, 42)) }
fn val(_ e: E) -> u8 {
    match e {
        E::A((_, v)) => v
        E::B => 0
    }
}";
    enum_payload_struct: Agree, "enum E { A(%H), B }
fn mk() -> E { E::A(Holder { t: 5 }) }";
    record_variant: Agree, "enum E { R { p: %P }, B }
fn mk() -> E { E::R { p: (true, 5) } }";
    struct_array_field: Agree, "struct Rec { a: %A }
fn sum(_ r: Rec) -> u256 { r.a[0] + r.a[1] + r.a[2] }";
    alias: Agree, "type X = %P
fn f(_ x: X) -> (bool, u8) { x }";
    const_item: Agree, "const C: %P = (true, 3)
const D: u8 = C.1";
    locals_arrays_tuples: Agree, "fn f() -> u8 {
    let a: [%P; 2] = [(true, 1), (false, 2)]
    let t: (u8, %P) = (3, (true, 4))
    a[1].1 + t.1.1
}";
    array_family_local: Agree, "fn f() -> u256 {
    let arr: %A = [1, 2, 3]
    let mut s: u256 = 0
    for x in arr {
        s += x
    }
    s + arr[1] + arr.len() as u256
}";
    where_subject: Agree, "trait Show {}
impl Show for (bool, u8) {}
fn f() where %P: Show {}
fn g() { f() }";
    where_subject_qualified: Agree, "trait Show {}
impl Show for (bool, u8) {}
fn f() where ⟨%QP|%P⟩: Show {}
fn g() { f() }";
    bound_argument: Agree, "trait Tr2<X> {}
impl Tr2<(bool, u8)> for u8 {}
fn f<T: Tr2<%P>>() {}
fn g() { f<u8>() }";
    generic_default: Agree, "struct D<T = %P> { t: T }
fn f(_ d: D) -> u8 { d.t.1 }";
    impl_trait_argument: Agree, "trait Tr2<X> {}
impl Tr2<%P> for bool {}
fn need<T: Tr2<(bool, u8)>>() {}
fn f() { need<bool>() }";
    effect_param: Agree, "fn f() -> u8 uses (c: %H) { c.t }
fn g() -> u8 {
    let h: %H = Holder { t: 4 }
    with (h) { f() }
}";
    effect_param_mut: Agree, "fn bump() uses (c: mut %H) { c.t += 1 }
fn g() -> u8 {
    let mut h: %H = Holder { t: 4 }
    with (h) { bump() }
    h.t
}";
    assoc_default: Agree, "trait D1 { type A = %P }
impl D1 for bool {}
fn f(_ x: <bool as D1>::A) -> (bool, u8) { x }";
    assoc_const_type: Agree, "trait C1 { const C: %P }
impl C1 for bool { const C: (bool, u8) = (true, 1) }";
    borrow_mut: Agree, "fn bump(_ c: mut %H) { c.t += 1 }
fn f() -> u8 {
    let mut c: %H = Holder { t: 1 }
    bump(mut c)
    c.t
}";
    // Paths in expressions and patterns.
    expr_call: Agree, "impl<T> Holder<T> { fn make(_ t: T) -> Holder<T> { Holder { t } } }
fn f() -> u8 { %H::make(1).t }";
    expr_qualified_call: Agree, "impl<T> Holder<T> { fn make(_ t: T) -> Holder<T> { Holder { t } } }
fn f() -> u8 { ⟨<Fac as Factory>::H<u8>|Fac::H⟩::make(2).t }";
    expr_record_and_pattern: Agree, "fn f() -> u8 {
    let %H { t } = %H { t: 6 }
    t
}";
    expr_inherent_const: Agree, "impl<T> Holder<T> { const K: u256 = 3 }
fn f() -> u256 { %H::K }";
    expr_enum_variants: Agree, "enum Opt<T> { Some(T), None }
trait Fo { type O⟨<T>|⟩ }
impl Fo for Fac { type O⟨<T> = Opt<T>| = Opt<u8>⟩ }
fn a() -> Fac::O⟨<u8>|⟩ { Fac::O⟨<u8>|⟩::Some(3) }
fn b(_ x: Fac::O⟨<u8>|⟩) -> u8 {
    match x {
        Fac::O⟨<u8>|⟩::Some(v) => v
        Fac::O⟨<u8>|⟩::None => 0
    }
}";
    // Constants and computed types.
    assoc_const_of_result: Agree, "trait HasK { const K: u256 }
impl<T> HasK for Holder<T> { const K: u256 = 11 }
fn f() -> u256 { <%H as HasK>::K }";
    array_length_from_result: Agree, "trait HasLen { const LEN: usize }
impl<T> HasLen for Holder<T> { const LEN: usize = 5 }
fn f() -> usize {
    let a: [u8; <%H as HasLen>::LEN] = [0; 5]
    a.len()
}";
    const_fn: Agree, "const fn mk(_ x: u8) -> %P { (true, x) }
const C: %P = mk(3)";
    const_generic_argument: Agree, "trait HasK { const K: u256 }
impl<T> HasK for Holder<T> { const K: u256 = 4 }
struct N<const V: u256> {}
fn f() { let _n = N<{ <%H as HasK>::K }> {} }";
    // Contracts and messages.
    msg_field: Agree, "msg M {
    #[selector = 2]
    Sum { t: %P } -> u8,
}
pub contract C {
    recv M {
        Sum { t } -> u8 { t.1 }
    }
}";
    // The qualified spelling, against the plain shorthand.
    enum_payload_qualified: Agree, "enum E { A(⟨%QP|%P⟩), B }
fn mk() -> E { E::A((true, 42)) }";
    struct_array_field_qualified: Agree, "struct Rec { a: ⟨<Fac as Factory>::A<u256>|Fac::A⟩ }
fn sum(_ r: Rec) -> u256 { r.a[0] + r.a[1] + r.a[2] }";
    msg_field_qualified: Agree, "msg M {
    #[selector = 2]
    Sum { t: ⟨%QP|%P⟩ } -> u8,
}
pub contract C {
    recv M {
        Sum { t } -> u8 { t.1 }
    }
}";
    contract_field_qualified: Agree, "struct Store<T> { a: T, b: T }
trait Fs { type S⟨<T>|⟩ }
impl Fs for Fac { type S⟨<T> = Store<T>| = Store<u256>⟩ }
msg M {
    #[selector = 2]
    Sum -> u256,
}
pub contract C {
    mut store: ⟨<Fac as Fs>::S<u256>|Fac::S⟩
    init() uses (mut store) {
        store.a = 1
        store.b = 2
    }
    recv M {
        Sum -> u256 uses (store) { store.a + store.b }
    }
}";
    msg_return: Agree, "msg M {
    #[selector = 3]
    Small { t: u8 } -> %P,
}
pub contract C {
    recv M {
        Small { t } -> %P { (true, t) }
    }
}";
    contract_field_struct: Agree, "struct Store<T> { a: T, b: T }
trait Fs { type S⟨<T>|⟩ }
impl Fs for Fac { type S⟨<T> = Store<T>| = Store<u256>⟩ }
msg M {
    #[selector = 2]
    Sum -> u256,
}
pub contract C {
    mut store: Fac::S⟨<u256>|⟩
    init() uses (mut store) {
        store.a = 1
        store.b = 2
    }
    recv M {
        Sum -> u256 uses (store) { store.a + store.b }
    }
}";
    contract_field_storage_map: Agree, "use std::evm::StorageMap
trait Fm { type Map⟨<V>|⟩ }
impl Fm for Fac { type Map⟨<V> = StorageMap<u256, V>| = StorageMap<u256, u256>⟩ }
msg M {
    #[selector = 1]
    Get { key: u256 } -> u256,
}
pub contract C {
    mut a: Fac::Map⟨<u256>|⟩,
    recv M {
        Get { key } -> u256 uses (a) { a.get(key: key) }
    }
}";
    contract_field_map_value: Agree, "use std::evm::StorageMap
trait Fv { type Val⟨<V>|⟩ }
impl Fv for Fac { type Val⟨<V> = V| = u256⟩ }
msg M {
    #[selector = 1]
    Get { key: u256 } -> u256,
}
pub contract C {
    mut a: StorageMap<u256, Fac::Val⟨<u256>|⟩>,
    recv M {
        Get { key } -> u256 uses (a) { a.get(key: key) }
    }
}";
    // Impl headers.
    trait_impl_header: Agree, "trait Marker { fn id() -> u8 }
impl Marker for %H { fn id() -> u8 { 7 } }
fn get<M: Marker>() -> u8 { M::id() }
fn f() -> u8 { get<Holder<u8>>() }";
    trait_impl_header_overlap: Agree, "trait Marker {}
impl Marker for %H {}
impl Marker for Holder<u8> {}";
    trait_impl_header_qualified: Agree, "trait Marker { fn id() -> u8 }
impl Marker for %QH { fn id() -> u8 { 7 } }
fn get<M: Marker>() -> u8 { M::id() }
fn f() -> u8 { get<Holder<u8>>() }";
    trait_impl_header_own_parameter: Agree, "trait Marker { fn id() -> u8 }
impl<T> Marker for ⟨Fac::H<T>|Holder<T>⟩ { fn id() -> u8 { 7 } }
fn get<M: Marker>() -> u8 { M::id() }
fn f() -> u8 { get<Holder<bool>>() }";
    trait_impl_header_needs_impl_parameter: Known(OWN_PARAMETER), "trait Marker {}
impl<F: Factory> Marker for ⟨F::H<u8>|F::H⟩ {}
impl Marker for Holder<u8> {}";
    inherent_impl_header: Agree, "impl %H {
    fn get(self) -> u8 { self.t }
}
fn f(_ h: Holder<u8>) -> u8 { h.get() }";
    inherent_impl_self_argument: Agree, "struct Box<G> { x: G }
impl Box<%P> {
    fn show(self) -> u8 { self.x.1 }
}
fn f(_ b: Box<(bool, u8)>) -> u8 { b.show() }";
    inherent_impl_self_argument_qualified: Agree, "struct Box<G> { x: G }
impl Box<%QP> {
    fn show(self) -> u8 { self.x.1 }
}
fn f(_ b: Box<(bool, u8)>) -> u8 { b.show() }";
    inherent_impl_own_parameter: Agree, "struct Box<G> { x: G }
impl<X> Box<⟨<Fac as Factory>::P<X>|(bool, X)⟩> {
    fn flag(self) -> bool { self.x.0 }
}
fn f(_ b: Box<(bool, u16)>) -> bool { b.flag() }";
    method_probe_cycle: Agree, "struct W1<T> {}
enum Payload { A(%P) }
impl W1<%P> {
    fn m(self) -> u8 { 1 }
}
fn g(_ w: W1<(bool, u8)>) -> u8 { w.m() }";
    method_self_resolved_impl_type: Agree, "trait Fr { type Plain }
impl Fr for Fac { type Plain = u8 }
struct Hd<G> { x: G }
impl Hd<⟨<Fac as Fr>::Plain|u8⟩> {
    fn show(self) -> u8 { self.x }
    fn take(own self) -> u8 { self.x }
}
fn f(_ h: Hd<u8>) -> u8 { h.show() }";
    // Generic code.
    default_and_override: Agree, "trait Fw {
    type Pair⟨<T> = (T, bool)| = (u8, bool)⟩
    fn wrap(_ x: u8) -> Self::Pair⟨<u8>|⟩
}
struct A {}
impl Fw for A {
    fn wrap(_ x: u8) -> Self::Pair⟨<u8>|⟩ { (x, true) }
}
struct B {}
impl Fw for B {
    type Pair⟨<T> = (bool, T)| = (bool, u8)⟩
    fn wrap(_ x: u8) -> Self::Pair⟨<u8>|⟩ { (false, x) }
}
fn helper<F: Fw>(_ x: u8) -> F::Pair⟨<u8>|⟩ { F::wrap(x) }
fn f() -> u8 { helper<A>(1).0 + helper<B>(2).1 }";
    default_not_assumed: Agree, "trait Fd { type Pair⟨<T> = (T, bool)| = (u8, bool)⟩ }
fn bad<F: Fd>(_ x: F::Pair⟨<u8>|⟩) -> (u8, bool) { x }";
    generic_mismatch: Agree, "trait Fg {
    type Pair⟨<T>|⟩
    type Q
}
fn a<F: Fg>(_ x: F::Pair⟨<u8>|⟩) -> F::⟨Pair<u16>|Q⟩ { x }";
    declared_bound_dispatch: Agree, "trait Get { fn get(self) -> u8 }
impl Get for (bool, u8) { fn get(self) -> u8 { self.1 } }
trait Fb {
    type Pair⟨<T>|⟩: Get
    fn mk() -> Self::Pair⟨<u8>|⟩
}
impl Fb for Fac {
    type Pair⟨<T> = (bool, u8)| = (bool, u8)⟩
    fn mk() -> Self::Pair⟨<u8>|⟩ { (true, 3) }
}
fn run<F: Fb>() -> u8 { F::mk().get() }
fn f() -> u8 { run<Fac>() }";
    supertrait: Agree, "trait Fs2 { type P2⟨<T>|⟩ }
trait Sub: Fs2 { fn g(_ x: Self::P2⟨<u8>|⟩) -> u8 }
impl Fs2 for Fac { type P2⟨<T> = (bool, T)| = (bool, u8)⟩ }
impl Sub for Fac { fn g(_ x: Self::P2⟨<u8>|⟩) -> u8 { x.1 } }
fn call<S: Sub>(_ x: S::P2⟨<u8>|⟩) -> u8 { S::g(x) }
fn f() -> u8 { call<Fac>((true, 9)) }";
    blanket_impl: Agree, "trait Marker {}
impl Marker for u8 {}
trait Fb2 { type P3⟨<U>|⟩ }
impl<T: Marker> Fb2 for T { type P3⟨<U> = (U, T)| = (bool, T)⟩ }
fn g(_ x: <u8 as Fb2>::P3⟨<bool>|⟩) -> (bool, u8) { x }";
    generic_impl_arguments: Agree, "struct G<T> {}
trait Fgi { type W⟨<U>|⟩ }
impl<T> Fgi for G<T> { type W⟨<U> = (U, T)| = (u8, T)⟩ }
fn ok<X>(_ x: <G<X> as Fgi>::W⟨<u8>|⟩) -> (u8, X) { x }
fn bad<X>(_ x: <G<X> as Fgi>::W⟨<u8>|⟩) -> (X, u8) { x }";
    impl_signature_expanded: Agree, "trait Fm2 {
    type Pair⟨<T>|⟩
    fn mk(_ t: u8) -> Self::Pair⟨<u8>|⟩
}
struct A {}
impl Fm2 for A {
    type Pair⟨<T> = (T, bool)| = (u8, bool)⟩
    fn mk(_ t: u8) -> (u8, bool) { (t, true) }
}";
    impl_signature_wrong: Agree, "trait Fm2 {
    type Pair⟨<T>|⟩
    fn mk(_ t: u8) -> Self::Pair⟨<u8>|⟩
}
struct A {}
impl Fm2 for A {
    type Pair⟨<T> = (T, bool)| = (u8, bool)⟩
    fn mk(_ t: u8) -> (bool, u8) { (true, t) }
}";
    chained: Agree, "trait Fc {
    type A2⟨<T>|⟩
    type B2⟨<T>|⟩
}
impl Fc for Fac {
    type A2⟨<T> = (T, u8)| = ((bool, u16), u8)⟩
    type B2⟨<T> = (bool, T)| = (bool, u16)⟩
}
fn mk() -> ⟨Fac::A2<Fac::B2<u16>>|Fac::A2⟩ { ((true, 300), 9) }";
    default_names_other_family: Agree, "trait Fo2 {
    type P4⟨<T>|⟩
    type Q4 = Self::P4⟨<u8>|⟩
}
impl Fo2 for Fac { type P4⟨<T> = (bool, T)| = (bool, u8)⟩ }
fn f(_ x: <Fac as Fo2>::Q4) -> (bool, u8) { x }";
    self_in_trait_body: Agree, "trait Fe2 {
    type Pair⟨<T> = (T, bool)| = (u8, bool)⟩
    fn mk(_ t: u8) -> Self::Pair⟨<u8>|⟩
    fn twice(_ v: u8) -> (Self::Pair⟨<u8>|⟩, Self::Pair⟨<u8>|⟩) {
        let a: Self::Pair⟨<u8>|⟩ = Self::mk(v)
        (a, Self::mk(v))
    }
}
struct A {}
impl Fe2 for A { fn mk(_ t: u8) -> Self::Pair⟨<u8>|⟩ { (t, true) } }
fn f() -> u8 { A::twice(3).1.0 }";
    // Families passed without their arguments; the plain twin is a struct.
    unapplied_constructor: Agree, "struct Pr<T> { a: bool, b: T }
struct One<G: * -> *> { g: G<u8> }
trait Fh2 { type Ctor<T> }
impl Fh2 for Fac { type Ctor<T> = (bool, T) }
fn f(_ x: One<⟨Fac::Ctor|Pr⟩>) -> u8 { x.g.⟨1|b⟩ }";
    unapplied_identity: Agree, "struct A {}
struct B {}
struct One<G: * -> *> { g: G<u8> }
trait Fi { type Pair<T> = (bool, T) }
impl Fi for A {}
impl Fi for B {}
struct Pa<T> {}
struct Pb<T> {}
fn pass(_ h: One<⟨A::Pair|Pa⟩>) -> One<⟨B::Pair|Pb⟩> { h }";
    constructor_with_extra_argument: Agree, "struct Box2<A, B> { a: A, b: B }
trait Fx { type Ctor⟨<T>|⟩: * -> * }
impl Fx for Fac { type Ctor⟨<T>|⟩ = Box2<u8> }
fn mk() -> ⟨Fac::Ctor<bool, u16>|<Fac as Fx>::Ctor<u16>⟩ { Box2 { a: 1, b: 300 } }";
    constructor_parameter: Agree, "struct Bx<T> { t: T }
trait Fa { type Ap⟨<F: * -> *>|⟩ }
impl Fa for Fac { type Ap⟨<F> = F<u8>| = Bx<u8>⟩ }
fn g(_ x: Fac::Ap⟨<Bx>|⟩) -> Bx<u8> { x }";
    // Programs both versions reject.
    over_application: Agree, "fn f(_ x: Fac::P<u8, u8>) {}";
    undeclared_in_impl: Agree, "trait Fu { type Pair⟨<T>|⟩ }
impl Fu for Fac {
    type Pair⟨<T> = (bool, T)| = u8⟩
    type Extra⟨<T> = T| = u8⟩
}";
    declared_binding_violated: Agree, "trait Tr2 { type X }
struct Foo {}
impl Tr2 for Foo { type X = u16 }
trait Fq { type F⟨<U>|⟩: Tr2<X = u8> }
impl Fq for Fac { type F⟨<U>|⟩ = Foo }";
    mutual_cycle: Agree, "trait Fz { type O⟨<T>|⟩ }
struct A {}
struct B {}
impl Fz for A { type O⟨<T>|⟩ = <B as Fz>::O⟨<T>|⟩ }
impl Fz for B { type O⟨<T>|⟩ = <A as Fz>::O⟨<T>|⟩ }
fn f(_ x: A::O⟨<u8>|⟩) {}";
    ambiguous_name: Agree, "trait F1 { type Out⟨<T>|⟩ }
trait F2 { type Out⟨<T>|⟩ }
struct S {}
impl F1 for S { type Out⟨<T> = (T, u8)| = (bool, u8)⟩ }
impl F2 for S { type Out⟨<T> = (T, u16)| = (bool, u16)⟩ }
fn f(_ x: S::Out⟨<bool>|⟩) {}";
    ambiguous_name_mixed_arity: Known("not decided whether `S::Out<bool>` is ambiguous or selects the only `Out` with a parameter; today it reports the argument count of the first candidate that applies"), "trait F1 { type Out⟨<T>|⟩ }
trait F2 { type Out }
struct S {}
impl F1 for S { type Out⟨<T> = (T, u8)| = (bool, u8)⟩ }
impl F2 for S { type Out = u16 }
fn f(_ x: S::Out⟨<bool>|⟩) {}";
    // A definition of another kind than the declared one: reported where it
    // is written (3-0065), and nothing at its uses.
    kind_of_definition_generic_use: Exactly(&["3-0065"]), "trait Fk { type Ctor<T>: * -> * }
struct B {}
impl Fk for B { type Ctor<T> = u8 }
fn g<F: Fk>(_ x: <F as Fk>::Ctor<bool, u8>) {}
fn h() { g<B>(1) }";
    kind_of_definition_generic_use_plain: Exactly(&["3-0065"]), "trait Fk { type Ctor: * -> * }
struct B {}
impl Fk for B { type Ctor = u8 }
fn g<F: Fk>(_ x: <F as Fk>::Ctor<u8>) {}
fn h() { g<B>(1) }";
    kind_of_definition_concrete_use: Exactly(&["3-0065"]), "trait Fk { type Ctor<T>: * -> * }
struct B {}
impl Fk for B { type Ctor<T> = u8 }
fn f(_ x: <B as Fk>::Ctor<bool, u8>) -> u8 { 1 }";
    kind_of_definition_concrete_use_plain: Exactly(&["3-0065"]), "trait Fk { type Ctor: * -> * }
struct B {}
impl Fk for B { type Ctor = u8 }
fn f(_ x: <B as Fk>::Ctor<u8>) -> u8 { x }";
    kind_of_definition_unused: Exactly(&["3-0065"]), "trait Fk { type Ctor<T>: * -> * }
struct B {}
impl Fk for B { type Ctor<T> = u8 }";
    kind_of_definition_unused_plain: Exactly(&["3-0065"]), "trait Fk { type Ctor: * -> * }
struct B {}
impl Fk for B { type Ctor = u8 }";
    kind_of_definition_constructor_for_type: Exactly(&["3-0065"]), "struct Box<T> { t: T }
trait Fk { type Out<T> }
struct B {}
impl Fk for B { type Out<T> = Box }
fn f(_ x: B::Out<u8>) {}";
    kind_of_trait_default: Exactly(&["3-0065"]), "trait Fk { type Ctor: * -> * = u16 }
struct B {}
impl Fk for B {}";
    // An argument that the definition ignores is still a written type: one
    // that is not well formed is reported, in every position and spelling.
    ill_formed_argument_signature: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
fn f(_ x: %BAD) {}";
    ill_formed_argument_signature_qualified: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
fn f(_ x: %QBAD) {}";
    ill_formed_argument_struct_field: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
struct S { a: %BAD }";
    ill_formed_argument_struct_field_qualified: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
struct S { a: %QBAD }";
    ill_formed_argument_enum_payload: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
enum E { A(%BAD), B }";
    ill_formed_argument_enum_payload_qualified: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
enum E { A(%QBAD), B }";
    ill_formed_argument_local: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
fn f() {
    let _x: %BAD = 1
}";
    ill_formed_argument_local_qualified: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
fn f() {
    let _x: %QBAD = 1
}";
    ill_formed_argument_trait_impl_header: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
trait Tr9 {}
impl Tr9 for %BAD {}";
    ill_formed_argument_trait_impl_header_qualified: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
trait Tr9 {}
impl Tr9 for %QBAD {}";
    ill_formed_argument_inherent_impl_argument: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
struct Box<G> { g: G }
impl Box<%BAD> {}";
    ill_formed_argument_inherent_impl_argument_qualified: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
struct Box<G> { g: G }
impl Box<%QBAD> {}";
    ill_formed_argument_effect_key: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
fn f() uses (x: %BAD) {}";
    ill_formed_argument_effect_key_qualified: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
fn f() uses (x: %QBAD) {}";
    ill_formed_argument_type_argument: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
struct Box<G> { g: G }
fn f(_ x: Box<%BAD>) {}";
    ill_formed_argument_type_argument_qualified: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
struct Box<G> { g: G }
fn f(_ x: Box<%QBAD>) {}";
    ill_formed_effect_key_without_associated_type: Exactly(&["6-0003"]), "trait Needed {}
struct Unmet {}
struct Bad<T: Needed> { t: T }
fn f() uses (x: Bad<Unmet>) {}";
    // Unsupported forms: one error at each declaration.
    const_parameter: Exactly(&["3-0060", "3-0060"]), "trait Fk { type Out<const N: usize> }
impl Fk for Fac { type Out<const N: usize> = [u8; N] }";
    parameter_default: Exactly(&["3-0064", "3-0064"]), "trait Fk { type Out<T = u8> }
impl Fk for Fac { type Out<T = u16> = T }";
    binding: Exactly(&["2-0021", "2-0021"]), "trait Fk { type Out<T> }
fn f<F: Fk<Out = u8>>() {}
fn g<F>() where F: Fk<Out = u8> {}";
    shadowed_parameter: Exactly(&["3-0007"]), "struct G<T> {}
trait Fk { type W<U> }
impl<T> Fk for G<T> { type W<T> = (T, bool) }";
    impl_parameter_bound: Exactly(&["6-0027"]), "trait Allowed {}
trait Fk { type F<U> }
impl Fk for Fac { type F<U: Allowed> = (U, U) }";
    // A name that does not resolve in a bound's arguments, nested or not:
    // the twin writes it directly as the argument.
    missing_name_nested_type: Agree, "trait Needs<X> {}
struct V<T> {}
fn c<T: Needs<⟨V<Missing>|Missing⟩>>() {}";
    missing_name_tuple: Agree, "trait Needs<X> {}
fn d<T: Needs<⟨(u8, Missing)|Missing⟩>>() {}";
    missing_name_array: Agree, "trait Needs<X> {}
fn e<T: Needs<⟨[Missing; 2]|Missing⟩>>() {}";
    missing_name_supertrait: Agree, "trait Needs<X> {}
trait Sup⟨: Needs<Missing>| where Self: Needs<Missing>⟩ {}";
}

/// Family-shaped rows of the limit matrix (`limit_families.rs`): the
/// question reaches its answer through a family application, at depth 3 and
/// 70, in both declaration orders, with the unrelated where clause false and
/// true. A family only renames a type, so each row expects what the plain
/// row it copies expects.
#[allow(dead_code)]
mod family_limit {
    include!("limit_families.rs");

    const FAM: &str = "trait Fam {
    type Id<T>
    type Keep<T>
    type Out<T: Nest>
}
struct S {}
impl Fam for S {
    type Id<T> = T
    type Keep<T> = u8
    type Out<T> = T::Out
}
";

    fn rows() -> Vec<Family> {
        let main = |extra: &'static str| -> &'static str {
            Box::leak(format!("{FAM}{extra}").into_boxed_str())
        };
        vec![
            Family {
                name: "method_invisible_trait",
                main: main(GET),
                rival: OTHER_GET_HIDDEN,
                question: "fn f(w: own S::Id<W<{D}>>) -> u8 { w.get() }",
                at_limit: AtLimit::Same,
            },
            Family {
                name: "method_visible_rival",
                main: main(GET),
                rival: OTHER_GET_VISIBLE,
                question: "fn f(w: own S::Id<W<{D}>>) -> u8 { w.get() }",
                at_limit: AtLimit::Limit,
            },
            Family {
                name: "type_other_item",
                main: main(HAS_ITEM),
                rival: "trait Other { type Thing }
impl<T: Nest> Other for W<T> where T::Out: Show { type Thing = u8 }",
                question: "fn f(x: <S::Id<W<{D}>> as HasItem>::Item) -> u8 { x }",
                at_limit: AtLimit::Same,
            },
            Family {
                name: "type_rival",
                main: main(HAS_ITEM),
                rival: "trait Other { type Item }
impl<T: Nest> Other for W<T> where T::Out: Show { type Item = bool }",
                question: "fn f(x: S::Id<W<{D}>>::Item) -> u8 { x }",
                at_limit: AtLimit::Limit,
            },
            Family {
                name: "const_rival",
                main: main(HAS_VALUE),
                rival: "trait Other { const VALUE: u8 }
impl<T: Nest> Other for W<T> where T::Out: Show { const VALUE: u8 = 2 }",
                question: "fn f() -> u8 { S::Id<W<{D}>>::VALUE }",
                at_limit: AtLimit::Limit,
            },
            Family {
                name: "solver_other_impl_holds",
                main: main(
                    "trait A {}
impl<T> A for N<T> {}
trait Tr {}
impl<T: A> Tr for W<T> {}",
                ),
                rival: "impl<T: Nest> Tr for W<T> where T::Out: Show {}",
                question: "fn need<X: Tr>() {}
fn h() { need<S::Id<W<{D}>>>() }",
                at_limit: AtLimit::Same,
            },
            Family {
                name: "solver_only_impl_over_limit",
                main: main("trait Tr {}"),
                rival: "impl<T: Nest> Tr for W<T> where T::Out: Show {}",
                question: "fn need<X: Tr>() {}
fn h() { need<S::Id<W<{D}>>>() }",
                at_limit: AtLimit::Limit,
            },
            // The family's definition or argument needs the deep projection.
            Family {
                name: "definition_projects_argument",
                main: main(""),
                rival: "",
                question: "fn f(x: S::Out<{D}>) -> u8 { x }",
                at_limit: AtLimit::Limit,
            },
            Family {
                name: "argument_is_deep_projection",
                main: main(""),
                rival: "",
                question: "fn f(x: S::Id<<{D} as Nest>::Out>) -> u8 { x }",
                at_limit: AtLimit::Limit,
            },
            Family {
                name: "ignored_argument_is_deep_projection",
                main: main(""),
                rival: "",
                question: "fn f(x: S::Keep<<{D} as Nest>::Out>) -> u8 { x }",
                at_limit: AtLimit::Limit,
            },
        ]
    }

    pub(super) fn check(codes: &(dyn Fn(&str) -> Vec<String> + Sync)) {
        let rows = rows();
        let failures: Vec<String> = std::thread::scope(|scope| {
            let handles: Vec<_> = rows
                .iter()
                .map(|row| scope.spawn(move || check_family(row, codes)))
                .collect();
            handles
                .into_iter()
                .filter_map(|h| h.join().unwrap())
                .collect()
        });
        assert!(failures.is_empty(), "{}", failures.join("\n\n"));
    }
}

/// `depth` nested applications of a family that returns its argument, and
/// the plain twin: as many nested projections of a blanket impl's type.
fn nested_applications(depth: usize, family: bool) -> String {
    if family {
        let ty = (0..depth).fold("u8".to_string(), |ty, _| format!("S::Id<{ty}>"));
        format!(
            "trait F {{ type Id<T> }}\nstruct S {{}}\nimpl F for S {{ type Id<T> = T }}\nfn f(_ x: {ty}) -> u8 {{ x }}\n"
        )
    } else {
        let ty = (0..depth).fold("u8".to_string(), |ty, _| format!("<{ty} as IdTr>::O"));
        format!(
            "trait IdTr {{ type O }}\nimpl<T> IdTr for T {{ type O = T }}\nfn f(_ x: {ty}) -> u8 {{ x }}\n"
        )
    }
}

const NESTED_KNOWN: &str = "a written family application is resolved when it is lowered, as a \
                            plain shorthand chain is, so no nesting limit applies to it; the twin \
                            nests qualified projections, which normalization resolves and counts";

/// Nested applications at depth 3 agree with the twin. At depth 65 they
/// are known to differ (`NESTED_KNOWN`: the plain twin meets the nesting
/// limit); both must still be checked without a crash.
fn check_nested_applications(codes: &dyn Fn(&str) -> Vec<String>) {
    assert_eq!(
        codes(&nested_applications(3, true)),
        codes(&nested_applications(3, false))
    );
    for family in [true, false] {
        let got = codes(&nested_applications(65, family));
        assert!(
            !got.contains(&"crash".to_string()),
            "depth 65, family {family}: {got:?} (known: {NESTED_KNOWN})"
        );
    }
}
