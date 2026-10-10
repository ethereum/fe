//! Contract layouts with associated types that take parameters agree with
//! their plain twins: the same report entries (path, kind, address space,
//! slot) and the same error codes. Each entry is one source with
//! `⟨family|plain⟩` alternatives in its declarations, its handle target and
//! its extra contract fields.

use fe_hir::{
    core::semantic::ContractLayoutValue,
    test_db::{HirAnalysisTestDb, find_contract},
};

/// The family (`true`) or plain text of `src`.
fn alt(src: &str, family: bool) -> String {
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
    out + rest
}

/// The program: the declarations, a storage handle `H` whose target is
/// `target`, and contract `C` with `h`, an array, an inferred root and
/// `fields`. As in `family_layout_roots.rs`.
fn program(decls: &str, target: &str, fields: &str) -> String {
    format!(
        "use core::effect_ref::{{AddressSpace, EffectHandle, StaticSlot}}
struct Slot<const ROOT: u256 = _> {{}}
struct Plain<const ROOT: u256> {{}}
struct Holder<const ROOT: u256 = _> {{}}
struct Fac {{}}
{decls}
struct H {{ raw: u256 }}
impl EffectHandle for H {{
    type Target = {target}
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage
    fn raw(self) -> u256 {{ self.raw }}
}}
contract C {{
    mut h: H,
    mut arr: [u256; 10],
    mut a: Slot,
{fields}
}}
"
    )
}

/// The error codes and layout report entries of `src`.
fn report(src: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_trusted_effect_handle_module("family_layout_twins.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    let mut codes: Vec<String> = db
        .run_on_top_mod(top_mod)
        .iter()
        .map(|diag| diag.to_complete(&db).error_code.to_string())
        .collect();
    codes.sort();
    let entries = match find_contract(&db, top_mod, "C").layout_report(&db) {
        Some(report) => report
            .entries
            .iter()
            .map(|entry| {
                let value = match &entry.value {
                    ContractLayoutValue::Scalar(value) => value.data(&db).to_string(),
                    ContractLayoutValue::Indexed { base, .. } => format!("{}[..]", base.data(&db)),
                };
                format!(
                    "{} = {:?} {:?} {value}",
                    entry.path.display(&db),
                    entry.kind,
                    entry.address_space
                )
            })
            .collect::<Vec<_>>()
            .join("\n   "),
        None => "no report".to_string(),
    };
    format!("codes {codes:?}\n   {entries}")
}

const ONE: &str = "trait Factory { type Wrap⟨<T>|⟩ }";
const FORWARDED: &str = "impl<const ROOT: u256> Factory for Holder<ROOT>";
const ROUTED: &str = "struct Routed<const ROOT: u256> {}
impl<const ROOT: u256> StaticSlot for Routed<ROOT> {
    const SPACE: AddressSpace = AddressSpace::TransientStorage
}";
const HG: &str = "struct HG<const R: u256> { raw: u256 }
impl<const R: u256> EffectHandle for HG<R> {
    type Target = TARGET
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage
    fn raw(self) -> u256 { self.raw }
}";

/// (name, declarations, target, fields)
fn cases() -> Vec<(&'static str, String, &'static str, &'static str)> {
    let w = |definition: &str| format!("{ONE}\nimpl Factory for Fac {{ type Wrap{definition} }}");
    let hg = |target: &str| HG.replace("TARGET", target);
    vec![
        ("default_explicit_root",
            "trait Factory { type Wrap⟨<T> = (T, Plain<7>)| = (u8, Plain<7>)⟩ }\nimpl Factory for Fac {}".into(),
            "<Fac as Factory>::Wrap⟨<u8>|⟩", ""),
        ("default_names_self",
            format!("trait Factory {{ type Wrap⟨<T> = (T, Self)| = (u8, Self)⟩ }}\n{FORWARDED} {{}}"),
            "<Holder<7> as Factory>::Wrap⟨<u8>|⟩", ""),
        ("root_in_argument", w("⟨<T> = (T, Slot)| = (Plain<7>, Slot)⟩"),
            "<Fac as Factory>::Wrap⟨<Plain<7>>|⟩", ""),
        ("impl_type_parameter",
            "struct Gen<X> {}\ntrait Factory { type Wrap⟨<T>|⟩ }\nimpl<X> Factory for Gen<X> { type Wrap⟨<T> = (T, X)| = (u8, X)⟩ }".into(),
            "<Gen<Plain<7>> as Factory>::Wrap⟨<u8>|⟩", ""),
        ("impl_const_parameter",
            format!("{ONE}\n{FORWARDED} {{ type Wrap⟨<T> = (T, Plain<ROOT>)| = (u8, Plain<ROOT>)⟩ }}"),
            "<Holder<7> as Factory>::Wrap⟨<u8>|⟩", ""),
        ("constructor_extra_argument",
            "struct W2<A, B> { a: A, b: B }\ntrait Factory { type Wrap⟨<T>: * -> *|⟩ }\nimpl Factory for Fac { type Wrap⟨<T> = W2<T>| = W2<Plain<7>, Slot>⟩ }".into(),
            "<Fac as Factory>::Wrap⟨<Plain<7>, Slot>|⟩", ""),
        ("unapplied_into_constructor_parameter",
            "struct HK<G: * -> *> { g: G<Slot> }\ntrait Factory { type Wrap⟨<T> = (T, Plain<7>)| = (Slot, Plain<7>)⟩ }\nimpl Factory for Fac {}⟨|\nstruct HKP { g: <Fac as Factory>::Wrap }⟩".into(),
            "⟨HK<<Fac as Factory>::Wrap>|HKP⟩", ""),
        ("next_to_explicit_field", w("⟨<T> = (T, Plain<7>)| = (u8, Plain<7>)⟩"),
            "<Fac as Factory>::Wrap⟨<u8>|⟩", "    mut z: Slot<7>,"),
        ("projection_argument",
            format!("trait Other {{ type Out }}\nimpl Other for Fac {{ type Out = Plain<7> }}\n{}", w("⟨<T> = (T, Slot)| = (<Fac as Other>::Out, Slot)⟩")),
            "<Fac as Factory>::Wrap⟨<<Fac as Other>::Out>|⟩", ""),
        ("projection_on_parameter",
            "trait Other { type Out }\nimpl<const ROOT: u256> Other for Holder<ROOT> { type Out = Plain<ROOT> }\ntrait Factory { type Wrap⟨<T: Other>|⟩ }\nimpl Factory for Fac { type Wrap⟨<T> = (u8, <T as Other>::Out)| = (u8, <Holder<7> as Other>::Out)⟩ }".into(),
            "<Fac as Factory>::Wrap⟨<Holder<7>>|⟩", ""),
        ("array_of_family", w("⟨<T> = (T, Plain<7>)| = (u8, Plain<7>)⟩"),
            "[<Fac as Factory>::Wrap⟨<u8>|⟩; 2]", ""),
        ("holder_argument", w("⟨<T> = (T, Slot)| = (Holder<9>, Slot)⟩"),
            "<Fac as Factory>::Wrap⟨<Holder<9>>|⟩", ""),
        ("ignored_argument", w("⟨<T>|⟩ = Slot"),
            "<Fac as Factory>::Wrap⟨<Plain<7>>|⟩", ""),
        ("two_parameters_swapped",
            "trait Factory { type Wrap⟨<A, B>|⟩ }\nimpl Factory for Fac { type Wrap⟨<A, B> = (B, A)| = (Plain<5>, Plain<6>)⟩ }".into(),
            "<Fac as Factory>::Wrap⟨<Plain<6>, Plain<5>>|⟩", ""),
        ("shorthand_target", w("⟨<T> = (T, Slot)| = (Slot<5>, Slot)⟩"),
            "Fac::Wrap⟨<Slot<5>>|⟩", ""),
        ("static_slot_argument",
            format!("{ROUTED}\n{}", w("⟨<T> = (u8, T)| = (u8, Routed<4>)⟩")),
            "<Fac as Factory>::Wrap⟨<Routed<4>>|⟩", ""),
        ("default_overridden",
            "trait Factory { type Wrap⟨<T> = (T, Plain<1>)| = (u8, Plain<1>)⟩ }\nimpl Factory for Fac { type Wrap⟨<T> = (T, Plain<2>)| = (u8, Plain<2>)⟩ }".into(),
            "<Fac as Factory>::Wrap⟨<u8>|⟩", ""),
        ("blanket_impl",
            "trait Mk {}\nimpl Mk for Fac {}\ntrait Factory { type Wrap⟨<T>|⟩ }\nimpl<X: Mk> Factory for X { type Wrap⟨<T> = (T, X)| = (Plain<8>, X)⟩ }".into(),
            "<Fac as Factory>::Wrap⟨<Plain<8>>|⟩", ""),
        ("family_in_family",
            format!("trait Factory {{ type Wrap⟨<T>|⟩\n type Outer⟨<U>|⟩ }}\n{FORWARDED} {{ type Wrap⟨<T> = (T, Plain<ROOT>)| = (u8, Plain<ROOT>)⟩\n type Outer⟨<U> = (bool, Self::Wrap<U>)| = (bool, Self::Wrap)⟩ }}"),
            "<Holder<7> as Factory>::Outer⟨<u8>|⟩", ""),
        ("same_definition_twice", w("⟨<T> = (T, Slot)| = (u8, Slot)⟩"),
            "(<Fac as Factory>::Wrap⟨<u8>|⟩, <Fac as Factory>::Wrap⟨<u16>|⟩)", ""),
        ("generic_handle_forwards_root",
            format!("{ONE}\n{FORWARDED} {{ type Wrap⟨<T> = (T, Plain<ROOT>)| = (u8, Plain<ROOT>)⟩ }}\n{}", hg("<Holder<R> as Factory>::Wrap⟨<u8>|⟩")),
            "u256", "    mut g: HG<7>,"),
        ("generic_handle_inferred_roots",
            format!("{}\n{}", w("⟨<T> = (T, Slot)| = Slot⟩"), hg("⟨<Fac as Factory>::Wrap<Plain<R>>|(Plain<R>, <Fac as Factory>::Wrap)⟩")),
            "u256", "    mut g: HG<7>,\n    mut k: HG<8>,"),
        // Contract fields typed by a family application.
        ("field_fixed_root", w("⟨<T>|⟩ = Slot<7>"),
            "u256", "    mut f: <Fac as Factory>::Wrap⟨<u8>|⟩,\n    mut g: Slot<7>,"),
        ("field_inferred_roots", w("⟨<T>|⟩ = Slot"),
            "u256", "    mut f: <Fac as Factory>::Wrap⟨<u8>|⟩,\n    mut g: <Fac as Factory>::Wrap⟨<u16>|⟩,"),
        ("field_root_in_argument", w("⟨<T> = T| = Slot<3>⟩"),
            "u256", "    mut f: <Fac as Factory>::Wrap⟨<Slot<3>>|⟩,\n    mut g: Slot<3>,"),
        ("field_struct_result",
            format!("struct Two<A, B> {{ a: A, b: B }}\n{}", w("⟨<T> = Two<T, Slot>| = Two<Slot, Slot>⟩")),
            "u256", "    mut f: <Fac as Factory>::Wrap⟨<Slot>|⟩,"),
        ("field_tuple_inferred_root", w("⟨<T> = (T, Slot)| = (u8, Slot)⟩"),
            "u256", "    mut f: Fac::Wrap⟨<u8>|⟩,"),
        ("field_tuple_argument_root", w("⟨<T> = (T, Slot)| = (Slot<5>, Slot)⟩"),
            "u256", "    mut f: Fac::Wrap⟨<Slot<5>>|⟩,\n    mut g: Slot<5>,"),
        ("field_tuple_qualified", w("⟨<T> = (T, Slot)| = (Slot<5>, Slot)⟩"),
            "u256", "    mut f: <Fac as Factory>::Wrap⟨<Slot<5>>|⟩,"),
    ]
}

#[test]
fn family_layouts_agree_with_their_plain_twins() {
    let cases = cases();
    let failures: Vec<String> = std::thread::scope(|scope| {
        let handles: Vec<_> = cases
            .iter()
            .map(|(name, decls, target, fields)| {
                scope.spawn(move || {
                    let side = |family: bool| {
                        report(&program(
                            &alt(decls, family),
                            &alt(target, family),
                            &alt(fields, family),
                        ))
                    };
                    let (family, plain) = (side(true), side(false));
                    (family != plain)
                        .then(|| format!("{name}:\n   family {family}\n   plain  {plain}"))
                })
            })
            .collect();
        handles
            .into_iter()
            .filter_map(|h| h.join().unwrap())
            .collect()
    });
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}
