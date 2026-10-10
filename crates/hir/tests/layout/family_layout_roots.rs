use crate::layout_test_support::parse_ok;
use fe_hir::{core::semantic::ContractLayoutValue, test_db::find_contract};

/// The layout of contract `C` in `src`: each entry of its report with its
/// address space and value, or `no report` when the contract has no layout.
fn layout(src: &str) -> String {
    parse_ok!(trusted db, top_mod, src);
    let contract = find_contract(&db, top_mod, "C");
    match contract.layout_report(&db) {
        Some(report) => report
            .entries
            .iter()
            .map(|entry| {
                let value = match &entry.value {
                    ContractLayoutValue::Scalar(value) => value.data(&db).to_string(),
                    ContractLayoutValue::Indexed { base, .. } => {
                        format!("{}[..]", base.data(&db))
                    }
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
    }
}

/// A contract whose provider's target is `target`, after `decls`.
fn program(decls: &str, target: &str) -> String {
    format!(
        r#"
use core::effect_ref::{{AddressSpace, EffectHandle, StaticSlot}}

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
}}
"#
    )
}

/// A family application exposes the layout roots of the type its definition
/// gives, whether they come from the definition, from the arguments, or are
/// written in the definition.
#[test]
fn a_family_application_exposes_the_roots_of_the_type_it_stands_for() {
    let cases = [
        // (plain definition, family definition, family use)
        ("(u8, Slot)", "(T, Slot)", "u8"),
        ("(Slot, Slot)", "(T, Slot)", "Slot"),
        ("(Slot<7>, (u8, Slot))", "(Slot<7>, (T, Slot))", "u8"),
    ];
    for (plain, family, arg) in cases {
        let expected = layout(&program(
            &format!(
                "trait Factory {{ type Wrap }}\nimpl Factory for Fac {{ type Wrap = {plain} }}"
            ),
            "<Fac as Factory>::Wrap",
        ));
        let given = layout(&program(
            &format!(
                "trait Factory {{ type Wrap<T> }}\nimpl Factory for Fac {{ type Wrap<T> = {family} }}"
            ),
            &format!("<Fac as Factory>::Wrap<{arg}>"),
        ));
        assert!(expected.contains("ROOT"), "{plain}: {expected}");
        assert_eq!(given, expected, "{plain}");
    }
}

/// A root given a concrete value reaches the layout through a family as it
/// does through a plain associated type: forwarded from the impl's
/// parameters, from an alias whose owner names the family's parameter, from
/// a projection in the definition, fixed or on the family's parameter, from
/// a family used in another family's definition, and into a static slot of
/// another address space. Each case is (name, plain declarations, plain
/// target, family declarations, family target).
#[test]
fn a_family_application_keeps_the_concrete_roots_of_its_definition() {
    let forwarded = "impl<const ROOT: u256> Factory for Holder<ROOT>";
    let other = "trait Other { type Out }
impl<const ROOT: u256> Other for Holder<ROOT> { type Out = Plain<ROOT> }";
    let pin = "struct Pair<X, const N: u256> { x: X }
type Pin<X, const R: u256 = _> = Pair<X, R>";
    let routed = "struct Routed<const ROOT: u256> {}
impl<const ROOT: u256> StaticSlot for Routed<ROOT> {
    const SPACE: AddressSpace = AddressSpace::TransientStorage
}";
    let cases = [
        (
            "impl parameter",
            format!(
                "trait Factory {{ type Wrap }}\n{forwarded} {{ type Wrap = (u8, Plain<ROOT>) }}"
            ),
            "<Holder<7> as Factory>::Wrap",
            format!(
                "trait Factory {{ type Wrap<T> }}\n{forwarded} {{ type Wrap<T> = (T, Plain<ROOT>) }}"
            ),
            "<Holder<7> as Factory>::Wrap<u8>",
        ),
        (
            "alias owner",
            format!(
                "{pin}\ntrait Factory {{ type Wrap }}\nimpl Factory for Fac {{ type Wrap = Pin<u8, 7> }}"
            ),
            "<Fac as Factory>::Wrap",
            format!(
                "{pin}\ntrait Factory {{ type Wrap<T> }}\nimpl Factory for Fac {{ type Wrap<T> = Pin<T, 7> }}"
            ),
            "<Fac as Factory>::Wrap<u8>",
        ),
        (
            "projection in the definition",
            format!(
                "{other}\ntrait Factory {{ type Wrap }}\nimpl Factory for Fac {{ type Wrap = (u8, <Holder<7> as Other>::Out) }}"
            ),
            "<Fac as Factory>::Wrap",
            format!(
                "{other}\ntrait Factory {{ type Wrap<T> }}\nimpl Factory for Fac {{ type Wrap<T> = (T, <Holder<7> as Other>::Out) }}"
            ),
            "<Fac as Factory>::Wrap<u8>",
        ),
        (
            "projection on the family's parameter",
            format!(
                "{other}\ntrait Factory {{ type Wrap }}\nimpl Factory for Fac {{ type Wrap = (u8, <Holder<7> as Other>::Out) }}"
            ),
            "<Fac as Factory>::Wrap",
            format!(
                "{other}\ntrait Factory {{ type Wrap<T: Other> }}\nimpl Factory for Fac {{ type Wrap<T> = (u8, <T as Other>::Out) }}"
            ),
            "<Fac as Factory>::Wrap<Holder<7>>",
        ),
        (
            "family in a family",
            format!(
                "trait Factory {{ type Wrap\n type Outer }}\n{forwarded} {{ type Wrap = (u8, Plain<ROOT>)\n type Outer = (bool, Self::Wrap) }}"
            ),
            "<Holder<7> as Factory>::Outer",
            format!(
                "trait Factory {{ type Wrap<T>\n type Outer<U> }}\n{forwarded} {{ type Wrap<T> = (T, Plain<ROOT>)\n type Outer<U> = (bool, Self::Wrap<U>) }}"
            ),
            "<Holder<7> as Factory>::Outer<u8>",
        ),
        (
            "static slot",
            format!(
                "{routed}\ntrait Factory {{ type Wrap }}\n{forwarded} {{ type Wrap = (u8, Routed<ROOT>) }}"
            ),
            "<Holder<7> as Factory>::Wrap",
            format!(
                "{routed}\ntrait Factory {{ type Wrap<T> }}\n{forwarded} {{ type Wrap<T> = (T, Routed<ROOT>) }}"
            ),
            "<Holder<7> as Factory>::Wrap<u8>",
        ),
    ];
    let mut differing = Vec::new();
    for (name, plain_decls, plain_target, family_decls, family_target) in cases {
        let expected = layout(&program(&plain_decls, plain_target));
        let given = layout(&program(&family_decls, family_target));
        assert!(
            expected.contains("Parameter(Explicit)"),
            "{name}: {expected}"
        );
        if given != expected {
            differing.push(format!("{name}:\n   {given}\nexpected\n   {expected}"));
        }
    }
    assert!(differing.is_empty(), "{}", differing.join("\n\n"));
}

/// A concrete root inside an array is rejected through a family as through a
/// plain associated type.
#[test]
fn a_family_application_keeps_a_concrete_root_inside_an_array() {
    let expected = layout(&program(
        "trait Factory { type Wrap }
impl<const ROOT: u256> Factory for Holder<ROOT> { type Wrap = (u8, [Plain<ROOT>; 2]) }",
        "<Holder<7> as Factory>::Wrap",
    ));
    let given = layout(&program(
        "trait Factory { type Wrap<T> }
impl<const ROOT: u256> Factory for Holder<ROOT> { type Wrap<T> = (T, [Plain<ROOT>; 2]) }",
        "<Holder<7> as Factory>::Wrap<u8>",
    ));
    assert_eq!(expected, "no report");
    assert_eq!(given, expected);
}
