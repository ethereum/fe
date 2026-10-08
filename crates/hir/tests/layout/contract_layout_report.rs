use crate::layout_test_support::{parse_module, parse_ok};
use fe_hir::{
    analysis::{initialize_analysis_pass, ty::ProviderAddressSpace},
    core::semantic::{
        ContractFieldId, ContractLayoutEntry, ContractLayoutEntryKind, ContractLayoutError,
        ContractLayoutValue,
    },
    hir_def::IdentId,
    test_db::{HirAnalysisTestDb, find_contract, format_diagnostics},
};

fn entry<'a, 'db>(
    db: &'db HirAnalysisTestDb,
    entries: &'a [ContractLayoutEntry<'db>],
    path: &str,
) -> &'a ContractLayoutEntry<'db> {
    entries
        .iter()
        .find(|entry| entry.path.display(db) == path)
        .unwrap_or_else(|| panic!("missing layout entry `{path}`: {entries:#?}"))
}

fn scalar_value(db: &HirAnalysisTestDb, entry: &ContractLayoutEntry<'_>) -> String {
    let ContractLayoutValue::Scalar(value) = entry.value else {
        panic!("expected scalar layout value: {entry:#?}");
    };
    value.data(db).to_string()
}

#[test]
fn report_preserves_inline_array_geometry_and_enum_overlays() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Leaf { left: u256, right: u8 }
enum Choice { Pair(Leaf), Unit }

contract C {
    mut values: [Leaf; 2],
    mut choice: Choice,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let report = contract.layout_report(&db).unwrap();

    for (path, base) in [("values[i0].left", "0"), ("values[i0].right", "1")] {
        let entry = entry(&db, &report.entries, path);
        let ContractLayoutValue::Indexed {
            base: actual,
            dimensions,
            strides,
            extent,
        } = &entry.value
        else {
            panic!("expected indexed inline entry: {entry:#?}");
        };
        assert_eq!(actual.data(&db).to_string(), base);
        assert_eq!(dimensions, &[2]);
        assert_eq!(strides, &[2]);
        assert_eq!(*extent, 3);
    }

    let tag = entry(&db, &report.entries, "choice.<tag>");
    assert_eq!(scalar_value(&db, tag), "4");
    assert_eq!(tag.kind, ContractLayoutEntryKind::EnumTag);
    assert_eq!(
        scalar_value(&db, entry(&db, &report.entries, "choice::Pair.0.left")),
        "5"
    );
    assert_eq!(
        scalar_value(&db, entry(&db, &report.entries, "choice::Pair.0.right")),
        "6"
    );
}

#[test]
fn report_numbers_storage_and_transient_fields_together() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::TStorPtr

contract C {
    mut stored: u256,
    mut temporary: TStorPtr<u256>,
    mut later: u256,
    immutable: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let report = contract.layout_report(&db).unwrap();
    for (path, space, value) in [
        ("stored", ProviderAddressSpace::Storage, "0"),
        ("temporary", ProviderAddressSpace::Transient, "1"),
        ("later", ProviderAddressSpace::Storage, "2"),
        ("immutable", ProviderAddressSpace::Code, "0"),
    ] {
        let entry = entry(&db, &report.entries, path);
        assert_eq!(entry.address_space, space, "{path}");
        assert_eq!(scalar_value(&db, entry), value, "{path}");
    }
}

#[test]
fn report_places_explicit_slot_fields() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::keccak
use std::evm::TStorPtr

struct Pair { left: u256, right: u256 }

contract C {
    mut first: u256,
    #[slot(1)]
    mut placed: u256,
    mut pair: Pair,
    #[slot(keccak(keccak("example.main") - 1) & !(0xff as u256))]
    mut main: Pair,
    #[slot(1)]
    mut flag: TStorPtr<bool>,
    mut after: TStorPtr<bool>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let report = contract.layout_report(&db).unwrap();
    // `pair` skips slot 1, which `placed` takes. `main` is at ERC-7201's
    // slot for the namespace `example.main`. Transient slot 1 is free for
    // `flag`, and `after` continues the shared counter.
    for (path, space, value) in [
        ("first", ProviderAddressSpace::Storage, "0"),
        ("placed", ProviderAddressSpace::Storage, "1"),
        ("pair.left", ProviderAddressSpace::Storage, "2"),
        ("pair.right", ProviderAddressSpace::Storage, "3"),
        (
            "main.left",
            ProviderAddressSpace::Storage,
            "10958655983261152271848436692291137275443024275653522991983264966744321209600",
        ),
        (
            "main.right",
            ProviderAddressSpace::Storage,
            "10958655983261152271848436692291137275443024275653522991983264966744321209601",
        ),
        ("flag", ProviderAddressSpace::Transient, "1"),
        ("after", ProviderAddressSpace::Transient, "4"),
    ] {
        let entry = entry(&db, &report.entries, path);
        assert_eq!(entry.address_space, space, "{path}");
        assert_eq!(scalar_value(&db, entry), value, "{path}");
    }
}

#[test]
fn overlapping_explicit_slots_reject_the_later_field() {
    parse_module!(
        db,
        top_mod,
        r#"
struct Pair { left: u256, right: u256 }

contract C {
    #[slot(4)]
    mut a: Pair,
    #[slot(5)]
    mut b: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let result = contract.storage_layout(&db);
    assert!(result.allocated.is_none());
    assert_eq!(
        result.field_errors(&IdentId::new(&db, "b".to_string())),
        Some(
            [ContractLayoutError::ExplicitSlotOverlap {
                other: IdentId::new(&db, "a".to_string())
            }]
            .as_slice()
        )
    );
}

#[test]
fn a_transient_cell_takes_its_slot_in_transient_storage_too() {
    parse_module!(
        db,
        top_mod,
        r#"
use std::evm::{TSlot, TStorPtr}

contract Overlapping {
    #[slot(5)]
    mut cell: TSlot<u256>,
    #[slot(5)]
    mut flag: TStorPtr<bool>,
}

contract Skipping {
    #[slot(0)]
    mut cell: TSlot<u256>,
    mut flag: TStorPtr<bool>,
}
"#,
    );
    // The cell's value lives in transient slot 5, which `flag` also names.
    let overlapping = find_contract(&db, top_mod, "Overlapping").storage_layout(&db);
    assert_eq!(
        overlapping.field_errors(&IdentId::new(&db, "flag".to_string())),
        Some(
            [ContractLayoutError::ExplicitSlotOverlap {
                other: IdentId::new(&db, "cell".to_string())
            }]
            .as_slice()
        )
    );
    // `flag` skips transient slot 0, which the cell's value takes.
    let skipping = find_contract(&db, top_mod, "Skipping").storage_layout(&db);
    let fields = &skipping
        .allocated
        .as_ref()
        .expect("contract layout should be allocated")
        .fields;
    assert_eq!(
        fields[&IdentId::new(&db, "flag".to_string())].slot_offset,
        1
    );
}

#[test]
fn mutex_lock_cells_share_the_field_counter() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::Mutex
use std::evm::effects::TStorPtr

contract C {
    t0: TStorPtr<bool>,
    t1: TStorPtr<bool>,
    mut m: Mutex<u256>,
    mut m2: Mutex<u256>,
    after: TStorPtr<bool>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db);
    let fields = &layout
        .allocated
        .as_ref()
        .expect("contract layout should be allocated")
        .fields;
    // Storage and transient fields share one counter. A mutex's value takes
    // a storage slot and its lock, a transient cell, the next number.
    for (name, offset, count) in [
        ("t0", 0, 1),
        ("t1", 1, 1),
        ("m", 2, 2),
        ("m2", 4, 2),
        ("after", 6, 1),
    ] {
        let field = &fields[&IdentId::new(&db, name.to_string())];
        assert_eq!(field.slot_offset, offset, "{name}");
        assert_eq!(field.slot_count, count, "{name}");
    }
}

#[test]
fn layout_extent_overflow_rejects_without_panicking() {
    parse_module!(
        db,
        top_mod,
        r#"
contract C {
    huge: [u256; 576460752303423488],
}
"#,
    );
    let rendered = format_diagnostics(&db, &initialize_analysis_pass().run_on_module(&db, top_mod));
    assert!(
        rendered.contains("contract-field layout extent overflowed"),
        "{rendered}"
    );
    let contract = find_contract(&db, top_mod, "C");
    let result = contract.storage_layout(&db);
    assert!(result.allocated.is_none());
    assert_eq!(
        result.field_errors(&IdentId::new(&db, "huge".to_string())),
        Some([ContractLayoutError::LayoutExtentOverflow].as_slice())
    );
}

#[test]
fn duplicate_contract_field_names_block_layout_without_panicking() {
    parse_module!(
        db,
        top_mod,
        r#"
contract DuplicateFields {
    mut value: u256,
    mut value: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "DuplicateFields");
    let result = contract.storage_layout(&db);
    assert!(result.allocated.is_none());
    assert_eq!(result.field_results.len(), 2);
    for index in 0..2 {
        assert!(matches!(
            result.field_errors_for_id(ContractFieldId { contract, index }),
            Some([ContractLayoutError::InvalidFieldType])
        ));
    }
    let rendered = format_diagnostics(&db, &initialize_analysis_pass().run_on_module(&db, top_mod));
    assert!(
        rendered.contains("duplicate field name in contract `DuplicateFields`"),
        "unexpected diagnostics:\n{rendered}"
    );
}

#[test]
fn ambiguous_handle_impl_rejects_the_field() {
    parse_module!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

trait MarkerA {}
trait MarkerB {}

struct Value {}

impl MarkerA for Value {}
impl MarkerB for Value {}

struct Ptr<T> { raw: u256 }

impl<T: MarkerA> EffectHandle for Ptr<T> {
    type Target = T

    const SPACE: AddressSpace = AddressSpace::Storage
    type Raw = u256

    fn raw(self) -> u256 {
        self.raw
    }
}

impl<T: MarkerB> EffectHandle for Ptr<T> {
    type Target = T

    const SPACE: AddressSpace = AddressSpace::Storage
    type Raw = u256

    fn raw(self) -> u256 {
        self.raw
    }
}

contract C {
    mut value: Ptr<Value>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let errors = contract
        .storage_layout(&db)
        .field_errors(&IdentId::new(&db, "value".to_string()))
        .expect("field should be rejected");
    assert!(matches!(
        errors.first(),
        Some(ContractLayoutError::AmbiguousProviderLayout)
    ));
    assert!(contract.storage_layout(&db).allocated.is_none());
}
