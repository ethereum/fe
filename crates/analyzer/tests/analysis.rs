use fe_analyzer::display::Displayable;
use fe_analyzer::namespace::items::{self, IngotId, IngotMode, Item, ModuleId, TypeDef};
use fe_analyzer::{AnalyzerDb, TestDb};
use fe_common::diagnostics::{diagnostics_string, print_diagnostics, Diagnostic, Label, Severity};
use fe_common::files::{FileKind, Utf8Path};
use fe_common::utils::files::BuildFiles;
use fe_parser::node::{NodeId, Span};
use indexmap::IndexMap;
use insta::assert_snapshot;
use smallvec::SmallVec;
use std::collections::{HashMap, VecDeque};
use std::fmt::Display;
use wasm_bindgen_test::wasm_bindgen_test;

#[test]
fn analyze_std_lib() {
    let mut db = TestDb::default();

    // Should return the same id
    let std_ingot = IngotId::std_lib(&mut db);
    let std_ingot_2 = IngotId::std_lib(&mut db);
    assert_eq!(std_ingot, std_ingot_2);

    db.set_root_ingot(std_ingot);

    let diags = std_ingot.diagnostics(&db);
    if !diags.is_empty() {
        print_diagnostics(&db, &diags);
        panic!("std lib analysis failed")
    }
}

#[test]
fn ingot_files_to_modules() {
    let mut db = TestDb::default();
    let ingot = IngotId::from_files(
        &mut db,
        "libcool",
        IngotMode::Lib,
        FileKind::Local,
        &[
            ("foo/fee.fe", ""),
            ("lib.fe", ""),
            ("bar/baz.fe", ""),
            ("bar/bum.fe", ""),
            ("x.fe", ""),
            ("a/b/c/d.fe", ""),
        ],
    );

    let submod_map = |module: ModuleId| {
        module
            .submodules(&db)
            .iter()
            .map(|m| (m.name(&db), *m))
            .collect::<HashMap<_, _>>()
    };

    let lib = ingot.root_module(&db).unwrap();
    let subs = submod_map(lib);
    assert_eq!(subs.len(), 4);

    let bar_subs = submod_map(subs["bar"]);
    assert_eq!(bar_subs.len(), 2);
    assert!(bar_subs.contains_key("bum"));
    assert!(bar_subs.contains_key("baz"));

    let d = subs["a"].submodules(&db)[0] // b
        .submodules(&db)[0] // c
        .submodules(&db)[0]; // d
    assert_eq!(d.name(&db), "d");
}

macro_rules! test_analysis {
    ($name:ident, $path:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let mut db = TestDb::default();

            let file_name = Utf8Path::new($path).file_name().unwrap();
            let module = ModuleId::new_standalone(&mut db, file_name, test_files::fixture($path));

            let diags = module.diagnostics(&db);
            if !diags.is_empty() {
                print_diagnostics(&db, &diags);
                panic!("analysis failed")
            }

            if cfg!(target_arch = "wasm32") {
                // NOTE: If this assertion fails, the generation of the output diff
                //  is very slow on wasm, and may result in an out-of-memory error
                //  for larger diffs. I recommend commenting out all tests but one.
                fe_common::assert_snapshot_wasm!(
                    concat!("snapshots/analysis__", stringify!($name), ".snap"),
                    build_snapshot(&db, module)
                );
            } else {
                assert_snapshot!(build_snapshot(&db, module));
            }
        }
    };
}

macro_rules! test_analysis_ingot {
    ($name:ident, $path:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let mut db = TestDb::default();

            let fixture_files = test_files::new_fixture_dir_files("ingots");
            let build_files = BuildFiles::load_static(fixture_files, $path)
                .expect("failed to statically load build files");
            let ingot = IngotId::from_build_files(&mut db, &build_files);

            let diags = ingot.diagnostics(&db);
            if !diags.is_empty() {
                print_diagnostics(&db, &diags);
                panic!("analysis failed")
            }
            diagnostics_string(&db, &diags);

            let snapshot =
                ModuleIter::new(&db, ingot.root_module(&db).expect("missing root module"))
                    .map(|module| build_snapshot(&db, module))
                    .collect::<Vec<_>>()
                    .join("\n");

            if cfg!(target_arch = "wasm32") {
                // NOTE: If this assertion fails, the generation of the output diff
                //  is very slow on wasm, and may result in an out-of-memory error
                //  for larger diffs. I recommend commenting out all tests but one.
                fe_common::assert_snapshot_wasm!(
                    concat!("snapshots/analysis__", stringify!($name), ".snap"),
                    snapshot
                );
            } else {
                assert_snapshot!(snapshot);
            }
        }
    };
}

/// Breadth-first walk of a module tree
struct ModuleIter<'a> {
    db: &'a dyn AnalyzerDb,
    emit: VecDeque<ModuleId>,
    walk: VecDeque<ModuleId>,
}

impl<'a> ModuleIter<'a> {
    pub fn new(db: &'a dyn AnalyzerDb, root: ModuleId) -> Self {
        let mut deq = VecDeque::new();
        deq.push_back(root);
        Self {
            db,
            emit: deq.clone(),
            walk: deq,
        }
    }
}

impl<'a> Iterator for ModuleIter<'a> {
    type Item = ModuleId;
    fn next(&mut self) -> Option<ModuleId> {
        if let Some(modid) = self.emit.pop_front() {
            Some(modid)
        } else if let Some(modid) = self.walk.pop_front() {
            let submods = modid.submodules(self.db);
            self.walk.extend(submods.iter());
            self.emit = VecDeque::from_iter(submods.iter().copied());
            self.next()
        } else {
            None
        }
    }
}

test_analysis! { erc20_token, "demos/erc20_token.fe"}
test_analysis! { guest_book, "demos/guest_book.fe"}
test_analysis! { simple_open_auction, "demos/simple_open_auction.fe"}
test_analysis! { uniswap, "demos/uniswap.fe"}
test_analysis! { abi_decode_complex, "features/abi_decode_complex.fe"}
test_analysis! { assert, "features/assert.fe"}
test_analysis! { aug_assign, "features/aug_assign.fe"}
test_analysis! { call_statement_with_args, "features/call_statement_with_args.fe"}
test_analysis! { call_statement_with_args_2, "features/call_statement_with_args_2.fe"}
test_analysis! { call_statement_without_args, "features/call_statement_without_args.fe"}
test_analysis! { checked_arithmetic, "features/checked_arithmetic.fe"}
test_analysis! { constructor, "features/constructor.fe"}
test_analysis! { create2_contract, "features/create2_contract.fe"}
test_analysis! { create_contract, "features/create_contract.fe"}
test_analysis! { create_contract_from_init, "features/create_contract_from_init.fe"}
test_analysis! { empty, "features/empty.fe"}
test_analysis! { enum_match, "features/enum_match.fe"}
test_analysis! { events, "features/events.fe"}
test_analysis! { module_level_events, "features/module_level_events.fe"}
test_analysis! { external_contract, "features/external_contract.fe"}
test_analysis! { for_loop_with_break, "features/for_loop_with_break.fe"}
test_analysis! { for_loop_with_continue, "features/for_loop_with_continue.fe"}
test_analysis! { for_loop_with_static_array, "features/for_loop_with_static_array.fe"}
test_analysis! { if_statement, "features/if_statement.fe"}
test_analysis! { if_statement_2, "features/if_statement_2.fe"}
test_analysis! { if_statement_with_block_declaration, "features/if_statement_with_block_declaration.fe"}
test_analysis! { keccak, "features/keccak.fe"}
test_analysis! { math, "features/math.fe"}
test_analysis! { module_const, "features/module_const.fe"}
test_analysis! { multi_param, "features/multi_param.fe"}
test_analysis! { nested_map, "features/nested_map.fe"}
test_analysis! { numeric_sizes, "features/numeric_sizes.fe"}
test_analysis! { ownable, "features/ownable.fe"}
test_analysis! { pure_fn_standalone, "features/pure_fn_standalone.fe"}
test_analysis! { revert, "features/revert.fe"}
test_analysis! { self_address, "features/self_address.fe"}
test_analysis! { send_value, "features/send_value.fe"}
test_analysis! { balances, "features/balances.fe"}
test_analysis! { sized_vals_in_sto, "features/sized_vals_in_sto.fe"}
test_analysis! { strings, "features/strings.fe"}
test_analysis! { structs, "features/structs.fe"}
test_analysis! { struct_fns, "features/struct_fns.fe"}
test_analysis! { ternary_expression, "features/ternary_expression.fe"}
test_analysis! { two_contracts, "features/two_contracts.fe"}
test_analysis! { type_coercion, "features/type_coercion.fe"}
test_analysis! { u8_u8_map, "features/u8_u8_map.fe"}
test_analysis! { u16_u16_map, "features/u16_u16_map.fe"}
test_analysis! { u32_u32_map, "features/u32_u32_map.fe"}
test_analysis! { u64_u64_map, "features/u64_u64_map.fe"}
test_analysis! { u128_u128_map, "features/u128_u128_map.fe"}
test_analysis! { u256_u256_map, "features/u256_u256_map.fe"}
test_analysis! { value_semantics, "features/value_semantics.fe"}
test_analysis! { while_loop, "features/while_loop.fe"}
test_analysis! { while_loop_with_break, "features/while_loop_with_break.fe"}
test_analysis! { while_loop_with_break_2, "features/while_loop_with_break_2.fe"}
test_analysis! { while_loop_with_continue, "features/while_loop_with_continue.fe"}
test_analysis! { abi_encoding_stress, "stress/abi_encoding_stress.fe"}
test_analysis! { data_copying_stress, "stress/data_copying_stress.fe"}
test_analysis! { tuple_stress, "stress/tuple_stress.fe"}
test_analysis! { tuple_destructuring, "features/tuple_destructuring.fe"}
test_analysis! { type_aliases, "features/type_aliases.fe"}
test_analysis! { const_generics, "features/const_generics.fe" }
test_analysis! { const_local, "features/const_local.fe" }

test_analysis_ingot! { basic_ingot, "ingots/basic_ingot"}

fn build_snapshot(db: &dyn AnalyzerDb, module: items::ModuleId) -> String {
    let diagnostics = module
        .all_items(db)
        .iter()
        .flat_map(|item| match item {
            Item::Type(TypeDef::Alias(alias)) => vec![build_display_diagnostic(
                alias.data(db).ast.span,
                &alias.type_id(db).unwrap().display(db),
            )],
            Item::Type(TypeDef::Struct(struct_)) => [
                label_in_non_overlapping_groups(
                    db,
                    &struct_
                        .fields(db)
                        .values()
                        .map(|field| (field.data(db).ast.span, field.typ(db).unwrap()))
                        .collect::<Vec<_>>(),
                ),
                struct_
                    .functions(db)
                    .values()
                    .flat_map(|id| function_diagnostics(*id, db))
                    .collect(),
            ]
            .concat(),
            Item::Type(TypeDef::Enum(enum_)) => [
                label_in_non_overlapping_groups(
                    db,
                    &enum_
                        .variants(db)
                        .values()
                        .map(|variant| (variant.data(db).ast.span, variant.kind(db).unwrap()))
                        .collect::<Vec<_>>(),
                ),
                enum_
                    .functions(db)
                    .values()
                    .flat_map(|id| function_diagnostics(*id, db))
                    .collect(),
            ]
            .concat(),
            Item::Type(TypeDef::Contract(contract)) => [
                label_in_non_overlapping_groups(
                    db,
                    &contract
                        .fields(db)
                        .values()
                        .map(|field| (field.data(db).ast.span, field.typ(db).unwrap()))
                        .collect::<Vec<_>>(),
                ),
                contract
                    .functions(db)
                    .values()
                    .flat_map(|id| function_diagnostics(*id, db))
                    .collect(),
            ]
            .concat(),
            Item::Trait(val) => val
                .all_functions(db)
                .iter()
                .map(|fun| {
                    build_display_diagnostic(fun.data(db).ast.span, &fun.signature(db).display(db))
                })
                .collect::<Vec<_>>(),
            Item::Impl(val) => val
                .all_functions(db)
                .iter()
                .map(|fun| {
                    build_display_diagnostic(fun.data(db).ast.span, &fun.signature(db).display(db))
                })
                .collect::<Vec<_>>(),
            Item::Function(id) => function_diagnostics(*id, db),
            Item::Constant(id) => vec![build_display_diagnostic(
                id.span(db),
                &id.typ(db).unwrap().display(db),
            )],
            // Built-in stuff
            Item::Type(TypeDef::Primitive(_))
            | Item::GenericType(_)
            | Item::BuiltinFunction(_)
            | Item::Intrinsic(_)
            | Item::Ingot(_)
            | Item::Attribute(_)
            | Item::Module(_) => vec![],
        })
        .collect::<Vec<_>>();

    diagnostics_string(db.upcast(), &diagnostics)
}

fn new_diagnostic(labels: Vec<Label>) -> Diagnostic {
    Diagnostic {
        severity: Severity::Note,
        message: String::new(),
        labels,
        notes: vec![],
    }
}

fn label_in_non_overlapping_groups(
    db: &dyn AnalyzerDb,
    spans: &[(Span, impl Displayable)],
) -> Vec<Diagnostic> {
    // Accumulate labels in a vec until we reach a span that overlaps
    // the labeled range, then emit a Diagnostic with the accumulated labels
    // and begin again. This assumes that all spans are within the same file.
    let file_id = if let Some((span, _)) = spans.first() {
        span.file_id
    } else {
        return vec![];
    };

    spans
        .iter()
        .enumerate()
        .scan(
            (Span::zero(file_id), vec![]),
            |(labeled, labels), (idx, (span, attr))| {
                let mut diags = SmallVec::<[Diagnostic; 2]>::new();

                let overlaps = span.start < labeled.end && span.end > labeled.start;

                // If the current span overlaps with the union of the current set of labels,
                // emit a diagnostic, and clear the set of labels.
                if overlaps {
                    diags.push(new_diagnostic(labels.clone()));
                    labels.clear();
                    *labeled = *span;
                }
                labels.push(Label::primary(*span, attr.display(db).to_string()));
                *labeled += *span;

                // If this is the last thing to label, emit a diagnostic.
                if idx == spans.len() - 1 {
                    diags.push(new_diagnostic(labels.clone()));
                }
                Some(diags)
            },
        )
        .flatten()
        .collect()
}

fn function_diagnostics(fun: items::FunctionId, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
    let body = fun.body(db);
    [
        // signature
        vec![build_display_diagnostic(
            fun.data(db).ast.span,
            &fun.signature(db).display(db),
        )],
        // declarations
        label_in_non_overlapping_groups(db, &lookup_spans(&body.var_types, &body.spans)),
        // expressions
        label_in_non_overlapping_groups(db, &lookup_spans(&body.expressions, &body.spans)),
        // CallType includes FunctionId,ContractId,StructId, so the debug output
        // may change when we add something to the std lib.
        // Disabling until we come up with a better label to use here.
        // label_in_non_overlapping_groups(db, &lookup_spans(&body.calls, &body.spans)),
    ]
    .concat()
}

fn lookup_spans<T: Clone>(
    node_attrs: &IndexMap<NodeId, T>,
    spans: &HashMap<NodeId, Span>,
) -> Vec<(Span, T)> {
    node_attrs
        .iter()
        .map(|(id, attr)| (*spans.get(id).unwrap(), attr.clone()))
        .collect()
}

fn build_display_diagnostic<T: Display>(span: Span, attributes: &T) -> Diagnostic {
    let label = Label::primary(span, format!("{attributes}"));
    Diagnostic {
        severity: Severity::Note,
        message: String::new(),
        labels: vec![label],
        notes: vec![],
    }
}
