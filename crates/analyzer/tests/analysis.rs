use fe_analyzer::context;
use fe_analyzer::namespace::items::{self, Item, TypeDef};
use fe_analyzer::namespace::types::{Event, FixedSize, FunctionSignature, Type};
use fe_analyzer::{AnalyzerDb, Db};
use fe_common::diagnostics::{diagnostics_string, print_diagnostics, Diagnostic, Label, Severity};
use fe_common::files::FileStore;
use fe_parser::node::NodeId;
use fe_parser::node::Span;
use indexmap::IndexMap;
use insta::assert_snapshot;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use wasm_bindgen_test::wasm_bindgen_test;

macro_rules! test_analysis {
    ($name:ident, $path:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            let mut files = FileStore::new();
            let src = test_files::fixture($path);
            let id = files.add_file($path, src);
            let fe_module = match fe_parser::parse_file(&src) {
                Ok((module, _)) => module,
                Err(diags) => {
                    print_diagnostics(&diags, id, &files);
                    panic!("parsing failed");
                }
            };

            let db = Db::default();
            let module = db.intern_module(Rc::new(items::Module { ast: fe_module }));
            let diagnostics = module.diagnostics(&db);
            if !diagnostics.is_empty() {
                print_diagnostics(&diagnostics, id, &files);
                panic!("analysis failed")
            }

            if cfg!(target_arch = "wasm32") {
                // NOTE: If this assertion fails, the generation of the output diff
                //  is very slow on wasm, and may result in an out-of-memory error
                //  for larger diffs. I recommend commenting out all tests but one.
                fe_common::assert_snapshot_wasm!(
                    concat!("snapshots/analysis__", stringify!($name), ".snap"),
                    build_snapshot($path, &src, module, &db)
                );
            } else {
                assert_snapshot!(build_snapshot($path, &src, module, &db));
            }
        }
    };
}

test_analysis! { erc20_token, "demos/erc20_token.fe"}
test_analysis! { guest_book, "demos/guest_book.fe"}
test_analysis! { uniswap, "demos/uniswap.fe"}
test_analysis! { address_bytes10_map, "features/address_bytes10_map.fe"}
test_analysis! { assert, "features/assert.fe"}
test_analysis! { aug_assign, "features/aug_assign.fe"}
test_analysis! { base_tuple, "features/base_tuple.fe"}
test_analysis! { call_statement_with_args, "features/call_statement_with_args.fe"}
test_analysis! { call_statement_with_args_2, "features/call_statement_with_args_2.fe"}
test_analysis! { call_statement_without_args, "features/call_statement_without_args.fe"}
test_analysis! { checked_arithmetic, "features/checked_arithmetic.fe"}
test_analysis! { constructor, "features/constructor.fe"}
test_analysis! { create2_contract, "features/create2_contract.fe"}
test_analysis! { create_contract, "features/create_contract.fe"}
test_analysis! { create_contract_from_init, "features/create_contract_from_init.fe"}
test_analysis! { empty, "features/empty.fe"}
test_analysis! { events, "features/events.fe"}
test_analysis! { external_contract, "features/external_contract.fe"}
test_analysis! { for_loop_with_break, "features/for_loop_with_break.fe"}
test_analysis! { for_loop_with_continue, "features/for_loop_with_continue.fe"}
test_analysis! { for_loop_with_static_array, "features/for_loop_with_static_array.fe"}
test_analysis! { if_statement, "features/if_statement.fe"}
test_analysis! { if_statement_2, "features/if_statement_2.fe"}
test_analysis! { if_statement_with_block_declaration, "features/if_statement_with_block_declaration.fe"}
test_analysis! { keccak, "features/keccak.fe"}
test_analysis! { math, "features/math.fe"}
test_analysis! { multi_param, "features/multi_param.fe"}
test_analysis! { nested_map, "features/nested_map.fe"}
test_analysis! { numeric_sizes, "features/numeric_sizes.fe"}
test_analysis! { ownable, "features/ownable.fe"}
test_analysis! { return_addition_i256, "features/return_addition_i256.fe"}
test_analysis! { return_addition_u128, "features/return_addition_u128.fe"}
test_analysis! { return_addition_u256, "features/return_addition_u256.fe"}
test_analysis! { return_array, "features/return_array.fe"}
test_analysis! { return_bitwiseand_u128, "features/return_bitwiseand_u128.fe"}
test_analysis! { return_bitwiseand_u256, "features/return_bitwiseand_u256.fe"}
test_analysis! { return_bitwiseor_u256, "features/return_bitwiseor_u256.fe"}
test_analysis! { return_bitwiseshl_u256, "features/return_bitwiseshl_u256.fe"}
test_analysis! { return_bitwiseshr_i256, "features/return_bitwiseshr_i256.fe"}
test_analysis! { return_bitwiseshr_u256, "features/return_bitwiseshr_u256.fe"}
test_analysis! { return_bitwisexor_u256, "features/return_bitwisexor_u256.fe"}
test_analysis! { return_bool_false, "features/return_bool_false.fe"}
test_analysis! { return_bool_inverted, "features/return_bool_inverted.fe"}
test_analysis! { return_bool_op_and, "features/return_bool_op_and.fe"}
test_analysis! { return_bool_op_or, "features/return_bool_op_or.fe"}
test_analysis! { return_bool_true, "features/return_bool_true.fe"}
test_analysis! { return_builtin_attributes, "features/return_builtin_attributes.fe"}
test_analysis! { return_division_i256, "features/return_division_i256.fe"}
test_analysis! { return_division_u256, "features/return_division_u256.fe"}
test_analysis! { return_empty_tuple, "features/return_unit.fe"}
test_analysis! { return_eq_u256, "features/return_eq_u256.fe"}
test_analysis! { return_gt_i256, "features/return_gt_i256.fe"}
test_analysis! { return_gt_u256, "features/return_gt_u256.fe"}
test_analysis! { return_gte_i256, "features/return_gte_i256.fe"}
test_analysis! { return_gte_u256, "features/return_gte_u256.fe"}
test_analysis! { return_i128_cast, "features/return_i128_cast.fe"}
test_analysis! { return_i256, "features/return_i256.fe"}
test_analysis! { return_identity_u8, "features/return_identity_u8.fe"}
test_analysis! { return_identity_u16, "features/return_identity_u16.fe"}
test_analysis! { return_identity_u32, "features/return_identity_u32.fe"}
test_analysis! { return_identity_u64, "features/return_identity_u64.fe"}
test_analysis! { return_identity_u128, "features/return_identity_u128.fe"}
test_analysis! { return_identity_u256, "features/return_identity_u256.fe"}
test_analysis! { return_lt_i256, "features/return_lt_i256.fe"}
test_analysis! { return_lt_u128, "features/return_lt_u128.fe"}
test_analysis! { return_lt_u256, "features/return_lt_u256.fe"}
test_analysis! { return_lte_i256, "features/return_lte_i256.fe"}
test_analysis! { return_lte_u256, "features/return_lte_u256.fe"}
test_analysis! { return_mod_i256, "features/return_mod_i256.fe"}
test_analysis! { return_mod_u256, "features/return_mod_u256.fe"}
test_analysis! { return_msg_sig, "features/return_msg_sig.fe"}
test_analysis! { return_multiplication_i256, "features/return_multiplication_i256.fe"}
test_analysis! { return_multiplication_u256, "features/return_multiplication_u256.fe"}
test_analysis! { return_noteq_u256, "features/return_noteq_u256.fe"}
test_analysis! { return_pow_i256, "features/return_pow_i256.fe"}
test_analysis! { return_pow_u256, "features/return_pow_u256.fe"}
test_analysis! { return_subtraction_i256, "features/return_subtraction_i256.fe"}
test_analysis! { return_subtraction_u256, "features/return_subtraction_u256.fe"}
test_analysis! { return_u128_cast, "features/return_u128_cast.fe"}
test_analysis! { return_u256, "features/return_u256.fe"}
test_analysis! { return_u256_from_called_fn, "features/return_u256_from_called_fn.fe"}
test_analysis! { return_u256_from_called_fn_with_args, "features/return_u256_from_called_fn_with_args.fe"}
test_analysis! { revert, "features/revert.fe"}
test_analysis! { self_address, "features/self_address.fe"}
test_analysis! { sized_vals_in_sto, "features/sized_vals_in_sto.fe"}
test_analysis! { strings, "features/strings.fe"}
test_analysis! { structs, "features/structs.fe"}
test_analysis! { ternary_expression, "features/ternary_expression.fe"}
test_analysis! { two_contracts, "features/two_contracts.fe"}
test_analysis! { u8_u8_map, "features/u8_u8_map.fe"}
test_analysis! { u16_u16_map, "features/u16_u16_map.fe"}
test_analysis! { u32_u32_map, "features/u32_u32_map.fe"}
test_analysis! { u64_u64_map, "features/u64_u64_map.fe"}
test_analysis! { u128_u128_map, "features/u128_u128_map.fe"}
test_analysis! { u256_u256_map, "features/u256_u256_map.fe"}
test_analysis! { while_loop, "features/while_loop.fe"}
test_analysis! { while_loop_with_break, "features/while_loop_with_break.fe"}
test_analysis! { while_loop_with_break_2, "features/while_loop_with_break_2.fe"}
test_analysis! { while_loop_with_continue, "features/while_loop_with_continue.fe"}
test_analysis! { abi_encoding_stress, "stress/abi_encoding_stress.fe"}
test_analysis! { data_copying_stress, "stress/data_copying_stress.fe"}
test_analysis! { tuple_stress, "stress/tuple_stress.fe"}
test_analysis! { type_aliases, "features/type_aliases.fe"}

fn build_snapshot(path: &str, src: &str, module: items::ModuleId, db: &dyn AnalyzerDb) -> String {
    let mut file_store = FileStore::new();
    let id = file_store.add_file(path, src);

    // contract and struct types aren't worth printing
    let type_aliases = module
        .all_items(db)
        .iter()
        .filter_map(|def| match def {
            Item::Type(TypeDef::Alias(alias)) => {
                Some((alias.data(db).ast.span, alias.typ(db).unwrap()))
            }
            _ => None,
        })
        .collect::<Vec<(Span, Type)>>();

    let struct_fields: Vec<(Span, Type)> = module
        .all_structs(db)
        .iter()
        .map(|struc| {
            struc
                .all_fields(db)
                .iter()
                .map(|field| (field.data(db).ast.span, field.typ(db).unwrap().into()))
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect();

    let contract_ids = module.all_contracts(db);
    let contract_fields: Vec<(Span, Type)> = contract_ids
        .iter()
        .map(|contract| {
            contract
                .all_fields(db)
                .iter()
                .map(|field| (field.data(db).ast.span, field.typ(db).unwrap()))
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect();
    let event_fields = contract_ids
        .iter()
        .map(|contract| {
            contract
                .all_events(db)
                .iter()
                .map(|event| {
                    // Event field spans are a bit of a hassle right now
                    event
                        .data(db)
                        .ast
                        .kind
                        .fields
                        .iter()
                        .map(|node| node.span)
                        .zip(
                            event
                                .typ(db)
                                .fields
                                .iter()
                                .map(|field| field.typ.clone().unwrap()),
                        )
                        .collect::<Vec<(Span, FixedSize)>>()
                })
                .flatten()
                .collect::<Vec<(Span, FixedSize)>>()
        })
        .flatten()
        .collect::<Vec<(Span, FixedSize)>>();

    let all_function_ids: Vec<items::FunctionId> = contract_ids
        .iter()
        .map(|contract| contract.all_functions(db).as_ref().clone())
        .flatten()
        .collect();

    let function_sigs = all_function_ids
        .iter()
        .map(|fun| (fun.data(db).ast.span, fun.signature(db)))
        .collect::<Vec<(Span, Rc<FunctionSignature>)>>();

    let all_function_bodies: Vec<Rc<context::FunctionBody>> =
        all_function_ids.iter().map(|func| func.body(db)).collect();

    let expressions = all_function_bodies
        .iter()
        .map(|body| lookup_spans(&body.expressions, &body.spans))
        .flatten()
        .collect::<Vec<_>>();
    let emits = all_function_bodies
        .iter()
        .map(|body| {
            lookup_spans(&body.emits, &body.spans)
                .into_iter()
                .map(|(span, eventid)| (span, eventid.typ(db)))
                .collect::<Vec<(Span, Rc<Event>)>>()
        })
        .flatten()
        .collect::<Vec<_>>();
    let declarations = all_function_bodies
        .iter()
        .map(|body| lookup_spans(&body.var_decl_types, &body.spans))
        .flatten()
        .collect::<Vec<_>>();
    let calls = all_function_bodies
        .iter()
        .map(|body| lookup_spans(&body.calls, &body.spans))
        .flatten()
        .collect::<Vec<_>>();

    let diagnostics = [
        build_display_diagnostics(&type_aliases),
        build_display_diagnostics(&struct_fields),
        build_display_diagnostics(&contract_fields),
        build_display_diagnostics(&event_fields),
        build_debug_diagnostics(&function_sigs),
        build_display_diagnostics(&expressions),
        build_debug_diagnostics(&emits),
        build_display_diagnostics(&declarations),
        build_debug_diagnostics(&calls),
    ]
    .concat();

    diagnostics_string(&diagnostics, id, &file_store)
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

fn build_debug_diagnostics<T: Debug>(spanned_attributes: &[(Span, T)]) -> Vec<Diagnostic> {
    spanned_attributes
        .iter()
        .map(|(span, attributes)| build_debug_diagnostic(*span, attributes))
        .collect::<Vec<_>>()
}

fn build_debug_diagnostic<T: Debug>(span: Span, attributes: &T) -> Diagnostic {
    // Hash the attributes and label the span with it.
    let label = Label::primary(span, format!("attributes hash: {}", hash(attributes)));
    Diagnostic {
        severity: Severity::Note,
        message: String::new(),
        labels: vec![label],
        notes: vec![format!("{:#?}", attributes)],
    }
}

fn build_display_diagnostics<T: Display>(spanned_attributes: &[(Span, T)]) -> Vec<Diagnostic> {
    spanned_attributes
        .iter()
        .map(|(span, attributes)| build_display_diagnostic(*span, attributes))
        .collect::<Vec<_>>()
}

fn build_display_diagnostic<T: Display>(span: Span, attributes: &T) -> Diagnostic {
    // Hash the attributes and label the span with it.
    let label = Label::primary(span, format!("{}", attributes));
    Diagnostic {
        severity: Severity::Note,
        message: String::new(),
        labels: vec![label],
        notes: vec![],
    }
}

fn hash<T: Debug>(item: &T) -> u64 {
    // Using the Hash trait on `item` here gives different hash values on wasm vs linux,
    // so we hash the debug string instead.
    let mut s = DefaultHasher::new();
    format!("{:?}", item).hash(&mut s);
    s.finish()
}
