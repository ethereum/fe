use fe_analyzer::context::Context;
use fe_analyzer::errors::AnalyzerError;
use fe_common::diagnostics::{diagnostics_string, print_diagnostics, CsLabel, Diagnostic};
use fe_common::files::{FileStore, SourceFileId};
use fe_parser::node::Span;
use insta::assert_snapshot;
use rstest::rstest;
use std::collections::hash_map::DefaultHasher;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

#[rstest(
    fixture,
    case::erc20_token("demos/erc20_token.fe"),
    case::guest_book("demos/guest_book.fe"),
    case::uniswap("demos/uniswap.fe"),
    case::address_bytes10_map("features/address_bytes10_map.fe"),
    case::assert("features/assert.fe"),
    case::aug_assign("features/aug_assign.fe"),
    case::base_tuple("features/base_tuple.fe"),
    case::call_statement_with_args("features/call_statement_with_args.fe"),
    case::call_statement_with_args_2("features/call_statement_with_args_2.fe"),
    case::call_statement_without_args("features/call_statement_without_args.fe"),
    case::checked_arithmetic("features/checked_arithmetic.fe"),
    case::constructor("features/constructor.fe"),
    case::create2_contract("features/create2_contract.fe"),
    case::create_contract("features/create_contract.fe"),
    case::create_contract_from_init("features/create_contract_from_init.fe"),
    case::empty("features/empty.fe"),
    case::events("features/events.fe"),
    case::external_contract("features/external_contract.fe"),
    case::for_loop_with_break("features/for_loop_with_break.fe"),
    case::for_loop_with_continue("features/for_loop_with_continue.fe"),
    case::for_loop_with_static_array("features/for_loop_with_static_array.fe"),
    case::if_statement("features/if_statement.fe"),
    case::if_statement_2("features/if_statement_2.fe"),
    case::if_statement_with_block_declaration("features/if_statement_with_block_declaration.fe"),
    case::keccak("features/keccak.fe"),
    case::math("features/math.fe"),
    case::multi_param("features/multi_param.fe"),
    case::nested_map("features/nested_map.fe"),
    case::numeric_sizes("features/numeric_sizes.fe"),
    case::ownable("features/ownable.fe"),
    case::return_addition_i256("features/return_addition_i256.fe"),
    case::return_addition_u128("features/return_addition_u128.fe"),
    case::return_addition_u256("features/return_addition_u256.fe"),
    case::return_array("features/return_array.fe"),
    case::return_bitwiseand_u128("features/return_bitwiseand_u128.fe"),
    case::return_bitwiseand_u256("features/return_bitwiseand_u256.fe"),
    case::return_bitwiseor_u256("features/return_bitwiseor_u256.fe"),
    case::return_bitwiseshl_u256("features/return_bitwiseshl_u256.fe"),
    case::return_bitwiseshr_i256("features/return_bitwiseshr_i256.fe"),
    case::return_bitwiseshr_u256("features/return_bitwiseshr_u256.fe"),
    case::return_bitwisexor_u256("features/return_bitwisexor_u256.fe"),
    case::return_bool_false("features/return_bool_false.fe"),
    case::return_bool_inverted("features/return_bool_inverted.fe"),
    case::return_bool_op_and("features/return_bool_op_and.fe"),
    case::return_bool_op_or("features/return_bool_op_or.fe"),
    case::return_bool_true("features/return_bool_true.fe"),
    case::return_builtin_attributes("features/return_builtin_attributes.fe"),
    case::return_division_i256("features/return_division_i256.fe"),
    case::return_division_u256("features/return_division_u256.fe"),
    case::return_empty_tuple("features/return_unit.fe"),
    case::return_eq_u256("features/return_eq_u256.fe"),
    case::return_gt_i256("features/return_gt_i256.fe"),
    case::return_gt_u256("features/return_gt_u256.fe"),
    case::return_gte_i256("features/return_gte_i256.fe"),
    case::return_gte_u256("features/return_gte_u256.fe"),
    case::return_i128_cast("features/return_i128_cast.fe"),
    case::return_i256("features/return_i256.fe"),
    case::return_identity_u8("features/return_identity_u8.fe"),
    case::return_identity_u16("features/return_identity_u16.fe"),
    case::return_identity_u32("features/return_identity_u32.fe"),
    case::return_identity_u64("features/return_identity_u64.fe"),
    case::return_identity_u128("features/return_identity_u128.fe"),
    case::return_identity_u256("features/return_identity_u256.fe"),
    case::return_lt_i256("features/return_lt_i256.fe"),
    case::return_lt_u128("features/return_lt_u128.fe"),
    case::return_lt_u256("features/return_lt_u256.fe"),
    case::return_lte_i256("features/return_lte_i256.fe"),
    case::return_lte_u256("features/return_lte_u256.fe"),
    case::return_mod_i256("features/return_mod_i256.fe"),
    case::return_mod_u256("features/return_mod_u256.fe"),
    case::return_msg_sig("features/return_msg_sig.fe"),
    case::return_multiplication_i256("features/return_multiplication_i256.fe"),
    case::return_multiplication_u256("features/return_multiplication_u256.fe"),
    case::return_noteq_u256("features/return_noteq_u256.fe"),
    case::return_pow_i256("features/return_pow_i256.fe"),
    case::return_pow_u256("features/return_pow_u256.fe"),
    case::return_subtraction_i256("features/return_subtraction_i256.fe"),
    case::return_subtraction_u256("features/return_subtraction_u256.fe"),
    case::return_u128_cast("features/return_u128_cast.fe"),
    case::return_u256("features/return_u256.fe"),
    case::return_u256_from_called_fn("features/return_u256_from_called_fn.fe"),
    case::return_u256_from_called_fn_with_args("features/return_u256_from_called_fn_with_args.fe"),
    case::revert("features/revert.fe"),
    case::self_address("features/self_address.fe"),
    case::sized_vals_in_sto("features/sized_vals_in_sto.fe"),
    case::strings("features/strings.fe"),
    case::structs("features/structs.fe"),
    case::ternary_expression("features/ternary_expression.fe"),
    case::two_contracts("features/two_contracts.fe"),
    case::u8_u8_map("features/u8_u8_map.fe"),
    case::u16_u16_map("features/u16_u16_map.fe"),
    case::u32_u32_map("features/u32_u32_map.fe"),
    case::u64_u64_map("features/u64_u64_map.fe"),
    case::u128_u128_map("features/u128_u128_map.fe"),
    case::u256_u256_map("features/u256_u256_map.fe"),
    case::while_loop("features/while_loop.fe"),
    case::while_loop_with_break("features/while_loop_with_break.fe"),
    case::while_loop_with_break_2("features/while_loop_with_break_2.fe"),
    case::while_loop_with_continue("features/while_loop_with_continue.fe"),
    case::abi_encoding_stress("stress/abi_encoding_stress.fe"),
    case::data_copying_stress("stress/data_copying_stress.fe"),
    case::tuple_stress("stress/tuple_stress.fe")
)]
fn analysis(fixture: &str) {
    let mut files = FileStore::new();
    let (src, id) = files.load_file(&format!("fixtures/{}", fixture)).unwrap();
    let fe_module = match fe_parser::parse_file(&src, id) {
        Ok((module, _)) => module,
        Err(diags) => {
            print_diagnostics(&diags, &files);
            panic!("parsing failed");
        }
    };
    match fe_analyzer::analyze(&fe_module, id) {
        Ok(context) => assert_snapshot!(build_snapshot(fixture, &src, &context)),
        Err(AnalyzerError(diagnostics)) => {
            print_diagnostics(&diagnostics, &files);
            panic!("analysis failed");
        }
    }
}

fn build_snapshot(path: &str, src: &str, context: &Context) -> String {
    let mut file_store = FileStore::new();
    let id = file_store.add_file(path, src);

    let diagnostics = [
        build_diagnostics(id, &context.get_spanned_expressions()),
        build_diagnostics(id, &context.get_spanned_emits()),
        build_diagnostics(id, &context.get_spanned_functions()),
        build_diagnostics(id, &context.get_spanned_declarations()),
        build_diagnostics(id, &context.get_spanned_contracts()),
        build_diagnostics(id, &context.get_spanned_calls()),
        build_diagnostics(id, &context.get_spanned_events()),
        build_diagnostics(id, &context.get_spanned_type_descs()),
    ]
    .concat();

    format!(
        "{:#?}\n\n{}",
        context
            .get_module()
            .expect("context is missing module attributes"),
        diagnostics_string(&diagnostics, &file_store)
    )
}

fn build_diagnostics<T: Hash + Debug>(
    file_id: SourceFileId,
    spanned_attributes: &[(Span, T)],
) -> Vec<Diagnostic> {
    spanned_attributes
        .iter()
        .map(|(span, attributes)| build_attributes_diagnostic(file_id, span, attributes))
        .collect::<Vec<_>>()
}

fn build_attributes_diagnostic<T: Hash + Debug>(
    file_id: SourceFileId,
    span: &Span,
    attributes: &T,
) -> Diagnostic {
    // Hash the attributes and label the span with it.
    let label = CsLabel::primary(file_id, span.start..span.end)
        .with_message(format!("attributes hash: {}", hash(attributes)));
    Diagnostic::note()
        .with_labels(vec![label])
        .with_notes(vec![format!("{:#?}", attributes)])
}

fn hash<T: Hash>(item: &T) -> u64 {
    let mut s = DefaultHasher::new();
    item.hash(&mut s);
    s.finish()
}
