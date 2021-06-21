use fe_analyzer::context::Context;
use fe_analyzer::errors::AnalyzerError;
use fe_compiler::lowering;
use fe_parser::ast as fe;
use regex::Regex;

use rstest::rstest;

use fe_common::assert_strings_eq;
use fe_common::diagnostics::print_diagnostics;
use fe_common::files::{FileStore, SourceFileId};
use fe_common::utils::ron::{to_ron_string_pretty, Diff};

fn lower_file(src: &str, id: SourceFileId, files: &FileStore) -> fe::Module {
    let fe_module = parse_file(src, id, files);
    let context = analyze(&fe_module, id, files);
    lowering::lower(&context, fe_module)
}

fn analyze(module: &fe::Module, id: SourceFileId, files: &FileStore) -> Context {
    match fe_analyzer::analyze(&module, id) {
        Ok(context) => context,
        Err(AnalyzerError(diagnostics)) => {
            print_diagnostics(&diagnostics, &files);
            panic!("analysis failed");
        }
    }
}

fn parse_file(src: &str, id: SourceFileId, files: &FileStore) -> fe::Module {
    match fe_parser::parse_file(src, id) {
        Ok((module, diags)) if diags.is_empty() => module,
        Ok((_, diags)) | Err(diags) => {
            print_diagnostics(&diags, files);
            panic!("failed to parse file");
        }
    }
}

fn replace_spans(input: String) -> String {
    let span_re = Regex::new(r"\n *span: Span\(\n\s*start: \d+,\n\s*end: \d+.\n\s*\),").unwrap();
    span_re.replace_all(&input, "").to_string()
}

#[rstest(
    fixture,
    case("aug_assign"),
    case("base_tuple"),
    case("list_expressions"),
    case("return_unit"),
    case("unit_implicit"),
    case("init"),
    case("custom_empty_type"),
    case("nested_tuple"),
    case("map_tuple"),
    case("type_alias_tuple")
//    case("array_tuple") // TODO: analysis fails on "arrays can only hold primitive types"
)]
fn test_lowering(fixture: &str) {
    let mut files = FileStore::new();
    let (src, src_id) = files
        .load_file(&format!("fixtures/lowering/{}.fe", fixture))
        .expect("unable to src read fixture file");
    let (expected_lowered, el_id) = files
        .load_file(&format!("fixtures/lowering/{}_lowered.fe", fixture))
        .expect("unable to read lowered fixture file");

    let expected_lowered_ast = parse_file(&expected_lowered, el_id, &files);
    let actual_lowered_ast = lower_file(&src, src_id, &files);

    assert_strings_eq!(
        replace_spans(to_ron_string_pretty(&expected_lowered_ast).unwrap()),
        replace_spans(to_ron_string_pretty(&actual_lowered_ast).unwrap()),
    );

    // TODO: the analyzer rejects lowered nested tuples, because
    //  nested structs aren't supported yet. we should move the
    //  not-yet-implemented error to the compiler.
    //
    // fe_analyzer::analyze(&actual_lowered_ast, src_id)
    //     .expect("analysis of the lowered module failed");
}
