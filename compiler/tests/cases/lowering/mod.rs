use fe_compiler::lowering;
use fe_parser::ast as fe;
use regex::Regex;
use std::fs;

use fe_common::files::SourceFileId;
use rstest::rstest;

use fe_common::assert_strings_eq;
use fe_common::utils::ron::{to_ron_string_pretty, Diff};

fn lower_file(src: &str) -> fe::Module {
    let fe_module = parse_file(src);
    let context = fe_analyzer::analyze(&fe_module).expect("failed to get context");
    lowering::lower(&context, fe_module)
}

fn parse_file(src: &str) -> fe::Module {
    fe_parser::parse_file(src, SourceFileId(0))
        .expect("failed to parse file")
        .0
}

fn replace_spans(input: String) -> String {
    let span_re = Regex::new(r"span: Span\(\n\s*start: \d+,\n\s*end: \d+.\n\s*\),").unwrap();
    span_re.replace_all(&input, "[span omitted]").to_string()
}

#[rstest(
    fixture,
    case("aug_assign"),
    case("base_tuple"),
    case("list_expressions"),
    case("return_unit"),
    case("unit_implicit"),
    case("init"),
    case("custom_empty_type")
)]
fn test_lowering(fixture: &str) {
    let src = fs::read_to_string(format!("tests/cases/lowering/fixtures/{}.fe", fixture))
        .expect("unable to src read fixture file");
    let expected_lowered = fs::read_to_string(format!(
        "tests/cases/lowering/fixtures/{}_lowered.fe",
        fixture
    ))
    .expect("unable to read lowered fixture file");

    let expected_lowered_ast = parse_file(&expected_lowered);
    let actual_lowered_ast = lower_file(&src);

    assert_strings_eq!(
        replace_spans(to_ron_string_pretty(&expected_lowered_ast).unwrap()),
        replace_spans(to_ron_string_pretty(&actual_lowered_ast).unwrap()),
    );

    fe_analyzer::analyze(&actual_lowered_ast).expect("analysis of the lowered module failed");
}
