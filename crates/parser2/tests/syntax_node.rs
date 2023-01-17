use fe_parser2::syntax_node::SyntaxNode;

#[allow(unused)]
fn build_cst(input: &str) -> SyntaxNode {
    let (cst, errors) = fe_parser2::parse_source_file(input);
    for error in &errors {
        println!("{}", error.msg);
    }
    assert! {errors.is_empty()}
    cst
}

fe_compiler_test_utils::build_debug_snap_tests! {
    "parser2/test_files/syntax_node",
    "parser2/test_files/syntax_node",
    build_cst
}
