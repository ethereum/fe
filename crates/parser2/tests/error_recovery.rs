use fe_parser2::{
    parser::{expr::parse_expr, item::ItemListScope, stmt::parse_stmt},
    syntax_node::SyntaxNode,
};
mod test_runner;
use test_runner::*;
fe_compiler_test_utils::build_debug_snap_tests! {
    "parser2/test_files/error_recovery/items",
    "parser2/test_files/error_recovery/items",
    test_item_list
}
fn test_item_list(input: &str) -> SyntaxNode {
    let runner = TestRunner::new(
        |parser| {
            parser.parse(ItemListScope::default(), None);
        },
        false,
    );
    runner.run(input)
}

fe_compiler_test_utils::build_debug_snap_tests! {
    "parser2/test_files/error_recovery/exprs",
    "parser2/test_files/error_recovery/exprs",
    test_expr
}
fn test_expr(input: &str) -> SyntaxNode {
    let runner = TestRunner::new(
        |parser| {
            parser.set_newline_as_trivia(false);

            bump_newlines(parser);
            while parser.current_kind().is_some() {
                bump_newlines(parser);
                parse_expr(parser);
                bump_newlines(parser);
            }
        },
        false,
    );
    runner.run(input)
}

fe_compiler_test_utils::build_debug_snap_tests! {
    "parser2/test_files/error_recovery/stmts",
    "parser2/test_files/error_recovery/stmts",
    test_stmt
}
fn test_stmt(input: &str) -> SyntaxNode {
    let runner = TestRunner::new(
        |parser| {
            parser.set_newline_as_trivia(false);

            bump_newlines(parser);
            while parser.current_kind().is_some() {
                bump_newlines(parser);
                parse_stmt(parser, None);
                bump_newlines(parser);
            }
        },
        false,
    );
    runner.run(input)
}
