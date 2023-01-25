use fe_parser2::{
    lexer,
    parser::{
        expr::parse_expr, item::ItemListScope, parse_pat, stmt::parse_stmt, Parser, RootScope,
    },
    syntax_node::SyntaxNode,
    SyntaxKind,
};

fe_compiler_test_utils::build_debug_snap_tests! {
    "parser2/test_files/syntax_node/structs",
    "parser2/test_files/syntax_node/structs",
    test_item_list
}
fn test_item_list(input: &str) -> SyntaxNode {
    let runner = TestRunner::new(|parser| {
        parser.parse(ItemListScope::default(), None);
    });
    runner.run(input)
}

fe_compiler_test_utils::build_debug_snap_tests! {
    "parser2/test_files/syntax_node/pats",
    "parser2/test_files/syntax_node/pats",
    test_pat
}
fn test_pat(input: &str) -> SyntaxNode {
    let runner = TestRunner::new(|parser| {
        while parser.current_kind().is_some() {
            parse_pat(parser);
        }
    });
    runner.run(input)
}

fe_compiler_test_utils::build_debug_snap_tests! {
    "parser2/test_files/syntax_node/exprs",
    "parser2/test_files/syntax_node/exprs",
    test_expr
}
fn test_expr(input: &str) -> SyntaxNode {
    let runner = TestRunner::new(|parser| {
        parser.set_newline_as_trivia(false);

        bump_newlines(parser);
        while parser.current_kind().is_some() {
            bump_newlines(parser);
            parse_expr(parser);
            bump_newlines(parser);
        }
    });
    runner.run(input)
}

fe_compiler_test_utils::build_debug_snap_tests! {
    "parser2/test_files/syntax_node/stmts",
    "parser2/test_files/syntax_node/stmts",
    test_stmt
}

fn test_stmt(input: &str) -> SyntaxNode {
    let runner = TestRunner::new(|parser| {
        parser.set_newline_as_trivia(false);

        bump_newlines(parser);
        while parser.current_kind().is_some() {
            bump_newlines(parser);
            parse_stmt(parser, None);
            bump_newlines(parser);
        }
    });
    runner.run(input)
}

fe_compiler_test_utils::build_debug_snap_tests!(
    "parser2/test_files/syntax_node/items",
    "parser2/test_files/syntax_node/items",
    test_item_list
);

struct TestRunner<F>
where
    F: Fn(&mut Parser<lexer::Lexer>),
{
    f: F,
}

impl<F> TestRunner<F>
where
    F: Fn(&mut Parser<lexer::Lexer>),
{
    fn new(f: F) -> Self {
        Self { f }
    }

    fn run(&self, input: &str) -> SyntaxNode {
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let checkpoint = parser.enter(RootScope::default(), None);
        (self.f)(&mut parser);
        parser.leave(checkpoint);

        let (cst, errors) = parser.finish();

        for error in &errors {
            println!("{}@{:?}", error.msg, error.range);
        }
        assert! {errors.is_empty()}
        assert!(input == cst.to_string());

        cst
    }
}

fn bump_newlines(parser: &mut Parser<lexer::Lexer>) {
    while parser.current_kind() == Some(SyntaxKind::Newline) {
        parser.bump();
    }
}
