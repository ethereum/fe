#![allow(unused)]

use fe_parser2::{
    lexer,
    parser::{Parser, RootScope},
    syntax_node::SyntaxNode,
    SyntaxKind,
};

pub struct TestRunner<F>
where
    F: Fn(&mut Parser<lexer::Lexer>),
{
    f: F,
    should_success: bool,
}

impl<F> TestRunner<F>
where
    F: Fn(&mut Parser<lexer::Lexer>),
{
    pub fn new(f: F, should_success: bool) -> Self {
        Self { f, should_success }
    }

    pub fn run(&self, input: &str) -> SyntaxNode {
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let checkpoint = parser.enter(RootScope::default(), None);
        (self.f)(&mut parser);
        parser.leave(checkpoint);

        let (cst, errors) = parser.finish();

        for error in &errors {
            println!("{}@{:?}", error.msg, error.range);
        }
        if self.should_success {
            assert! {errors.is_empty()}
        } else {
            assert! {!errors.is_empty()}
        }
        assert!(input == cst.to_string());

        cst
    }
}

pub fn bump_newlines(parser: &mut Parser<lexer::Lexer>) {
    while parser.current_kind() == Some(SyntaxKind::Newline) {
        parser.bump();
    }
}
