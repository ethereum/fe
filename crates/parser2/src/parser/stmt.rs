use crate::SyntaxKind;

use super::{
    define_scope,
    expr::{parse_expr, parse_expr_no_struct},
    expr_atom::BlockExprScope,
    pat::parse_pat,
    token_stream::TokenStream,
    type_::parse_type,
    Checkpoint, Parser,
};

pub fn parse_stmt<S: TokenStream>(parser: &mut Parser<S>, checkpoint: Option<Checkpoint>) -> bool {
    use SyntaxKind::*;

    match parser.current_kind() {
        Some(LetKw) => parser.parse(LetStmtScope::default(), checkpoint),
        Some(ForKw) => parser.parse(ForStmtScope::default(), checkpoint),
        Some(WhileKw) => parser.parse(WhileStmtScope::default(), checkpoint),
        Some(ContinueKw) => parser.parse(ContinueStmtScope::default(), checkpoint),
        Some(BreakKw) => parser.parse(BreakStmtScope::default(), checkpoint),
        Some(ReturnKw) => parser.parse(ReturnStmtScope::default(), checkpoint),
        _ => parser.parse(ExprStmtScope::default(), checkpoint),
    }
    .0
}

define_scope! { LetStmtScope, LetStmt, Inheritance }
impl super::Parse for LetStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LetKw);
        parser.set_newline_as_trivia(false);
        if !parse_pat(parser) {
            parser.error_and_recover("expected pattern", None);
            return;
        }
        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.bump_expected(SyntaxKind::Colon);
            parse_type(parser, None);
        }

        if parser.bump_if(SyntaxKind::Eq) {
            parse_expr(parser);
        }
    }
}

define_scope! { ForStmtScope, ForStmt, Inheritance }
impl super::Parse for ForStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ForKw);

        parser.with_next_expected_tokens(parse_pat, &[SyntaxKind::InKw, SyntaxKind::LBrace]);

        parser.bump_or_recover(SyntaxKind::InKw, "expected `in` keyword", None);

        parser.with_next_expected_tokens(parse_expr_no_struct, &[SyntaxKind::LBrace]);

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected block", None);
            return;
        }
        parser.parse(BlockExprScope::default(), None);
    }
}

define_scope! { WhileStmtScope, WhileStmt, Inheritance }
impl super::Parse for WhileStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::WhileKw);

        parser.with_next_expected_tokens(parse_expr_no_struct, &[SyntaxKind::LBrace]);

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected block", None);
            return;
        }
        parser.parse(BlockExprScope::default(), None);
    }
}

define_scope! { ContinueStmtScope, ContinueStmt, Inheritance }
impl super::Parse for ContinueStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ContinueKw);
    }
}

define_scope! { BreakStmtScope, BreakStmt, Inheritance }
impl super::Parse for BreakStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::BreakKw);
    }
}

define_scope! { ReturnStmtScope, ReturnStmt, Inheritance }
impl super::Parse for ReturnStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ReturnKw);
        parser.set_newline_as_trivia(false);

        let has_val = parser.dry_run(parse_expr);
        if has_val {
            parse_expr(parser);
        }
    }
}

define_scope! { ExprStmtScope, ExprStmt, Inheritance }
impl super::Parse for ExprStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_expr(parser);
    }
}
