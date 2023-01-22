use crate::SyntaxKind;

use super::{
    define_scope, expr::parse_expr, pat::parse_pat, token_stream::TokenStream, type_::parse_type,
    Checkpoint, Parser,
};

pub(super) fn parse_stmt<S: TokenStream>(
    parser: &mut Parser<S>,
    checkpoint: Option<Checkpoint>,
) -> bool {
    use SyntaxKind::*;

    match parser.current_kind() {
        Some(LetKw) => parser.parse(LetStmtScope::default(), checkpoint),
        Some(ForKw) => parser.parse(ForStmtScope::default(), checkpoint),
        Some(WhileKw) => parser.parse(WhileStmtScope::default(), checkpoint),
        Some(ContinueKw) => parser.parse(ContinueStmtScope::default(), checkpoint),
        Some(BreakKw) => parser.parse(BreakStmtScope::default(), checkpoint),
        Some(AssertKw) => parser.parse(AssertStmtScope::default(), checkpoint),
        Some(ReturnKw) => parser.parse(ReturnStmtScope::default(), checkpoint),
        _ => {
            parser.start_dry_run();
            if parser.parse(AssignStmtScope::default(), checkpoint).0 {
                parser.end_dry_run();
                parser.parse(AssignStmtScope::default(), checkpoint)
            } else {
                parser.end_dry_run();
                parser.parse(ExprStmtScope::default(), checkpoint)
            }
        }
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
            if !parse_type(parser, None) {
                return;
            }
        }

        if parser.current_kind() == Some(SyntaxKind::Eq) {
            parser.bump_expected(SyntaxKind::Eq);
            parse_expr(parser);
        }
    }
}

define_scope! { ForStmtScope, ForStmt, Inheritance }
impl super::Parse for ForStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ForKw);
        if !parse_pat(parser) {
            return;
        }

        if !parser.bump_if(SyntaxKind::InKw) {
            parser.error_and_recover("expected `in` keyword", None);
            return;
        }
        if !parse_expr(parser) {
            return;
        }

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected block", None);
            return;
        }
        parse_expr(parser);
    }
}

define_scope! { WhileStmtScope, WhileStmt, Inheritance }
impl super::Parse for WhileStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::WhileKw);
        if !parse_expr(parser) {
            return;
        }

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected block", None);
            return;
        }
        parse_expr(parser);
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

define_scope! { AssertStmtScope, AssertStmt, Inheritance }
impl super::Parse for AssertStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::AssertKw);
        parser.set_newline_as_trivia(false);
        parse_expr(parser);
    }
}

define_scope! { ReturnStmtScope, ReturnStmt, Inheritance }
impl super::Parse for ReturnStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::ReturnStmt);
        parser.set_newline_as_trivia(false);
        parse_expr(parser);
    }
}

define_scope! { AssignStmtScope, AssignStmt, Inheritance }
impl super::Parse for AssignStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parse_pat(parser) {
            return;
        }
        parser.set_newline_as_trivia(false);
        if !parser.bump_if(SyntaxKind::Eq) {
            parser.error_and_recover("expected `=` keyword", None);
            return;
        }
        parse_expr(parser);
    }
}

define_scope! { ExprStmtScope, ExprStmt, Inheritance }
impl super::Parse for ExprStmtScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_expr(parser);
    }
}
