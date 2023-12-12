use std::convert::Infallible;

use crate::SyntaxKind;

use super::{
    define_scope,
    expr::{parse_expr, parse_expr_no_struct},
    expr_atom::BlockExprScope,
    pat::parse_pat,
    token_stream::TokenStream,
    type_::parse_type,
    ErrProof, Parser, Recovery,
};

pub fn parse_stmt<S: TokenStream>(parser: &mut Parser<S>) -> Result<(), Recovery<ErrProof>> {
    use SyntaxKind::*;

    match parser.current_kind() {
        Some(LetKw) => parser.parse(LetStmtScope::default()),
        Some(ForKw) => parser.parse(ForStmtScope::default()),
        Some(WhileKw) => parser.parse(WhileStmtScope::default()),
        Some(ContinueKw) => parser
            .parse(ContinueStmtScope::default())
            .map_err(|e| e.into()),
        Some(BreakKw) => parser
            .parse(BreakStmtScope::default())
            .map_err(|e| e.into()),
        Some(ReturnKw) => parser.parse(ReturnStmtScope::default()),
        _ => parser.parse(ExprStmtScope::default()),
    }
}

define_scope! { LetStmtScope, LetStmt, Inheritance }
impl super::Parse for LetStmtScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::LetKw);
        parser.set_newline_as_trivia(false);
        parse_pat(parser)?; // xxx "expected pattern" error msg

        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.bump_expected(SyntaxKind::Colon);
            parse_type(parser, None)?;
        }

        if parser.bump_if(SyntaxKind::Eq) {
            parse_expr(parser)?;
        }
        Ok(())
    }
}

define_scope! { ForStmtScope, ForStmt, Inheritance }
impl super::Parse for ForStmtScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ForKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::InKw, SyntaxKind::LBrace]);
        parse_pat(parser)?;

        if parser.find_and_pop(SyntaxKind::InKw, None)? {
            parser.bump();
        }
        parse_expr_no_struct(parser)?;

        if parser.find_and_pop(SyntaxKind::LBrace, None)? {
            parser.parse(BlockExprScope::default())?;
        }
        Ok(())
    }
}

define_scope! { WhileStmtScope, WhileStmt, Inheritance }
impl super::Parse for WhileStmtScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::WhileKw);

        parser.set_scope_recovery_stack(&[SyntaxKind::LBrace]);
        parse_expr_no_struct(parser)?;

        if parser.find_and_pop(SyntaxKind::LBrace, None)? {
            parser.parse(BlockExprScope::default())?;
        }
        Ok(())
    }
}

define_scope! { ContinueStmtScope, ContinueStmt, Inheritance }
impl super::Parse for ContinueStmtScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ContinueKw);
        Ok(())
    }
}

define_scope! { BreakStmtScope, BreakStmt, Inheritance }
impl super::Parse for BreakStmtScope {
    type Error = Infallible;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::BreakKw);
        Ok(())
    }
}

define_scope! { ReturnStmtScope, ReturnStmt, Inheritance }
impl super::Parse for ReturnStmtScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.bump_expected(SyntaxKind::ReturnKw);
        parser.set_newline_as_trivia(false);

        if !matches!(
            parser.current_kind(),
            None | Some(SyntaxKind::Newline | SyntaxKind::RBrace)
        ) {
            parse_expr(parser)?;
        }
        Ok(())
    }
}

define_scope! { ExprStmtScope, ExprStmt, Inheritance }
impl super::Parse for ExprStmtScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_expr(parser)
    }
}
