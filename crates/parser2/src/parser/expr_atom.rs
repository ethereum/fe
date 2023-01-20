use std::cell::RefCell;

use crate::{parser::path, SyntaxKind};

use super::{
    attr::parse_attr_list, define_scope, expr::parse_expr, parse_pat, stmt::parse_stmt,
    token_stream::TokenStream, Parser,
};

pub(super) fn _parse_expr_atom<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    use SyntaxKind::*;
    match parser.current_kind() {
        Some(Int | String) => parser.parse(LitExprScope::default(), None),
        Some(IfKw) => parser.parse(IfExprScope::default(), None),
        Some(MatchKw) => parser.parse(MatchExprScope::default(), None),
        Some(LBrace) => parser.parse(BlockExprScope::default(), None),
        Some(LParen) => parser.parse(ParenScope::default(), None),
        Some(LBracket) => parser.parse(ArrayScope::default(), None),
        Some(kind) if path::is_path_header(kind) => {
            let checkpoint = parser.checkpoint();
            let success = parser.parse(path::PathScope::default(), None);
            if success && parser.peek_non_trivia(true) == Some(LBrace) {
                parser.bump_trivias(true);
                parser.parse(RecordInitExprScope::default(), Some(checkpoint))
            } else {
                success
            }
        }
        _ => {
            parser.error_and_recover("expected expression", None);
            false
        }
    }
}

define_scope! {
    BlockExprScope,
    BlockExpr,
    Override(
        RBrace,
        Newline,
        LetKw,
        ForKw,
        WhileKw,
        ContinueKw,
        BreakKw,
        AssertKw,
        ReturnKw
    )
}
impl super::Parse for BlockExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);
        parser.bump_trivias(true);

        loop {
            if parser.current_kind() == Some(SyntaxKind::RBrace) || parser.current_kind().is_none()
            {
                break;
            }
            let checkpoint = parse_attr_list(parser);
            if !parse_stmt(parser, checkpoint) {
                continue;
            }

            parser.bump_trivias(false);
            if !parser.bump_if(SyntaxKind::Newline)
                && parser.peek_non_trivia(true) != Some(SyntaxKind::RBrace)
            {
                parser.error_and_recover("expected newline after statement", None);
            }
            parser.bump_trivias(true);
        }

        if !parser.bump_if(SyntaxKind::RBrace) {
            parser.error_and_bump_until("expected `}`", None, SyntaxKind::RBrace);
        }
    }
}

define_scope! { IfExprScope, IfExpr, Inheritance }
impl super::Parse for IfExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::IfKw);

        if parser.peek_non_trivia(true) != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected `{`", None);
            return;
        }
        parser.bump_trivias(true);
        parser.parse(BlockExprScope::default(), None);

        if parser.peek_non_trivia(true) == Some(SyntaxKind::ElseKw) {
            parser.bump_trivias(true);
            parser.bump_expected(SyntaxKind::ElseKw);

            if !matches!(
                parser.peek_non_trivia(true),
                Some(SyntaxKind::LBrace | SyntaxKind::IfKw)
            ) {
                parser.error_and_recover("expected `{` or `if` after `else`", None);
                parse_expr(parser);
            }
        }
    }
}

define_scope! { MatchExprScope, MatchExpr, Inheritance }
impl super::Parse for MatchExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::MatchKw);

        parser.bump_trivias(true);
        parse_expr(parser);

        if parser.peek_non_trivia(true) != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected `{`", None);
        }
        parser.bump_trivias(true);
        parser.parse(MatchArmListScope::default(), None);
    }
}

define_scope! { MatchArmListScope, MatchArmList, Override(SyntaxKind::Newline, SyntaxKind::RBrace) }
impl super::Parse for MatchArmListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);

        loop {
            parser.bump_trivias(true);
            if matches!(
                parser.peek_non_trivia(true),
                Some(SyntaxKind::RBrace) | None
            ) {
                break;
            }
            parser.parse(MatchArmScope::default(), None);
            parser.bump_trivias(true);
        }

        parser.bump_trivias(true);
    }
}

define_scope! { MatchArmScope, MatchArm, Inheritance }
impl super::Parse for MatchArmScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parse_pat(parser) {
            return;
        }

        if parser.peek_non_trivia(true) != Some(SyntaxKind::FatArrow) {
            parser.error_and_recover("expected `=>`", None);
            return;
        }
        parser.bump_trivias(true);
        parser.bump_expected(SyntaxKind::FatArrow);

        parser.bump_trivias(true);
        parse_expr(parser);

        if parser.peek_non_trivia(false) != Some(SyntaxKind::Newline) {
            parser.error_and_bump_until(
                "expected newline after match arm",
                None,
                SyntaxKind::Newline,
            );
        }
    }
}

define_scope! { LitExprScope, LitExpr, Inheritance }
impl super::Parse for LitExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        match parser.current_kind() {
            Some(SyntaxKind::Int | SyntaxKind::String) => parser.bump(),
            _ => unreachable!(),
        }
    }
}

define_scope! { RecordInitExprScope, RecordInitExpr, Inheritance }
impl super::Parse for RecordInitExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.parse(RecordFieldListScope::default(), None);
    }
}

define_scope! { RecordFieldListScope, RecordFieldList, Override(SyntaxKind::RBrace, SyntaxKind::Comma) }
impl super::Parse for RecordFieldListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);
        parser.bump_trivias(true);

        if parser.bump_if(SyntaxKind::LBrace) {
            return;
        }

        parser.parse(RecordFieldScope::default(), None);
        parser.bump_trivias(true);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.bump_trivias(true);
            parser.parse(RecordFieldScope::default(), None);
            parser.bump_trivias(true);
        }

        if !parser.bump_if(SyntaxKind::RBrace) {
            parser.error_and_bump_until("expected `}`", None, SyntaxKind::RBrace);
        }
    }
}

define_scope! { RecordFieldScope, RecordField, Inheritance }
impl super::Parse for RecordFieldScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected identifier", None);
        }

        parser.bump_trivias(true);
        if !parser.bump_if(SyntaxKind::Colon) {
            parser.error_and_recover("expected `:`", None);
        }

        parser.bump_trivias(true);
        parse_expr(parser);
    }
}

// We can't use `define_scope` here since the `syntax_kind` of the scope can be
// determined after parsing.
#[derive(Debug, Clone)]
struct ParenScope {
    syntax_kind: RefCell<SyntaxKind>,
    recovery_method: super::RecoveryMethod,
}
impl Default for ParenScope {
    fn default() -> Self {
        Self {
            syntax_kind: SyntaxKind::ParenExpr.into(),
            recovery_method: super::RecoveryMethod::override_(&[
                SyntaxKind::RParen,
                SyntaxKind::Comma,
            ]),
        }
    }
}
impl super::ParsingScope for ParenScope {
    fn recovery_method(&self) -> &super::RecoveryMethod {
        &self.recovery_method
    }
    fn syntax_kind(&self) -> SyntaxKind {
        *self.syntax_kind.borrow()
    }
}
impl super::Parse for ParenScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);
        parser.bump_trivias(true);

        if parser.bump_if(SyntaxKind::RParen) {
            *self.syntax_kind.borrow_mut() = SyntaxKind::TupleExpr;
            return;
        }

        parse_expr(parser);
        parser.bump_trivias(true);
        while parser.bump_if(SyntaxKind::Comma) {
            *self.syntax_kind.borrow_mut() = SyntaxKind::TupleExpr;
            if parser.peek_non_trivia(true) == Some(SyntaxKind::RParen) {
                parser.bump_trivias(true);
                break;
            }
            parse_expr(parser);
            parser.bump_trivias(true);
        }

        if !parser.bump_if(SyntaxKind::RParen) {
            parser.error_and_bump_until("expected `)`", None, SyntaxKind::RParen);
        }
    }
}

// We can't use `define_scope` here since the `syntax_kind` of the scope can be
// determined after parsing.
#[derive(Debug, Clone)]
struct ArrayScope {
    syntax_kind: RefCell<SyntaxKind>,
    recovery_method: super::RecoveryMethod,
}
impl Default for ArrayScope {
    fn default() -> Self {
        Self {
            syntax_kind: SyntaxKind::ArrayExpr.into(),
            recovery_method: super::RecoveryMethod::override_(&[
                SyntaxKind::RBracket,
                SyntaxKind::Comma,
                SyntaxKind::SemiColon,
            ]),
        }
    }
}
impl super::ParsingScope for ArrayScope {
    fn recovery_method(&self) -> &super::RecoveryMethod {
        &self.recovery_method
    }
    fn syntax_kind(&self) -> SyntaxKind {
        *self.syntax_kind.borrow()
    }
}
impl super::Parse for ArrayScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBracket);
        parser.bump_trivias(true);

        if parser.bump_if(SyntaxKind::RBracket) {
            return;
        }

        parse_expr(parser);
        parser.bump_trivias(true);

        if parser.bump_if(SyntaxKind::SemiColon) {
            parser.bump_trivias(true);
            *self.syntax_kind.borrow_mut() = SyntaxKind::ArrayRepExpr;
            parse_expr(parser);
        } else {
            while parser.bump_if(SyntaxKind::Comma) {
                parser.bump_trivias(true);
                parse_expr(parser);
                parser.bump_trivias(true);
            }
        }

        parser.bump_trivias(true);
        if !parser.bump_if(SyntaxKind::RBracket) {
            parser.error_and_bump_until("expected `]`", None, SyntaxKind::RBracket);
        }
    }
}
