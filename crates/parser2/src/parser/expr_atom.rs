use rowan::Checkpoint;

use crate::{
    parser::{lit, path},
    SyntaxKind,
};

use super::{
    define_scope,
    expr::{parse_expr, parse_expr_no_struct},
    parse_pat,
    stmt::parse_stmt,
    token_stream::TokenStream,
    Parser,
};

pub(super) fn parse_expr_atom<S: TokenStream>(
    parser: &mut Parser<S>,
    allow_record_init: bool,
) -> (bool, Checkpoint) {
    use SyntaxKind::*;
    match parser.current_kind() {
        Some(IfKw) => parser.parse(IfExprScope::default(), None),
        Some(MatchKw) => parser.parse(MatchExprScope::default(), None),
        Some(LBrace) => parser.parse(BlockExprScope::default(), None),
        Some(LParen) => parser.parse(ParenScope::default(), None),
        Some(LBracket) => parser.parse(ArrayScope::default(), None),
        Some(kind) if lit::is_lit(kind) => parser.parse(LitExprScope::default(), None),
        Some(kind) if path::is_path_segment(kind) => {
            parser.parse(PathExprScope::new(allow_record_init), None)
        }
        _ => {
            parser.error_and_recover("expected expression", None);
            (false, parser.checkpoint())
        }
    }
}

define_scope! {
    pub(crate) BlockExprScope,
    BlockExpr,
    Override(
        RBrace,
        Newline,
        LetKw,
        ForKw,
        WhileKw,
        ContinueKw,
        BreakKw,
        ReturnKw
    )
}
impl super::Parse for BlockExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);

        loop {
            parser.set_newline_as_trivia(true);
            if parser.current_kind() == Some(SyntaxKind::RBrace) || parser.current_kind().is_none()
            {
                break;
            }
            if !parse_stmt(parser, None) {
                continue;
            }

            parser.set_newline_as_trivia(false);
            if !parser.bump_if(SyntaxKind::Newline)
                && parser.current_kind() != Some(SyntaxKind::RBrace)
            {
                parser.error_and_recover("expected newline after statement", None);
                parser.bump_if(SyntaxKind::Newline);
            }
        }

        if !parser.bump_if(SyntaxKind::RBrace) {
            parser.error_and_bump_until("expected `}`", None, SyntaxKind::RBrace);
            parser.bump_if(SyntaxKind::RBrace);
        }
    }
}

define_scope! { IfExprScope, IfExpr, Inheritance }
impl super::Parse for IfExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::IfKw);

        parser.with_next_expected_tokens(parse_expr_no_struct, &[SyntaxKind::LBrace]);

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected `{`", None);
            return;
        }
        parser.parse(BlockExprScope::default(), None);

        if parser.current_kind() == Some(SyntaxKind::ElseKw) {
            parser.with_next_expected_tokens(
                |parser| {
                    parser.bump_expected(SyntaxKind::ElseKw);
                },
                &[SyntaxKind::LBrace, SyntaxKind::IfKw],
            );

            if !matches!(
                parser.current_kind(),
                Some(SyntaxKind::LBrace | SyntaxKind::IfKw)
            ) {
                parser.error_and_recover("expected `{` or `if` after `else`", None);
                return;
            }
            parse_expr(parser);
        }
    }
}

define_scope! { MatchExprScope, MatchExpr, Inheritance }
impl super::Parse for MatchExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::MatchKw);

        parser.with_next_expected_tokens(parse_expr_no_struct, &[SyntaxKind::LBrace]);

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected `{`", None);
            return;
        }
        parser.parse(MatchArmListScope::default(), None);
    }
}

define_scope! { MatchArmListScope, MatchArmList, Override(SyntaxKind::Newline, SyntaxKind::RBrace) }
impl super::Parse for MatchArmListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);

        loop {
            parser.set_newline_as_trivia(true);
            if parser.current_kind() == Some(SyntaxKind::RBrace) || parser.current_kind().is_none()
            {
                break;
            }

            parser.parse(MatchArmScope::default(), None);

            parser.set_newline_as_trivia(false);
            if parser.current_kind() != Some(SyntaxKind::RBrace) {
                parser.bump_or_recover(
                    SyntaxKind::Newline,
                    "expected newline after match arm",
                    None,
                );
            }
        }

        parser.bump_or_recover(SyntaxKind::RBrace, "expected `}`", None);
    }
}

define_scope! { MatchArmScope, MatchArm, Inheritance }
impl super::Parse for MatchArmScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);

        parser.with_next_expected_tokens(parse_pat, &[SyntaxKind::FatArrow]);
        parser.bump_or_recover(SyntaxKind::FatArrow, "expected `=>`", None);

        parser.with_next_expected_tokens(parse_expr, &[SyntaxKind::RBrace, SyntaxKind::Newline]);
    }
}

define_scope! { pub(crate) LitExprScope, LitExpr, Inheritance }
impl super::Parse for LitExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.parse(lit::LitScope::default(), None);
    }
}

define_scope! { PathExprScope{ allow_record_init: bool }, PathExpr, Inheritance }
impl super::Parse for PathExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.with_recovery_tokens(
            |parser| parser.parse(path::PathScope::default(), None),
            &[SyntaxKind::LBrace],
        );
        if parser.current_kind() == Some(SyntaxKind::LBrace) && self.allow_record_init {
            self.set_kind(SyntaxKind::RecordInitExpr);
            parser.parse(RecordFieldListScope::default(), None);
        }
    }
}

define_scope! { RecordFieldListScope, RecordFieldList, Override(RBrace, Comma) }
impl super::Parse for RecordFieldListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);

        if parser.bump_if(SyntaxKind::RBrace) {
            return;
        }

        parser.with_next_expected_tokens(
            |parser| parser.parse(RecordFieldScope::default(), None),
            &[SyntaxKind::RBrace, SyntaxKind::Comma],
        );

        while parser.bump_if(SyntaxKind::Comma) {
            parser.with_next_expected_tokens(
                |parser| {
                    parser.parse(RecordFieldScope::default(), None);
                },
                &[SyntaxKind::RBrace, SyntaxKind::Comma],
            )
        }

        parser.bump_or_recover(SyntaxKind::RBrace, "expected `}`", None);
    }
}

define_scope! { RecordFieldScope, RecordField, Inheritance }
impl super::Parse for RecordFieldScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_if(SyntaxKind::Ident);

        if parser.bump_if(SyntaxKind::Colon) {
            parse_expr(parser);
        }
    }
}

define_scope! { ParenScope, ParenExpr, Override(RParen, Comma) }
impl super::Parse for ParenScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);

        if parser.bump_if(SyntaxKind::RParen) {
            self.set_kind(SyntaxKind::ParenExpr);
            return;
        }

        parser.with_next_expected_tokens(parse_expr, &[SyntaxKind::RParen, SyntaxKind::Comma]);
        while parser.bump_if(SyntaxKind::Comma) {
            self.set_kind(SyntaxKind::TupleExpr);
            parser.with_next_expected_tokens(parse_expr, &[SyntaxKind::RParen, SyntaxKind::Comma]);
        }

        parser.bump_or_recover(SyntaxKind::RParen, "expected `)`", None);
    }
}

define_scope! {
    ArrayScope,
    ArrayExpr,
    Override(RBracket, Comma, SemiColon)
}
impl super::Parse for ArrayScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBracket);

        if parser.bump_if(SyntaxKind::RBracket) {
            return;
        }

        parser.with_next_expected_tokens(
            parse_expr,
            &[
                SyntaxKind::SemiColon,
                SyntaxKind::Comma,
                SyntaxKind::RBracket,
            ],
        );

        if parser.bump_if(SyntaxKind::SemiColon) {
            self.set_kind(SyntaxKind::ArrayRepExpr);
            parser.with_next_expected_tokens(parse_expr, &[SyntaxKind::RBracket]);
        } else {
            while parser.bump_if(SyntaxKind::Comma) {
                parser.with_next_expected_tokens(
                    parse_expr,
                    &[SyntaxKind::Comma, SyntaxKind::RBracket],
                );
            }
        }

        parser.bump_or_recover(SyntaxKind::RBracket, "expected `]`", None);
    }
}
