use rowan::Checkpoint;

use crate::{parser::path, SyntaxKind};

use super::{
    attr::parse_attr_list,
    define_scope,
    expr::{parse_expr, parse_expr_no_struct},
    parse_pat,
    stmt::parse_stmt,
    token_stream::TokenStream,
    Parser,
};

pub(super) fn parse_expr_atom<S: TokenStream>(
    parser: &mut Parser<S>,
    allow_struct_init: bool,
) -> (bool, Checkpoint) {
    use SyntaxKind::*;
    match parser.current_kind() {
        Some(Int | String | TrueKw | FalseKw) => parser.parse(LitExprScope::default(), None),
        Some(IfKw) => parser.parse(IfExprScope::default(), None),
        Some(MatchKw) => parser.parse(MatchExprScope::default(), None),
        Some(LBrace) => parser.parse(BlockExprScope::default(), None),
        Some(LParen) => parser.parse(ParenScope::default(), None),
        Some(LBracket) => parser.parse(ArrayScope::default(), None),
        Some(kind) if path::is_path_segment(kind) => {
            let (success, checkpoint) = parser.parse(path::PathScope::default(), None);
            if success && parser.current_kind() == Some(LBrace) && allow_struct_init {
                let (success, _) = parser.parse(RecordInitExprScope::default(), Some(checkpoint));
                (success, checkpoint)
            } else {
                (success, checkpoint)
            }
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
        AssertKw,
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
            let checkpoint = parse_attr_list(parser);
            if !parse_stmt(parser, checkpoint) {
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

        parser.with_recovery_tokens(&[SyntaxKind::LBrace], parse_expr_no_struct);

        if parser.current_kind() != Some(SyntaxKind::LBrace) {
            parser.error_and_recover("expected `{`", None);
            return;
        }
        parser.parse(BlockExprScope::default(), None);

        if parser.current_kind() == Some(SyntaxKind::ElseKw) {
            parser.bump_expected(SyntaxKind::ElseKw);

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

        parser.with_recovery_tokens(&[SyntaxKind::LBrace], parse_expr_no_struct);

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
            if !parser.bump_if(SyntaxKind::Newline)
                && parser.current_kind() != Some(SyntaxKind::RBrace)
            {
                parser.error_and_recover("expected newline after match arm", None);
            }
        }

        if !parser.bump_if(SyntaxKind::RBrace) {
            parser.error_and_bump_until("expected }", None, SyntaxKind::RBrace)
        }
    }
}

define_scope! { MatchArmScope, MatchArm, Inheritance }
impl super::Parse for MatchArmScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);

        parser.with_recovery_tokens(&[SyntaxKind::FatArrow], parse_pat);

        if !parser.bump_if(SyntaxKind::FatArrow) {
            parser.error_and_recover("expected `=>`", None);
            return;
        }

        parse_expr(parser);
    }
}

define_scope! { pub(crate) LitExprScope, LitExpr, Inheritance }
impl super::Parse for LitExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        match parser.current_kind() {
            Some(
                SyntaxKind::Int | SyntaxKind::String | SyntaxKind::TrueKw | SyntaxKind::FalseKw,
            ) => parser.bump(),
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

define_scope! { RecordFieldListScope, RecordFieldList, Override(RBrace, Comma) }
impl super::Parse for RecordFieldListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);

        if parser.bump_if(SyntaxKind::LBrace) {
            return;
        }

        parser.parse(RecordFieldScope::default(), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(RecordFieldScope::default(), None);
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

        if !parser.bump_if(SyntaxKind::Colon) {
            parser.error_and_recover("expected `:`", None);
        }

        parse_expr(parser);
    }
}

define_scope! { ParenScope, ParenExpr, Override(RParen, Comma) }
impl super::Parse for ParenScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);

        if parser.bump_if(SyntaxKind::RParen) {
            self.set_kind(SyntaxKind::TupleExpr);
            return;
        }

        parse_expr(parser);
        while parser.bump_if(SyntaxKind::Comma) {
            self.set_kind(SyntaxKind::TupleExpr);
            if parser.current_kind() == Some(SyntaxKind::RParen) {
                break;
            }
            parse_expr(parser);
        }

        if !parser.bump_if(SyntaxKind::RParen) {
            parser.error_and_bump_until("expected `)`", None, SyntaxKind::RParen);
        }
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

        parse_expr(parser);

        if parser.bump_if(SyntaxKind::SemiColon) {
            self.set_kind(SyntaxKind::ArrayRepExpr);
            parse_expr(parser);
        } else {
            while parser.bump_if(SyntaxKind::Comma) {
                parse_expr(parser);
            }
        }

        if !parser.bump_if(SyntaxKind::RBracket) {
            parser.error_and_bump_until("expected `]`", None, SyntaxKind::RBracket);
        }
    }
}
