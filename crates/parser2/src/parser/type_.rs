use crate::SyntaxKind;

use super::{
    define_scope,
    expr::parse_expr,
    param::GenericArgListScope,
    path::{is_path_segment, PathScope},
    token_stream::TokenStream,
    Checkpoint, Parser,
};

pub fn parse_type<S: TokenStream>(parser: &mut Parser<S>, checkpoint: Option<Checkpoint>) -> bool {
    match parser.current_kind() {
        Some(SyntaxKind::Star) => parser.parse(PtrTypeScope::default(), checkpoint),
        Some(SyntaxKind::SelfTypeKw) => parser.parse(SelfTypeScope::new(), checkpoint),
        Some(SyntaxKind::LParen) => parser.parse(TupleTypeScope::default(), checkpoint),
        Some(SyntaxKind::LBracket) => parser.parse(ArrayTypeScope::default(), checkpoint),
        _ => parser.parse(PathTypeScope::default(), checkpoint),
    }
    .0
}

pub(crate) fn is_type_start(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::Star | SyntaxKind::SelfTypeKw | SyntaxKind::LParen | SyntaxKind::LBracket => {
            true
        }
        kind if is_path_segment(kind) => true,
        _ => false,
    }
}

define_scope!(PtrTypeScope, PtrType, Inheritance);
impl super::Parse for PtrTypeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Star);
        parse_type(parser, None);
    }
}

define_scope!(pub(crate) PathTypeScope , PathType, Inheritance);
impl super::Parse for PathTypeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        if !parser.parse(PathScope::default(), None).0 {
            return;
        }

        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default(), None);
        }
    }
}

define_scope!(SelfTypeScope, SelfType, Inheritance);
impl super::Parse for SelfTypeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::SelfTypeKw);
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default(), None);
        }
    }
}
define_scope! {
    pub(crate) TupleTypeScope,
    TupleType,
    Override(
        RParen,
        Comma
    )
}
impl super::Parse for TupleTypeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::LParen);
        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }
        parser.set_newline_as_trivia(true);

        parse_type(parser, None);
        while parser.bump_if(SyntaxKind::Comma) {
            parse_type(parser, None);
        }

        if !parser.bump_if(SyntaxKind::RParen) {
            parser.error_and_recover("expected `)`", None);
            parser.bump_if(SyntaxKind::RParen);
        }
    }
}

define_scope! {
    ArrayTypeScope,
    ArrayType,
    Override(RBracket)
}
impl super::Parse for ArrayTypeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::LBracket);

        parser
            .with_next_expected_tokens(|parser| parse_type(parser, None), &[SyntaxKind::SemiColon]);

        if !parser.bump_if(SyntaxKind::SemiColon) {
            parser.error_and_recover("expected `;`", None);
            parser.bump_if(SyntaxKind::LBracket);
            return;
        }

        parse_expr(parser);

        if !parser.bump_if(SyntaxKind::RBracket) {
            parser.error_and_recover("expected closing `]`", None);
            parser.bump_if(SyntaxKind::RBracket);
        }
    }
}
