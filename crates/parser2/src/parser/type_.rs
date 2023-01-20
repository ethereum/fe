use crate::SyntaxKind;

use super::{
    define_scope, param::GenericArgListScope, path::PathScope, token_stream::TokenStream,
    Checkpoint, Parser,
};

pub(super) fn parse_type<S: TokenStream>(
    parser: &mut Parser<S>,
    checkpoint: Option<Checkpoint>,
) -> bool {
    match parser.current_kind() {
        Some(SyntaxKind::Star) => parser.parse(PtrTypeScope::default(), checkpoint),
        Some(SyntaxKind::SelfTypeKw) => parser.parse(SelfTypeScope::default(), checkpoint),
        Some(SyntaxKind::LParen) => parser.parse(TupleTypeScope::default(), checkpoint),
        _ => parser.parse(PathTypeScope::default(), checkpoint),
    }
}

define_scope!(PtrTypeScope, PtrType, Inheritance);
impl super::Parse for PtrTypeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Star);
        parser.bump_trivias(false);
        parse_type(parser, None);
    }
}

define_scope!(PathTypeScope, PathType, Inheritance);
impl super::Parse for PathTypeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.parse(PathScope::default(), None) {
            return;
        }

        if parser.peek_non_trivia(false) == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default(), None);
        }
    }
}

define_scope!(SelfTypeScope, SelfType, Inheritance);
impl super::Parse for SelfTypeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::SelfTypeKw);
    }
}
define_scope! {
    TupleTypeScope,
    TupleType,
    Override(
        RParen,
        Comma
    )
}
impl super::Parse for TupleTypeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);
        parser.bump_trivias(true);
        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parse_type(parser, None);
        parser.bump_trivias(true);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.bump_trivias(true);
            parse_type(parser, None);
            parser.bump_trivias(true);
        }

        if !parser.bump_if(SyntaxKind::RParen) {
            parser.error_and_recover("expected `)`", None);
            parser.bump_if(SyntaxKind::RParen);
        }
    }
}
