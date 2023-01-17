use super::{define_scope, path::PathScope, token_stream::TokenStream, Parser};

use crate::SyntaxKind;

define_scope! {
    TupleDefScope,
    TupleDef,
    RecoverySet(
        RParen,
        Comma
    )
}
impl super::Parse for TupleDefScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);
        parser.bump_trivias(true);
        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parser.parse(PathScope::default(), None);
        parser.bump_trivias(true);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.bump_trivias(true);
            parser.parse(PathScope::default(), None);
            parser.bump_trivias(true);
        }

        parser.bump_trivias(true);
        if !parser.bump_if(SyntaxKind::RParen) {
            parser.error_and_recover("expected `)`", None);
            parser.bump_if(SyntaxKind::RParen);
        }
    }
}
