use crate::SyntaxKind;

use super::{define_scope, path::PathScope, token_stream::TokenStream, Parser};

define_scope! {
    GenericParamListScope,
    GenericParamList,
    Override(Gt)
}
impl super::Parse for GenericParamListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Lt);
        parser.bump_trivias(true);
        if parser.bump_if(SyntaxKind::Gt) {
            return;
        }

        parser.parse(GenericParamScope::default(), None);
        parser.bump_trivias(true);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.bump_trivias(true);
            parser.parse(GenericParamScope::default(), None);
            parser.bump_trivias(true);
        }

        if !parser.bump_if(SyntaxKind::Gt) {
            parser.error_and_recover("expected closing `>`", None);
            parser.bump_if(SyntaxKind::Gt);
        }
    }
}

define_scope! {
    GenericParamScope,
    GenericParam,
    Inheritance(Comma)
}
impl super::Parse for GenericParamScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected type parameter", None);
        }

        if parser.peek_non_trivia(true) == Some(SyntaxKind::Colon) {
            parser.bump_trivias(true);
            parser.bump_expected(SyntaxKind::Colon);
            parser.bump_trivias(true);
            parser.parse(TraitBoundListScope::default(), None);
        }
    }
}

define_scope! {
    TraitBoundListScope,
    TraitBoundList,
    Inheritance(Plus)
}
impl super::Parse for TraitBoundListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.parse(TraitBoundScope::default(), None);
        while parser.peek_non_trivia(true) == Some(SyntaxKind::Plus) {
            parser.bump_trivias(true);
            parser.bump_expected(SyntaxKind::Plus);
            parser.bump_trivias(true);
            parser.parse(TraitBoundScope::default(), None);
        }
    }
}

define_scope! {
    TraitBoundScope,
    TraitBound,
    Inheritance
}
impl super::Parse for TraitBoundScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.parse(PathScope::default(), None);
    }
}
