use crate::SyntaxKind;

use super::{define_scope, expr::parse_expr, path::PathScope, token_stream::TokenStream, Parser};

define_scope! {
    GenericParamListScope,
    GenericParamList,
    Override(Gt)
}
impl super::Parse for GenericParamListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Lt);
        if parser.bump_if(SyntaxKind::Gt) {
            return;
        }

        parser.parse(GenericParamScope::default(), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(GenericParamScope::default(), None);
        }

        if !parser.bump_if(SyntaxKind::Gt) {
            parser.error_and_bump_until("expected closing `>`", None, SyntaxKind::Gt);
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

        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.bump_expected(SyntaxKind::Colon);
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
        while parser.current_kind() == Some(SyntaxKind::Plus) {
            parser.bump_expected(SyntaxKind::Plus);
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
        // TODO: Allow trait bound with associated type bound.
        // `Trait<Item = Type>`.
    }
}

define_scope! {
    GenericArgListScope,
    GenericParamList,
    Override(Gt, Comma)
}
impl super::Parse for GenericArgListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Lt);

        if parser.bump_if(SyntaxKind::Gt) {
            return;
        }

        parser.parse(GenericArgScope::default(), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(GenericArgScope::default(), None);
        }

        if !parser.bump_if(SyntaxKind::Gt) {
            parser.error_and_bump_until("expected closing `>`", None, SyntaxKind::Gt);
            parser.bump_if(SyntaxKind::Gt);
        }
    }
}

define_scope! { GenericArgScope, GenericParam, Inheritance}
impl super::Parse for GenericArgScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_expr(parser);
    }
}
