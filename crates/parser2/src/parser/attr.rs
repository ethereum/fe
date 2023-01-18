use super::{define_scope, token_stream::TokenStream, Parser};

use crate::SyntaxKind;

define_scope! {
    AttrListScope,
    AttrList,
    Override(
        Newline
    )
}
impl super::Parse for AttrListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        use SyntaxKind::*;

        loop {
            parser.bump_trivias(true);
            match parser.current_kind() {
                Some(Pound) => parser.parse(AttrScope::default(), None),
                Some(DocComment) => parser.parse(DocCommentAttrScope::default(), None),
                _ => break,
            }
        }
    }
}

define_scope! {
    AttrScope,
    Attr,
    Inheritance
}
impl super::Parse for AttrScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Pound);
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected attribute name", None);
            return;
        }

        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(AttrParamListScope::default(), None);
        }
    }
}

define_scope! {
    AttrParamListScope,
    AttrParamList,
    Override(
        RParen
    )
}
impl super::Parse for AttrParamListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);
        parser.bump_trivias(true);
        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parser.parse(AttrParam::default(), None);
        parser.bump_trivias(true);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.bump_trivias(true);
            parser.parse(AttrParam::default(), None);
        }

        parser.bump_trivias(true);
        if !parser.bump_if(SyntaxKind::RParen) {
            parser.error_and_recover("expected `)`", None);
        }
    }
}

define_scope! {
    AttrParam,
    AttrParam,
    Override(
        Comma,
        RParen
    )
}
impl super::Parse for AttrParam {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected `key: value`", None);
        }

        parser.bump_trivias(true);
        if !parser.bump_if(SyntaxKind::Colon) {
            parser.error_and_recover("expected `key: value`", None);
        }

        parser.bump_trivias(true);
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected `ident`", None)
        }

        match parser.peek_non_trivia(true) {
            Some(SyntaxKind::Comma) | Some(SyntaxKind::RParen) | None => {}

            _ => parser.error_and_recover("unexpected token", None),
        }
    }
}

define_scope! {
    DocCommentAttrScope,
    DocCommentAttr,
    Inheritance
}
impl super::Parse for DocCommentAttrScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::DocComment);
        parser.bump_if(SyntaxKind::Newline);
    }
}
