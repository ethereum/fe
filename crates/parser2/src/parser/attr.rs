use super::{define_scope, token_stream::TokenStream, Checkpoint, Parser};

use crate::SyntaxKind;

pub(super) fn parse_attr_list<S: TokenStream>(parser: &mut Parser<S>) -> Option<Checkpoint> {
    if let Some(SyntaxKind::DocComment) | Some(SyntaxKind::Pound) = parser.current_kind() {
        Some(parser.parse(super::attr::AttrListScope::default(), None).1)
    } else {
        None
    }
}

define_scope! {
    AttrListScope,
    AttrList,
    Override(
        Newline
    )
}
impl super::Parse for AttrListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        loop {
            parser.set_newline_as_trivia(true);
            match parser.current_kind() {
                Some(SyntaxKind::Pound) => parser.parse(AttrScope::default(), None),
                Some(SyntaxKind::DocComment) => parser.parse(DocCommentAttrScope::default(), None),
                _ => break,
            };
            parser.set_newline_as_trivia(false);
            if !parser.bump_if(SyntaxKind::Newline) {
                parser.error_and_recover("expected newline after Attribute", None)
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
        parser.set_newline_as_trivia(false);
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
        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parser.parse(AttrParam::default(), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(AttrParam::default(), None);
        }

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

        if !parser.bump_if(SyntaxKind::Colon) {
            parser.error_and_recover("expected `key: value`", None);
        }

        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected `ident`", None)
        }

        match parser.current_kind() {
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
