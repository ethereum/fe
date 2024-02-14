use super::{define_scope, token_stream::TokenStream, Checkpoint, Parser};

use crate::SyntaxKind;

pub(super) fn parse_attr_list<S: TokenStream>(parser: &mut Parser<S>) -> Option<Checkpoint> {
    if let Some(SyntaxKind::DocComment) | Some(SyntaxKind::Pound) = parser.current_kind() {
        Some(parser.parse(AttrListScope::default(), None).1)
    } else {
        None
    }
}

define_scope! {
    pub(crate) AttrListScope,
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
            parser.bump_or_recover(
                SyntaxKind::Newline,
                "expected newline after Attribute",
                None,
            )
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
        parser.with_recovery_tokens(
            |parser| {
                parser.bump_or_recover(SyntaxKind::Ident, "expected attribute name", None);
            },
            &[SyntaxKind::LParen],
        );

        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(AttrArgListScope::default(), None);
        }
    }
}

define_scope! {
    AttrArgListScope,
    AttrArgList,
    Override(
        RParen
    )
}
impl super::Parse for AttrArgListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);
        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parser.with_next_expected_tokens(
            |parser| parser.parse(AttrArgScope::default(), None),
            &[SyntaxKind::Comma, SyntaxKind::RParen],
        );
        while parser.bump_if(SyntaxKind::Comma) {
            parser.with_next_expected_tokens(
                |parser| parser.parse(AttrArgScope::default(), None),
                &[SyntaxKind::Comma, SyntaxKind::RParen],
            );
        }

        parser.bump_or_recover(SyntaxKind::RParen, "expected `)`", None);
    }
}

define_scope! {
    AttrArgScope,
    AttrArg,
    Override(
        Comma,
        RParen
    )
}
impl super::Parse for AttrArgScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.with_next_expected_tokens(
            |parser| parser.bump_or_recover(SyntaxKind::Ident, "Expected `key: value`", None),
            &[SyntaxKind::Colon],
        );

        parser.with_next_expected_tokens(
            |parser| parser.bump_or_recover(SyntaxKind::Colon, "Expected `key: value`", None),
            &[SyntaxKind::Ident],
        );

        parser.bump_or_recover(SyntaxKind::Ident, "Expected `key: value`", None);
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
