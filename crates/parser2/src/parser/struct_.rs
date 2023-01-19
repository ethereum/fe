use crate::SyntaxKind;

use super::{
    attr::parse_attr_list, define_scope, param::GenericParamListScope, token_stream::TokenStream,
    type_::parse_type, Parser,
};

define_scope! {
    StructScope,
    Struct,
    Inheritance
}
impl super::Parse for StructScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::StructKw);

        parser.bump_trivias(true);
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected ident for the struct name", None)
        }

        parser.bump_trivias(true);
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericParamListScope::default(), None);
        }

        parser.bump_trivias(true);
        if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(StructFieldDefListScope::default(), None);
        } else {
            parser.error_and_recover("expected the struct field definition", None);
        }
    }
}

define_scope! {
    StructFieldDefListScope,
    StructFieldDefList,
    Override(
        RBrace,
        Newline
    )
}
impl super::Parse for StructFieldDefListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);
        parser.bump_trivias(true);
        while !matches!(parser.current_kind(), Some(SyntaxKind::RBrace) | None) {
            parser.parse(StructFieldDefScope::default(), None);
            parser.bump_trivias(true);
        }

        if !parser.bump_if(SyntaxKind::RBrace) {
            parser.error_and_recover(
                "expected the closing brace of the struct field definition",
                None,
            );
        }
    }
}

define_scope! {
    StructFieldDefScope,
    StructFieldDef,
    Inheritance
}
impl super::Parse for StructFieldDefScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parse_attr_list(parser);
        parser.bump_trivias(true);

        parser.bump_if(SyntaxKind::PubKw);
        parser.bump_trivias(false);
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected ident for the field name", None);
        }
        parser.bump_trivias(false);
        if !parser.bump_if(SyntaxKind::Colon) {
            parser.error_and_recover("expected `name: type` for the field definition", None);
        }
        parser.bump_trivias(false);
        parse_type(parser, None);
        if !matches!(
            parser.peek_non_trivia(false),
            Some(SyntaxKind::Newline) | Some(SyntaxKind::RBrace)
        ) {
            parser.error_and_recover("expected newline after the field definition", None);
        }
    }
}
