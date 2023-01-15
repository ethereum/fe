use crate::SyntaxKind;

use super::{define_scope, token_stream::TokenStream, Parser};

define_scope! {
    StructScope,
    Struct,
    Inheritance
}
impl super::Parse for StructScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Struct);

        parser.bump_trivias(true);
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected ident for the struct name", None)
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
    RecoverySet(
        RBrace
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
    RecoverySet(
        Newline
    )
}
impl super::Parse for StructFieldDefScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if matches!(
            parser.current_kind(),
            Some(SyntaxKind::Pound | SyntaxKind::DocComment)
        ) {
            parser.parse(super::attr::AttrListScope::default(), None);
        }
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
        parser.parse(super::path::PathScope::default(), None);
        if !matches!(
            parser.peek_non_trivia(false),
            Some(SyntaxKind::Newline) | Some(SyntaxKind::RBrace)
        ) {
            parser.error_and_recover("expected newline after the field definition", None);
        }
    }
}
