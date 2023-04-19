use crate::SyntaxKind;

use super::{
    attr::parse_attr_list,
    define_scope,
    param::{parse_generic_params_opt, parse_where_clause_opt},
    token_stream::TokenStream,
    type_::parse_type,
    Parser,
};

define_scope! {
    pub(crate) StructScope,
    Struct,
    Inheritance
}
impl super::Parse for StructScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>, _idx: usize) {
        parser.bump_expected(SyntaxKind::StructKw);

        parser.with_next_expected_tokens(
            |parser| {
                if !parser.bump_if(SyntaxKind::Ident) {
                    parser.error_and_recover("expected ident for the struct name", None)
                }
            },
            &[SyntaxKind::Lt, SyntaxKind::LBrace, SyntaxKind::WhereKw],
        );

        parser.with_next_expected_tokens(
            |parser| parse_generic_params_opt(parser),
            &[SyntaxKind::LBrace, SyntaxKind::WhereKw],
        );

        parser.with_next_expected_tokens(parse_where_clause_opt, &[SyntaxKind::LBrace]);

        if parser.current_kind() == Some(SyntaxKind::LBrace) {
            parser.parse(RecordFieldDefListScope::default(), None);
        } else {
            parser.error_and_recover("expected struct field definition", None);
        }
    }
}

define_scope! {
    pub(crate) RecordFieldDefListScope,
    RecordFieldDefList,
    Override(
        RBrace,
        Newline
    )
}
impl super::Parse for RecordFieldDefListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>, _idx: usize) {
        parser.bump_expected(SyntaxKind::LBrace);

        loop {
            parser.set_newline_as_trivia(true);
            if parser.current_kind() == Some(SyntaxKind::RBrace) || parser.current_kind().is_none()
            {
                break;
            }
            parser.parse(RecordFieldDefScope::default(), None);
            parser.set_newline_as_trivia(false);
            if !parser.bump_if(SyntaxKind::Newline)
                && parser.current_kind() != Some(SyntaxKind::RBrace)
            {
                parser.error_and_recover("expected newline after field definition", None);
            }
        }

        if !parser.bump_if(SyntaxKind::RBrace) {
            parser.error_and_recover(
                "expected the closing brace of the struct field definition",
                None,
            );
            parser.bump_if(SyntaxKind::RBrace);
        }
    }
}

define_scope! {
    RecordFieldDefScope,
    RecordFieldDef,
    Inheritance
}
impl super::Parse for RecordFieldDefScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>, _idx: usize) {
        parser.set_newline_as_trivia(false);
        parse_attr_list(parser);

        parser.bump_if(SyntaxKind::PubKw);
        parser.with_next_expected_tokens(
            |parser| {
                if !parser.bump_if(SyntaxKind::Ident) {
                    parser.error_and_recover("expected ident for the field name", None);
                }
            },
            &[SyntaxKind::Colon],
        );
        if parser.bump_if(SyntaxKind::Colon) {
            parser.with_next_expected_tokens(
                |parser| parse_type(parser, None),
                &[SyntaxKind::Newline, SyntaxKind::RBrace],
            );
        } else {
            parser.error_and_recover("expected `name: type` for the field definition", None);
        }
    }
}
