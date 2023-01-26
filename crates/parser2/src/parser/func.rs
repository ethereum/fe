use crate::SyntaxKind;

use super::{
    define_scope,
    expr_atom::BlockExprScope,
    param::{parse_where_clause_opt, FnArgListScope, GenericParamListScope},
    token_stream::TokenStream,
    type_::parse_type,
    Parser,
};

define_scope! {
    pub(crate) FnScope {
        fn_def_scope: FnDefScope
    },
    Fn,
    Inheritance
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum FnDefScope {
    Normal,
    TraitDef,
    Extern,
}
impl Default for FnDefScope {
    fn default() -> Self {
        Self::Normal
    }
}

impl super::Parse for FnScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::FnKw);

        match self.fn_def_scope {
            FnDefScope::Normal => parse_normal_fn_def_impl(parser),
            FnDefScope::TraitDef => parse_trait_fn_def_impl(parser),
            FnDefScope::Extern => parse_extern_fn_def_impl(parser),
        }
    }
}

fn parse_normal_fn_def_impl<S: TokenStream>(parser: &mut Parser<S>) {
    parser.with_next_expected_tokens(&[SyntaxKind::Lt, SyntaxKind::LParen], |parser| {
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected ident for the function name", None)
        }
    });

    parser.with_next_expected_tokens(&[SyntaxKind::LParen], |parser| {
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericParamListScope::default(), None);
        }
    });

    parser.with_next_expected_tokens(
        &[SyntaxKind::LBrace, SyntaxKind::Arrow, SyntaxKind::WhereKw],
        |parser| {
            if parser.current_kind() == Some(SyntaxKind::LParen) {
                parser.parse(FnArgListScope::default(), None);
            } else {
                parser.error_and_recover("expected `(` for the function arguments", None);
            }
        },
    );

    parser.with_next_expected_tokens(&[SyntaxKind::LBrace, SyntaxKind::WhereKw], |parser| {
        if parser.bump_if(SyntaxKind::Arrow) {
            parse_type(parser, None, false);
        }
    });
    parser.with_next_expected_tokens(&[SyntaxKind::LBrace], parse_where_clause_opt);

    if parser.current_kind() == Some(SyntaxKind::LBrace) {
        parser.parse(BlockExprScope::default(), None);
    } else {
        parser.error_and_recover("function body is required", None)
    }
}

fn parse_trait_fn_def_impl<S: TokenStream>(parser: &mut Parser<S>) {
    parser.with_next_expected_tokens(&[SyntaxKind::Lt, SyntaxKind::LParen], |parser| {
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected ident for the function name", None)
        }
    });

    parser.with_next_expected_tokens(&[SyntaxKind::LParen], |parser| {
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericParamListScope::default(), None);
        }
    });

    parser.with_recovery_tokens(
        &[SyntaxKind::LBrace, SyntaxKind::Arrow, SyntaxKind::WhereKw],
        |parser| {
            if parser.current_kind() == Some(SyntaxKind::LParen) {
                parser.parse(FnArgListScope::default(), None);
            } else {
                parser.error_and_recover("expected `(` for the function arguments", None);
            }
        },
    );

    parser.with_recovery_tokens(&[SyntaxKind::LBrace, SyntaxKind::WhereKw], |parser| {
        if parser.bump_if(SyntaxKind::Arrow) {
            parse_type(parser, None, false);
        }
    });
    parser.with_recovery_tokens(&[SyntaxKind::LBrace], parse_where_clause_opt);

    if parser.current_kind() == Some(SyntaxKind::LBrace) {
        parser.parse(BlockExprScope::default(), None);
    }
}

fn parse_extern_fn_def_impl<S: TokenStream>(parser: &mut Parser<S>) {
    parser.with_next_expected_tokens(&[SyntaxKind::LParen], |parser| {
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected ident for the function name", None)
        }
    });

    parser.with_recovery_tokens(&[SyntaxKind::Arrow], |parser| {
        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(FnArgListScope::default(), None);
        } else {
            parser.error_and_recover("expected `(` for the function arguments", None);
        }
    });

    if parser.bump_if(SyntaxKind::Arrow) {
        parse_type(parser, None, false);
    }
}
