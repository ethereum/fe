use crate::SyntaxKind;

use super::{
    define_scope,
    expr_atom::BlockExprScope,
    param::{parse_generic_params_opt, parse_where_clause_opt, FuncParamListScope},
    token_stream::TokenStream,
    type_::parse_type,
    Parser,
};

define_scope! {
    pub(crate) FuncScope {
        fn_def_scope: FuncDefScope
    },
    Func,
    Inheritance
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum FuncDefScope {
    Normal,
    TraitDef,
    Extern,
}
impl Default for FuncDefScope {
    fn default() -> Self {
        Self::Normal
    }
}

impl super::Parse for FuncScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::FnKw);

        match self.fn_def_scope {
            FuncDefScope::Normal => parse_normal_fn_def_impl(parser),
            FuncDefScope::TraitDef => parse_trait_fn_def_impl(parser),
            FuncDefScope::Extern => parse_extern_fn_def_impl(parser),
        }
    }
}

fn parse_normal_fn_def_impl<S: TokenStream>(parser: &mut Parser<S>) {
    parser.with_next_expected_tokens(
        |parser| {
            parser.bump_or_recover(
                SyntaxKind::Ident,
                "expected ident for the function name",
                None,
            )
        },
        &[SyntaxKind::Lt, SyntaxKind::LParen, SyntaxKind::LBrace],
    );

    parser.with_next_expected_tokens(
        |parser| parse_generic_params_opt(parser, false),
        &[SyntaxKind::LParen, SyntaxKind::LBrace],
    );

    parser.with_next_expected_tokens(
        |parser| {
            if parser.current_kind() == Some(SyntaxKind::LParen) {
                parser.parse(FuncParamListScope::default(), None);
            } else {
                parser.error_and_recover("expected `(` for the function arguments", None);
            }
        },
        &[SyntaxKind::LBrace, SyntaxKind::Arrow, SyntaxKind::WhereKw],
    );

    parser.with_next_expected_tokens(
        |parser| {
            if parser.bump_if(SyntaxKind::Arrow) {
                parse_type(parser, None);
            }
        },
        &[SyntaxKind::LBrace, SyntaxKind::WhereKw],
    );
    parser.with_next_expected_tokens(parse_where_clause_opt, &[SyntaxKind::LBrace]);

    if parser.current_kind() == Some(SyntaxKind::LBrace) {
        parser.parse(BlockExprScope::default(), None);
    } else {
        parser.error_and_recover("function body is required", None)
    }
}

fn parse_trait_fn_def_impl<S: TokenStream>(parser: &mut Parser<S>) {
    parser.with_next_expected_tokens(
        |parser| {
            parser.bump_or_recover(
                SyntaxKind::Ident,
                "expected ident for the function name",
                None,
            )
        },
        &[SyntaxKind::Lt, SyntaxKind::LParen],
    );

    parser.with_next_expected_tokens(
        |parser| parse_generic_params_opt(parser, false),
        &[SyntaxKind::LParen],
    );

    parser.with_recovery_tokens(
        |parser| {
            if parser.current_kind() == Some(SyntaxKind::LParen) {
                parser.parse(FuncParamListScope::default(), None);
            } else {
                parser.error_and_recover("expected `(` for the function arguments", None);
            }
        },
        &[SyntaxKind::LBrace, SyntaxKind::Arrow, SyntaxKind::WhereKw],
    );

    parser.with_recovery_tokens(
        |parser| {
            if parser.bump_if(SyntaxKind::Arrow) {
                parse_type(parser, None);
            }
        },
        &[SyntaxKind::LBrace, SyntaxKind::WhereKw],
    );
    parser.with_recovery_tokens(parse_where_clause_opt, &[SyntaxKind::LBrace]);

    if parser.current_kind() == Some(SyntaxKind::LBrace) {
        parser.parse(BlockExprScope::default(), None);
    }
}

fn parse_extern_fn_def_impl<S: TokenStream>(parser: &mut Parser<S>) {
    parser.with_next_expected_tokens(
        |parser| {
            parser.bump_or_recover(
                SyntaxKind::Ident,
                "expected identifier for the function name",
                None,
            )
        },
        &[SyntaxKind::LParen],
    );

    parser.with_recovery_tokens(
        |parser| {
            if parser.current_kind() == Some(SyntaxKind::LParen) {
                parser.parse(FuncParamListScope::default(), None);
            } else {
                parser.error_and_recover("expected `(` for the function arguments", None);
            }
        },
        &[SyntaxKind::Arrow],
    );

    if parser.bump_if(SyntaxKind::Arrow) {
        parse_type(parser, None);
    }
}
