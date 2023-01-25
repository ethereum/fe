use crate::SyntaxKind;

use super::{
    define_scope,
    expr_atom::BlockExprScope,
    param::{FnArgListScope, GenericParamListScope},
    token_stream::TokenStream,
    type_::parse_type,
    Parser,
};

define_scope! {
    pub(crate) FnScope {
        disallow_def: bool
    },
    Fn,
    Inheritance
}
impl FnScope {
    pub(crate) fn disallow_def() -> Self {
        Self {
            disallow_def: true,
            ..Self::default()
        }
    }
}
impl super::Parse for FnScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::FnKw);

        parser.with_recovery_tokens(&[SyntaxKind::Lt, SyntaxKind::LParen], |parser| {
            if !parser.bump_if(SyntaxKind::Ident) {
                parser.error_and_recover("expected ident for the function name", None)
            }
        });

        parser.with_recovery_tokens(&[SyntaxKind::LParen], |parser| {
            if parser.current_kind() == Some(SyntaxKind::Lt) {
                parser.parse(GenericParamListScope::default(), None);
            }
        });

        parser.with_recovery_tokens(&[SyntaxKind::LBrace, SyntaxKind::Arrow], |parser| {
            if parser.current_kind() == Some(SyntaxKind::LParen) {
                parser.parse(FnArgListScope::default(), None);
            } else {
                parser.error_and_recover("expected `(` for the function arguments", None);
            }
        });

        parser.with_recovery_tokens(&[SyntaxKind::LBrace], |parser| {
            if parser.bump_if(SyntaxKind::Arrow) {
                parse_type(parser, None, false);
            }
        });

        if parser.current_kind() == Some(SyntaxKind::LBrace) {
            if self.disallow_def {
                parser.error_and_recover("function definition is not allowed", None);
            }
            parser.parse(BlockExprScope::default(), None);
        }
    }
}
