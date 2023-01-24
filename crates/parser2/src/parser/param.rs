use crate::SyntaxKind;

use super::{
    define_scope,
    expr::parse_expr,
    expr_atom::{BlockExprScope, LitExprScope},
    path::PathScope,
    token_stream::TokenStream,
    type_::parse_type,
    Parser,
};

define_scope! {
    pub(crate) FnArgListScope,
    FnArgList,
    Override(RParen, Comma)
}
impl super::Parse for FnArgListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);
        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parser.parse(FnArgScope::default(), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(FnArgScope::default(), None);
        }

        if !parser.bump_if(SyntaxKind::RParen) {
            parser.error_and_bump_until("expected closing `)`", None, SyntaxKind::RParen);
            parser.bump_if(SyntaxKind::LParen);
        }
    }
}

define_scope! {
    FnArgScope,
    FnArg,
    Inheritance
}
impl super::Parse for FnArgScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_if(SyntaxKind::MutKw);

        parser.add_recovery_token(SyntaxKind::Colon);
        match parser.current_kind() {
            Some(SyntaxKind::SelfKw) => {
                parser.bump_expected(SyntaxKind::SelfKw);
                return;
            }
            Some(SyntaxKind::Ident | SyntaxKind::Underscore) => {
                parser.bump();
                if !parser.bump_if(SyntaxKind::Ident) {
                    parser.bump_if(SyntaxKind::Underscore);
                }
            }
            _ => {
                parser.error_and_recover("expected identifier for argument name", None);
            }
        }
        parser.remove_recovery_token(SyntaxKind::Colon);

        if !parser.bump_if(SyntaxKind::Colon) {
            parser.error_and_recover("expected `:` after argument name", None);
        }

        parse_type(parser, None);
    }
}

define_scope! {
    pub(crate) GenericParamListScope,
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
    pub(crate) GenericArgListScope,
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
        match parser.current_kind() {
            Some(SyntaxKind::LBrace) => {
                parser.parse(BlockExprScope::default(), None);
            }

            Some(SyntaxKind::Star | SyntaxKind::LBracket | SyntaxKind::LParen) => {
                parse_type(parser, None);
            }

            Some(kind) if kind.is_literal_leaf() => {
                parser.parse(LitExprScope::default(), None);
            }

            _ => {
                parser.parse(PathScope::default(), None);
            }
        }
    }
}

define_scope! { pub(crate) CallArgListScope, CallArgList, Override(RParen, Comma) }
impl super::Parse for CallArgListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);

        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parser.parse(CallArgScope::default(), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(CallArgScope::default(), None);
        }

        if !parser.bump_if(SyntaxKind::RParen) {
            parser.error_and_bump_until("expected closing `)`", None, SyntaxKind::RParen);
            parser.bump_if(SyntaxKind::RParen);
        }
    }
}

define_scope! { CallArgScope, CallArg, Inheritance }
impl super::Parse for CallArgScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);

        parser.start_dry_run();
        let has_label = parser.bump_if(SyntaxKind::Ident) && parser.bump_if(SyntaxKind::Colon);
        parser.end_dry_run();

        if has_label {
            parser.bump_expected(SyntaxKind::Ident);
            parser.bump_expected(SyntaxKind::Colon);
        }
        parse_expr(parser);
    }
}
