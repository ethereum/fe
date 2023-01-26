use crate::SyntaxKind;

use super::{
    define_scope,
    expr::parse_expr,
    expr_atom::{BlockExprScope, LitExprScope},
    path::{is_path_segment, PathScope},
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
            parser.error_and_recover("expected closing `)`", None);
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

        let is_self = parser.with_recovery_tokens(&[SyntaxKind::Colon], |parser| {
            match parser.current_kind() {
                Some(SyntaxKind::SelfKw) => {
                    parser.bump_expected(SyntaxKind::SelfKw);
                    true
                }
                Some(SyntaxKind::Ident | SyntaxKind::Underscore) => {
                    parser.bump();
                    if !parser.bump_if(SyntaxKind::Ident) {
                        parser.bump_if(SyntaxKind::Underscore);
                    }
                    false
                }
                _ => {
                    parser.error_and_recover("expected identifier for argument name", None);
                    false
                }
            }
        });
        if is_self {
            return;
        }

        if !parser.bump_if(SyntaxKind::Colon) {
            parser.error_and_recover("expected `:` after argument name", None);
        }

        parse_type(parser, None, false);
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
            parser.error_and_recover("expected closing `>`", None);
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
        parser.bump_if(SyntaxKind::ConstKw);

        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected type parameter", None);
        }

        if parser.current_kind() == Some(SyntaxKind::Colon) {
            parser.parse(TypeBoundListScope::default(), None);
        }
    }
}

define_scope! {
    TypeBoundListScope,
    TypeBoundList,
    Inheritance(Plus)
}
impl super::Parse for TypeBoundListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Colon);

        parser.parse(TypeBoundScope::default(), None);
        while parser.current_kind() == Some(SyntaxKind::Plus) {
            parser.bump_expected(SyntaxKind::Plus);
            parser.parse(TypeBoundScope::default(), None);
        }
    }
}

define_scope! {
    TypeBoundScope,
    TypeBound,
    Inheritance
}
impl super::Parse for TypeBoundScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.parse(PathScope::default(), None);
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::new(false), None);
        }
    }
}

define_scope! {
    pub(crate) GenericArgListScope{ allow_bounds: bool },
    GenericArgList,
    Override(Gt, Comma)
}
impl super::Parse for GenericArgListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Lt);

        if parser.bump_if(SyntaxKind::Gt) {
            return;
        }

        parser.parse(GenericArgScope::new(self.allow_bounds), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(GenericArgScope::new(self.allow_bounds), None);
        }

        if !parser.bump_if(SyntaxKind::Gt) {
            parser.error_and_recover("expected closing `>`", None);
            parser.bump_if(SyntaxKind::Gt);
        }
    }
}

define_scope! {
    GenericArgScope{ allow_bounds: bool },
    GenericArg,
    Inheritance
}
impl super::Parse for GenericArgScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        match parser.current_kind() {
            Some(SyntaxKind::LBrace) => {
                parser.parse(BlockExprScope::default(), None);
            }

            Some(kind) if kind.is_literal_leaf() => {
                parser.parse(LitExprScope::default(), None);
            }

            _ => {
                parse_type(parser, None, self.allow_bounds);
                if parser.current_kind() == Some(SyntaxKind::Colon) {
                    if !self.allow_bounds {
                        parser.error_and_recover("type bounds are not allowed here", None);
                    } else {
                        parser.parse(TypeBoundListScope::default(), None);
                    }
                }
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
            parser.error_and_recover("expected closing `)`", None);
            parser.bump_if(SyntaxKind::RParen);
        }
    }
}

define_scope! { CallArgScope, CallArg, Inheritance }
impl super::Parse for CallArgScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);

        let has_label = parser.dry_run(|parser| {
            parser.bump_if(SyntaxKind::Ident) && parser.bump_if(SyntaxKind::Colon)
        });

        if has_label {
            parser.bump_expected(SyntaxKind::Ident);
            parser.bump_expected(SyntaxKind::Colon);
        }
        parse_expr(parser);
    }
}

define_scope! { WhereClauseScope, WhereClause, Inheritance(Newline) }
impl super::Parse for WhereClauseScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::WhereKw);

        loop {
            parser.set_newline_as_trivia(true);
            match parser.current_kind() {
                Some(kind) if is_path_segment(kind) => {
                    parse_type(parser, None, false);
                    if parser.current_kind() == Some(SyntaxKind::Colon) {
                        parser.parse(TypeBoundListScope::default(), None);
                        parser.set_newline_as_trivia(false);
                        if !parser.bump_if(SyntaxKind::Newline) {
                            parser.error_and_recover("expected newline after type bounds", None);
                        }
                    } else {
                        parser.error_and_recover("expected `:` for type bounds", None);
                    }
                }
                _ => break,
            }
        }
    }
}

pub(crate) fn parse_where_clause_opt<S: TokenStream>(parser: &mut Parser<S>) {
    let newline_as_trivia = parser.set_newline_as_trivia(true);
    if parser.current_kind() == Some(SyntaxKind::WhereKw) {
        parser.parse(WhereClauseScope::default(), None);
    }
    parser.set_newline_as_trivia(newline_as_trivia);
}
