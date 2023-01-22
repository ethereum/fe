use std::cell::RefCell;

use crate::SyntaxKind;

use super::{define_scope, path::PathScope, token_stream::TokenStream, Parser, RecoveryMethod};

pub fn parse_pat<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    use SyntaxKind::*;
    let (success, checkpoint) = match parser.current_kind() {
        Some(Underscore) => parser.parse(WildCardPatScope::default(), None),
        Some(Dot2) => parser.parse(RestPatScope::default(), None),
        Some(LParen) => parser.parse(TuplePatScope::default(), None),
        Some(Int | String) => parser.parse(LitPatScope::default(), None),
        _ => parser.parse(PathPatScope::default(), None),
    };

    if parser.current_kind() == Some(SyntaxKind::Pipe) {
        parser.parse(OrPatScope::default(), Some(checkpoint)).0 && success
    } else {
        success
    }
}

define_scope! { WildCardPatScope, WildCardPat, Inheritance(SyntaxKind::Pipe) }
impl super::Parse for WildCardPatScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Underscore);
    }
}

define_scope! { RestPatScope, RestPat, Inheritance }
impl super::Parse for RestPatScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Dot2);
    }
}

define_scope! { LitPatScope, LitPat, Inheritance(SyntaxKind::Pipe) }
impl super::Parse for LitPatScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        match parser.current_kind() {
            Some(SyntaxKind::Int | SyntaxKind::String) => parser.bump(),
            _ => unreachable!(),
        }
    }
}

define_scope! { TuplePatScope, TuplePat, Override(RParen) }
impl super::Parse for TuplePatScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);
        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parse_pat(parser);
        while parser.bump_if(SyntaxKind::Comma) {
            parse_pat(parser);
        }

        if !parser.bump_if(SyntaxKind::RParen) {
            parser.error_and_recover("expected `)`", None);
            parser.bump_if(SyntaxKind::RParen);
        }
    }
}

// We can't use `define_scope` here since the `syntax_kind` of the scope can be
// determined after parsing.
#[derive(Debug, Clone)]
struct PathPatScope {
    syntax_kind: RefCell<SyntaxKind>,
    recovery_method: RecoveryMethod,
}
impl Default for PathPatScope {
    fn default() -> Self {
        Self {
            syntax_kind: SyntaxKind::PathPat.into(),
            recovery_method: RecoveryMethod::inheritance(&[SyntaxKind::Pipe]),
        }
    }
}
impl super::ParsingScope for PathPatScope {
    /// Returns the recovery method of the current scope.
    fn recovery_method(&self) -> &RecoveryMethod {
        &self.recovery_method
    }

    fn syntax_kind(&self) -> SyntaxKind {
        *self.syntax_kind.borrow()
    }
}
impl super::Parse for PathPatScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.parse(PathScope::default(), None).0 {
            return;
        }

        parser.set_newline_as_trivia(false);
        if parser.current_kind() == Some(SyntaxKind::LParen) {
            parser.parse(TuplePatScope::default(), None);
            *self.syntax_kind.borrow_mut() = SyntaxKind::PathTuplePat;
        }
    }
}

define_scope! { OrPatScope, OrPat, Inheritance(SyntaxKind::Pipe) }
impl super::Parse for OrPatScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Pipe);
        parse_pat(parser);
    }
}
