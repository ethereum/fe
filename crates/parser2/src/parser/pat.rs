use crate::{
    parser::lit::{is_lit, LitScope},
    SyntaxKind,
};

use super::{define_scope, path::PathScope, token_stream::TokenStream, Parser};

pub fn parse_pat<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    use SyntaxKind::*;
    parser.bump_trivias();
    let checkpoint = parser.checkpoint();
    parser.bump_if(SyntaxKind::MutKw);

    let success = match parser.current_kind() {
        Some(Underscore) => parser.parse(WildCardPatScope::default(), Some(checkpoint)),
        Some(Dot2) => parser.parse(RestPatScope::default(), Some(checkpoint)),
        Some(LParen) => parser.parse(TuplePatScope::default(), Some(checkpoint)),
        Some(kind) if is_lit(kind) => parser.parse(LitPatScope::default(), Some(checkpoint)),
        _ => parser.parse(PathPatScope::default(), Some(checkpoint)),
    }
    .0;

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
        parser.parse(LitScope::default(), None);
    }
}

define_scope! { TuplePatScope, TuplePat, Inheritance }
impl super::Parse for TuplePatScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.parse(TuplePatElemListScope::default(), None);
    }
}

define_scope! { TuplePatElemListScope, TuplePatElemList, Override(RParen) }
impl super::Parse for TuplePatElemListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LParen);
        if parser.bump_if(SyntaxKind::RParen) {
            return;
        }

        parser.with_next_expected_tokens(parse_pat, &[SyntaxKind::RParen, SyntaxKind::Comma]);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.with_next_expected_tokens(parse_pat, &[SyntaxKind::RParen, SyntaxKind::Comma]);
        }

        parser.bump_or_recover(SyntaxKind::RParen, "expected `)`", None);
    }
}

define_scope! { PathPatScope, PathPat, Inheritance(Pipe) }
impl super::Parse for PathPatScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.parse(PathScope::default(), None).0 {
            return;
        }

        parser.set_newline_as_trivia(false);
        if parser.current_kind() == Some(SyntaxKind::LParen) {
            self.set_kind(SyntaxKind::PathTuplePat);
            parser.parse(TuplePatElemListScope::default(), None);
        } else if parser.current_kind() == Some(SyntaxKind::LBrace) {
            self.set_kind(SyntaxKind::RecordPat);
            parser.parse(RecordPatFieldListScope::default(), None);
        }
    }
}

define_scope! { RecordPatFieldListScope, RecordPatFieldList, Override(Comma, RBrace) }
impl super::Parse for RecordPatFieldListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);
        if parser.bump_if(SyntaxKind::RBrace) {
            return;
        }

        parser.parse(RecordPatFieldScope::default(), None);
        while parser.bump_if(SyntaxKind::Comma) {
            parser.parse(RecordPatFieldScope::default(), None);
        }

        parser.bump_or_recover(SyntaxKind::RBrace, "expected `}`", None);
    }
}

define_scope! { RecordPatFieldScope, RecordPatField, Override(Comma, RBrace) }
impl super::Parse for RecordPatFieldScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        let has_label = parser.dry_run(|parser| {
            parser.bump_if(SyntaxKind::Ident) && parser.bump_if(SyntaxKind::Colon)
        });
        if has_label {
            parser.bump_expected(SyntaxKind::Ident);
            parser.bump_expected(SyntaxKind::Colon);
        }
        parser.with_next_expected_tokens(parse_pat, &[SyntaxKind::Comma, SyntaxKind::RBrace]);
    }
}

define_scope! { OrPatScope, OrPat, Inheritance(SyntaxKind::Pipe) }
impl super::Parse for OrPatScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::Pipe);
        parse_pat(parser);
    }
}
