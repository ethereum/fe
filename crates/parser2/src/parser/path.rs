use crate::SyntaxKind;

use super::{define_scope, token_stream::TokenStream, Parser};

define_scope! {
    PathScope,
    Path,
    Inheritance
}
impl super::Parse for PathScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.parse(PathSegmentScope::default(), None);
        while parser.peek_non_trivia(false) == Some(SyntaxKind::Colon2) {
            parser.bump_trivias(false);
            parser.bump_expected(SyntaxKind::Colon2);
            parser.parse(PathSegmentScope::default(), None);
        }
    }
}

define_scope! {
    PathSegmentScope,
    Path,
    Inheritance
}
impl super::Parse for PathSegmentScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_bump("expected path segment", 1);
        }
    }
}
