use crate::SyntaxKind;

use super::{define_scope, token_stream::TokenStream, Parser};

define_scope! {
    PathScope,
    Path,
    Inheritance
}
impl super::Parse for PathScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.parse(PathSegmentScope::default(), None);
        while parser.current_kind() == Some(SyntaxKind::Colon2) {
            parser.bump_expected(SyntaxKind::Colon2);
            parser.parse(PathSegmentScope::default(), None);
        }
    }
}

define_scope! {
    PathSegmentScope,
    PathSegment,
    Inheritance
}
impl super::Parse for PathSegmentScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        dbg! {"{:?}", parser.current_kind()};
        debug_assert!(is_path_header(parser.current_kind().unwrap()));
        parser.bump()
    }
}

pub(super) fn is_path_header(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::SelfType | SyntaxKind::SelfKw | SyntaxKind::Ident
    )
}
