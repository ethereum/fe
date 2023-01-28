use crate::SyntaxKind;

use super::{define_scope, token_stream::TokenStream, Parser};

define_scope! {
    pub(crate) UseTreeScope,
    UseTree,
    Inheritance
}
impl super::Parse for UseTreeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        match parser.current_kind() {
            Some(SyntaxKind::LBrace) => {
                parser.parse(UseTreeListScope::default(), None);
                return;
            }
            Some(SyntaxKind::Star) => {
                parser.bump();
                return;
            }
            _ => {}
        }

        parser.parse(UsePathScope::default(), None);

        if !parser.bump_if(SyntaxKind::Colon2) {
            if parser.current_kind() == Some(SyntaxKind::AsKw) {
                parser.parse(UseTreeRenameScope::default(), None);
            }
            return;
        }

        match parser.current_kind() {
            Some(SyntaxKind::LBrace) => {
                parser.parse(UseTreeListScope::default(), None);
            }
            Some(SyntaxKind::Star) => {
                parser.bump();
            }
            _ => {
                parser.error_and_recover("expected identifier or `self`", None);
            }
        };
    }
}

define_scope! {
    UseTreeListScope,
    UseTreeList,
    Override(Comma, RBrace)
}
impl super::Parse for UseTreeListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBrace);
        parser.with_next_expected_tokens(
            |parser| {
                parser.parse(UseTreeScope::default(), None);
            },
            &[SyntaxKind::RBrace, SyntaxKind::Comma],
        );

        while parser.bump_if(SyntaxKind::Comma) {
            parser.with_next_expected_tokens(
                |parser| {
                    parser.parse(UseTreeScope::default(), None);
                },
                &[SyntaxKind::RBrace, SyntaxKind::Comma],
            );
        }

        parser.bump_or_recover(SyntaxKind::RBrace, "expected `}`", None);
    }
}

define_scope! {
    UsePathScope,
    UsePath,
    Inheritance(Colon2)
}
impl super::Parse for UsePathScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.parse(UsePathSegmentScope::default(), None);

        loop {
            let is_path_segment = parser.dry_run(|parser| {
                parser.bump_if(SyntaxKind::Colon2)
                    && parser.parse(UsePathSegmentScope::default(), None).0
            });
            if is_path_segment {
                parser.bump_expected(SyntaxKind::Colon2);
                parser.parse(UsePathSegmentScope::default(), None);
            } else {
                break;
            }
        }
    }
}

define_scope! {
    UsePathSegmentScope,
    UsePathSegment,
    Inheritance
}
impl super::Parse for UsePathSegmentScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        match parser.current_kind() {
            Some(kind) if is_use_path_segment(kind) => {
                parser.bump();
            }
            _ => {
                parser.error_and_recover("expected identifier or `self`", None);
            }
        }
    }
}

define_scope! {
    UseTreeRenameScope,
    UseTreeRename,
    Inheritance
}
impl super::Parse for UseTreeRenameScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::AsKw);

        match parser.current_kind() {
            Some(SyntaxKind::Ident) => parser.bump_expected(SyntaxKind::Ident),
            Some(SyntaxKind::Underscore) => parser.bump_expected(SyntaxKind::Underscore),
            _ => parser.error_and_recover("expected identifier or `_`", None),
        };
    }
}

fn is_use_path_segment(kind: SyntaxKind) -> bool {
    use SyntaxKind::*;
    matches!(kind, Ident | SelfKw)
}
