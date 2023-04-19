use std::{cell::Cell, rc::Rc};

use crate::SyntaxKind;

use super::{define_scope, token_stream::TokenStream, Parser};

define_scope! {
    pub(crate) UseTreeScope,
    UseTree,
    Inheritance
}
impl super::Parse for UseTreeScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>, _idx: usize) {
        parser.set_newline_as_trivia(false);
        if let Some(SyntaxKind::LBrace) = parser.current_kind() {
            parser.parse(UseTreeListScope::default(), None);
            return;
        }

        let use_path_scope = UsePathScope::default();
        parser.parse(use_path_scope.clone(), None);
        let has_wildcard = use_path_scope.has_wildcard.get();

        if parser.current_kind() == Some(SyntaxKind::AsKw) {
            if has_wildcard {
                parser.error_and_recover("cant use `as` with wildcard", None);
            }
            if parser.current_kind() == Some(SyntaxKind::AsKw) {
                parser.parse(UseTreeRenameScope::default(), None);
            }
            return;
        }

        if !parser.bump_if(SyntaxKind::Colon2) {
            return;
        }
        match parser.current_kind() {
            Some(SyntaxKind::LBrace) if !has_wildcard => {
                if has_wildcard {
                    parser.error_and_recover("can't use `*` with `{}`", None);
                } else {
                    parser.parse(UseTreeListScope::default(), None);
                }
            }
            _ => {
                parser.error_and_recover("expected identifier, `*` or `self`", None);
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
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>, _idx: usize) {
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
    UsePathScope{ has_wildcard: Rc<Cell<bool>>},
    UsePath,
    Inheritance(Colon2)
}
impl super::Parse for UsePathScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>, _idx: usize) {
        parser.set_newline_as_trivia(false);
        parser.parse(UsePathSegmentScope::default(), None);

        loop {
            let is_path_segment = parser.dry_run(|parser| {
                parser.bump_if(SyntaxKind::Colon2)
                    && parser.parse(UsePathSegmentScope::default(), None).0
            });
            if is_path_segment {
                parser.bump_expected(SyntaxKind::Colon2);
                self.has_wildcard
                    .set(parser.current_kind() == Some(SyntaxKind::Star));
                parser.parse(UsePathSegmentScope::default(), None);
                if self.has_wildcard.get() {
                    break;
                }
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
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>, _idx: usize) {
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
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>, _idx: usize) {
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
    matches!(kind, Ident | SelfKw | Star)
}
