use crate::{ParseError, SyntaxKind};

use super::{
    define_scope,
    expr::parse_expr,
    param::GenericArgListScope,
    parse_list,
    path::{is_path_segment, PathScope},
    token_stream::TokenStream,
    Checkpoint, ErrProof, Parser, Recovery,
};

pub fn parse_type<S: TokenStream>(
    parser: &mut Parser<S>,
    checkpoint: Option<Checkpoint>,
) -> Result<Checkpoint, Recovery<ErrProof>> {
    match parser.current_kind() {
        Some(SyntaxKind::Star) => parser.parse_cp(PtrTypeScope::default(), checkpoint),
        Some(SyntaxKind::SelfTypeKw) => parser.parse_cp(SelfTypeScope::new(), checkpoint),
        Some(SyntaxKind::LParen) => parser.parse_cp(TupleTypeScope::default(), checkpoint),
        Some(SyntaxKind::LBracket) => parser.parse_cp(ArrayTypeScope::default(), checkpoint),
        _ => parser.parse_cp(PathTypeScope::default(), checkpoint),
    }
}

pub(crate) fn is_type_start(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::Star | SyntaxKind::SelfTypeKw | SyntaxKind::LParen | SyntaxKind::LBracket => {
            true
        }
        kind if is_path_segment(kind) => true,
        _ => false,
    }
}

define_scope!(PtrTypeScope, PtrType, Inheritance);
impl super::Parse for PtrTypeScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Star);
        parse_type(parser, None).map(|_| ())
    }
}

define_scope!(pub(crate) PathTypeScope , PathType, Inheritance);
impl super::Parse for PathTypeScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);

        parser.or_recover(|p| {
            p.parse(PathScope::default()).map_err(|_| {
                ParseError::expected(&[SyntaxKind::PathType], None, p.end_of_prev_token)
            })
        })?;

        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default())?;
        }
        Ok(())
    }
}

define_scope!(pub(super) SelfTypeScope, SelfType, Inheritance);
impl super::Parse for SelfTypeScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::SelfTypeKw);
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default())?;
        }
        Ok(())
    }
}
define_scope! {
    pub(crate) TupleTypeScope,
    TupleType,
    Override(
        RParen,
        Comma
    )
}
impl super::Parse for TupleTypeScope {
    type Error = Recovery<ErrProof>;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parse_list(
            parser,
            false,
            (SyntaxKind::LParen, SyntaxKind::RParen),
            |parser| {
                parse_type(parser, None)?;
                Ok(())
            },
        )
    }
}

define_scope! {
    ArrayTypeScope,
    ArrayType,
    Inheritance
}
impl super::Parse for ArrayTypeScope {
    type Error = Recovery<ErrProof>;

    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error> {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::LBracket);

        parser.set_scope_recovery_stack(&[SyntaxKind::SemiColon, SyntaxKind::RBracket]);

        parse_type(parser, None)?;

        if parser.find_and_pop(SyntaxKind::SemiColon, None)? {
            parser.bump();
        }

        parse_expr(parser)?;

        if parser.find_and_pop(SyntaxKind::RBracket, None)? {
            parser.bump();
        }
        Ok(())
    }
}
