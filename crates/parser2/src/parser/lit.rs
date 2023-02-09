use crate::SyntaxKind;

use super::{define_scope, token_stream::TokenStream, Parser};

define_scope! {
    pub(crate) LitScope,
    Lit,
    Inheritance
}
impl super::Parse for LitScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        match parser.current_kind() {
            Some(kind) if is_lit(kind) => {
                parser.bump();
            }
            _ => parser.error_and_recover("expected literal", None),
        }
    }
}

pub fn is_lit(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Int | SyntaxKind::TrueKw | SyntaxKind::FalseKw | SyntaxKind::String
    )
}
