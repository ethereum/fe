use super::{define_scope, token_stream::TokenStream, Parser};

define_scope! {
    AttrListScope,
    AttrList,
    RecoverySet(
        Newline
    )
}

impl super::Parse for AttrListScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        todo!()
    }
}
