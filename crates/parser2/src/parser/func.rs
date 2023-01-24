use super::{define_scope, token_stream::TokenStream, Parser};

define_scope! {
    pub(crate) FnScope,
    Fn,
    Inheritance
}

impl super::Parse for FnScope {
    fn parse<S: TokenStream>(&mut self, _parser: &mut Parser<S>) {
        todo!()
    }
}
