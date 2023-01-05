use super::{define_scope, token_stream::TokenStream, Parser};

define_scope! {
    FnScope,
    Fn,
    Inheritance
}

impl super::Parse for FnScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        todo!()
    }
}
