use std::cell::RefCell;

use crate::{parser::path, SyntaxKind};

use super::{
    attr::parse_attr_list, define_scope, parse_pat, stmt::parse_stmt, token_stream::TokenStream,
    Parser,
};

pub(super) fn parse_expr<S: TokenStream>(_parser: &mut Parser<S>) -> bool {
    todo!()
}
