pub mod ast;
pub mod grammar;
pub mod lexer;
pub use lexer::{
    Token,
    TokenKind,
};
mod parser;
pub use parser::{
    Label,
    ParseFailed,
    ParseResult,
    Parser,
};
pub mod node;

use fe_common::{
    diagnostics::Diagnostic,
    files::SourceFileId,
};

/// Parse a [`Module`] from the file content string.
/// A [`SourceFileId`] is required to associate any parsing errors with the
/// underlying file.
pub fn parse_file(
    file_content: &str,
    file_id: SourceFileId,
) -> Result<ast::Module, Vec<Diagnostic>> {
    let mut parser = Parser::new(file_content, file_id);
    match crate::grammar::module::parse_module(&mut parser) {
        Err(_) => Err(parser.diagnostics),
        Ok(node) => Ok(node.kind),
    }
}

pub fn parse_code_chunk<F, T>(mut parse_fn: F, src: &str) -> ParseResult<T>
where
    F: FnMut(&mut Parser) -> ParseResult<T>,
{
    let mut files = fe_common::files::FileStore::new();
    let id = files.add_file("parse_code_chunk test snippet".to_string(), src.to_string());
    let mut parser = Parser::new(src, id);
    if let Ok(node) = parse_fn(&mut parser) {
        Ok(node)
    } else {
        fe_common::diagnostics::print_diagnostics(&parser.diagnostics, &files);
        Err(ParseFailed)
    }
}
