pub mod ast;
pub mod grammar;
pub mod lexer;
pub use lexer::{Token, TokenKind};
mod parser;
pub use parser::{Label, ParseFailed, ParseResult, Parser};
pub mod node;

use ast::Module;
use fe_common::diagnostics::Diagnostic;
use fe_common::files::SourceFileId;

/// Parse a [`Module`] from the file content string.
///
/// Returns a `Module` (which may be incomplete), and a vec of [`Diagnostic`]s
/// (which may be empty) to display to the user. If any of the returned
/// diagnostics are errors, the compilation of this file should ultimately fail.
///
/// If a fatal parse error occurred, the last element of the `Module::body` will
/// be a `ModuleStmt::ParseError`. The parser currently has very limited ability
/// to recover from syntax errors; this is just a first meager attempt at returning a
/// useful AST when there are syntax errors.
///
/// A [`SourceFileId`] is required to associate any diagnostics with the
/// underlying file.
pub fn parse_file(file_id: SourceFileId, src: &str) -> (Module, Vec<Diagnostic>) {
    let mut parser = Parser::new(file_id, src);
    let node = crate::grammar::module::parse_module(&mut parser);
    (node.kind, parser.diagnostics)
}
