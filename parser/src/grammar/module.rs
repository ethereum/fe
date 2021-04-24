use super::contracts::parse_contract_def;
use super::types::{parse_struct_def, parse_type_def};
use crate::ast::{Module, ModuleStmt, SimpleImportName};
use crate::node::{Node, Span};
use crate::{ParseFailed, ParseResult, Parser, TokenKind};

/// Parse a [`Module`].
pub fn parse_module(par: &mut Parser) -> ParseResult<Node<Module>> {
    let mut body = vec![];
    loop {
        match par.peek() {
            Some(TokenKind::Newline) => par.expect_newline("module")?,
            Some(TokenKind::Dedent) => {
                par.next()?;
                break;
            }
            None => break,
            Some(_) => {
                let stmt = parse_module_stmt(par)?;
                body.push(stmt);
            }
        }
    }
    let span = if body.is_empty() {
        Span::new(0, 0)
    } else {
        body.first().unwrap().span + body.last()
    };

    Ok(Node::new(Module { body }, span))
}

/// Parse a [`ModuleStmt`].
pub fn parse_module_stmt(par: &mut Parser) -> ParseResult<Node<ModuleStmt>> {
    match par.peek().unwrap() {
        TokenKind::Import => parse_simple_import(par),
        TokenKind::Name if par.peeked_text() == "from" => parse_from_import(par),
        TokenKind::Contract => parse_contract_def(par),
        TokenKind::Struct => parse_struct_def(par),
        TokenKind::Type => parse_type_def(par),
        TokenKind::Event => todo!("module-level event def"),
        _ => {
            let tok = par.next().unwrap();
            par.unexpected_token_error(
                tok.span,
                "failed to parse module",
                vec!["Note: expected import, contract, struct, type, or event".into()],
            );
            par.error(
                tok.span,
                format!("Unexpected token when parsing module: `{:?}`", tok.kind),
            );
            Err(ParseFailed)
        }
    }
}

/// Parse an `import` statement. This does not yet support paths, just module
/// names. Note that `from x import y` style imports are handled in
/// [`parse_from_import`].
/// # Panics
/// Panics if the next token isn't `import`.
pub fn parse_simple_import(par: &mut Parser) -> ParseResult<Node<ModuleStmt>> {
    let import_tok = par.assert(TokenKind::Import);

    // TODO: only handles `import foo, bar as baz`

    let mut names = vec![];
    loop {
        let name =
            par.expect_with_notes(TokenKind::Name, "failed to parse import statement", || {
                vec![
                    "Note: `import` must be followed by a module name or path".into(),
                    "Example: `import mymodule".into(),
                ]
            })?;

        let alias = if par.peek() == Some(TokenKind::As) {
            par.next()?;
            let tok = par.expect(TokenKind::Name, "failed to parse import statement")?;
            Some(tok.into())
        } else {
            None
        };

        let span = name.span + alias.as_ref();
        names.push(Node::new(
            SimpleImportName {
                path: vec![Node::new(name.text.to_string(), name.span)],
                alias,
            },
            span,
        ));

        match par.peek() {
            Some(TokenKind::Comma) => {
                par.next()?;
                continue;
            }
            Some(TokenKind::Newline) | None => break,
            Some(_) => {
                let tok = par.next()?;
                par.unexpected_token_error(tok.span, "failed to parse `import` statement", vec![]);
                return Err(ParseFailed);
            }
        }
    }

    let span = import_tok.span + names.last();
    Ok(Node::new(ModuleStmt::SimpleImport { names }, span))
}

/// Parse a `from x import y` style import statement.
/// # Panics
/// Always panics. Unimplemented.
pub fn parse_from_import(par: &mut Parser) -> ParseResult<Node<ModuleStmt>> {
    let tok = par.assert(TokenKind::Name);
    assert_eq!(tok.text, "from");
    todo!("parse from .. import (not supported in rest of compiler yet)")
}
