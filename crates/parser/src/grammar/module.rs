use super::contracts::parse_contract_def;
use super::types::{parse_struct_def, parse_type_alias};
use crate::ast::{Import, Module, ModuleStmt, Pragma, SimpleImportName};
use crate::node::{Node, Span};
use crate::{Label, ParseFailed, ParseResult, Parser, TokenKind};

use semver::VersionReq;

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
    let span = Span::zero() + body.first() + body.last();
    Ok(Node::new(Module { body }, span))
}

/// Parse a [`ModuleStmt`].
pub fn parse_module_stmt(par: &mut Parser) -> ParseResult<ModuleStmt> {
    let stmt = match par.peek_or_err()? {
        TokenKind::Pragma => ModuleStmt::Pragma(parse_pragma(par)?),
        TokenKind::Import => ModuleStmt::Import(parse_simple_import(par)?),
        TokenKind::Contract => ModuleStmt::Contract(parse_contract_def(par)?),
        TokenKind::Struct => ModuleStmt::Struct(parse_struct_def(par)?),
        TokenKind::Type => ModuleStmt::TypeAlias(parse_type_alias(par)?),

        // Let these be parse errors for now:
        // TokenKind::Event => todo!("module-level event def"),
        // TokenKind::Name if par.peeked_text() == "from" => parse_from_import(par),
        _ => {
            let tok = par.next()?;
            par.unexpected_token_error(
                tok.span,
                "failed to parse module",
                vec!["Note: expected import, contract, struct, type, or event".into()],
            );
            return Err(ParseFailed);
        }
    };
    Ok(stmt)
}

/// Parse an `import` statement. This does not yet support paths, just module
/// names. Note that `from x import y` style imports are handled in
/// [`parse_from_import`].
/// # Panics
/// Panics if the next token isn't `import`.
pub fn parse_simple_import(par: &mut Parser) -> ParseResult<Node<Import>> {
    let import_tok = par.assert(TokenKind::Import);

    // TODO: only handles `import foo, bar as baz`

    let mut names = vec![];
    loop {
        let name =
            par.expect_with_notes(TokenKind::Name, "failed to parse import statement", |_| {
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
    Ok(Node::new(Import::Simple { names }, span))
}

/// Parse a `from x import y` style import statement.
/// # Panics
/// Always panics. Unimplemented.
pub fn parse_from_import(par: &mut Parser) -> ParseResult<Node<Import>> {
    let tok = par.assert(TokenKind::Name);
    assert_eq!(tok.text, "from");
    todo!("parse from .. import (not supported in rest of compiler yet)")
}

/// Parse a `pragma <version-requirement>` statement.
pub fn parse_pragma(par: &mut Parser) -> ParseResult<Node<Pragma>> {
    let tok = par.assert(TokenKind::Pragma);
    assert_eq!(tok.text, "pragma");

    let mut version_string = String::new();
    let mut tokens = vec![];
    loop {
        match par.peek() {
            Some(TokenKind::Newline) => break,
            None => break,
            _ => {
                let tok = par.next()?;
                version_string.push_str(tok.text);
                tokens.push(tok);
            }
        }
    }

    let version_requirement_span = match (tokens.first(), tokens.last()) {
        (Some(first), Some(last)) => first.span + last.span,
        _ => {
            par.error(
                tok.span,
                "failed to parse pragma statement: missing version requirement",
            );
            return Err(ParseFailed);
        }
    };

    match VersionReq::parse(&version_string) {
        Ok(_) => Ok(Node::new(
            Pragma {
                version_requirement: Node::new(version_string, version_requirement_span),
            },
            tok.span + version_requirement_span,
        )),
        Err(err) => {
            par.fancy_error(
                format!("failed to parse pragma statement: {}", err),
                vec![Label::primary(
                    version_requirement_span,
                    "Invalid version requirement",
                )],
                vec!["Example: `^0.5.0`".into()],
            );
            Err(ParseFailed)
        }
    }
}
