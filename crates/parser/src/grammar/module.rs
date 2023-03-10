use super::expressions::parse_expr;
use super::functions::parse_fn_def;
use super::types::{
    parse_impl_def, parse_path_tail, parse_struct_def, parse_trait_def, parse_type_alias,
    parse_type_desc,
};
use super::{contracts::parse_contract_def, types::parse_enum_def};
use crate::ast::{ConstantDecl, Module, ModuleStmt, Pragma, Use, UseTree};
use crate::node::{Node, Span};
use crate::{Label, ParseFailed, ParseResult, Parser, TokenKind};

use semver::VersionReq;

/// Parse a [`Module`].
pub fn parse_module(par: &mut Parser) -> Node<Module> {
    let mut body = vec![];
    loop {
        match par.peek() {
            Some(TokenKind::Newline) => {
                par.next().unwrap();
            }
            None => break,
            Some(_) => {
                match parse_module_stmt(par) {
                    Ok(stmt) => body.push(stmt),
                    Err(_) => {
                        // TODO: capture a real span here
                        body.push(ModuleStmt::ParseError(Span::zero(par.file_id)));
                        break;
                    }
                };
            }
        }
    }
    let span = Span::zero(par.file_id) + body.first() + body.last();
    Node::new(Module { body }, span)
}

/// Parse a [`ModuleStmt`].
pub fn parse_module_stmt(par: &mut Parser) -> ParseResult<ModuleStmt> {
    let stmt = match par.peek_or_err()? {
        TokenKind::Pragma => ModuleStmt::Pragma(parse_pragma(par)?),
        TokenKind::Use => ModuleStmt::Use(parse_use(par)?),
        TokenKind::Contract => ModuleStmt::Contract(parse_contract_def(par, None)?),
        TokenKind::Struct => ModuleStmt::Struct(parse_struct_def(par, None)?),
        TokenKind::Enum => ModuleStmt::Enum(parse_enum_def(par, None)?),
        TokenKind::Trait => ModuleStmt::Trait(parse_trait_def(par, None)?),
        TokenKind::Impl => ModuleStmt::Impl(parse_impl_def(par)?),
        TokenKind::Type => ModuleStmt::TypeAlias(parse_type_alias(par, None)?),
        TokenKind::Const => ModuleStmt::Constant(parse_constant(par, None)?),
        TokenKind::Pub => {
            let pub_span = par.next()?.span;
            match par.peek_or_err()? {
                TokenKind::Fn | TokenKind::Unsafe => {
                    ModuleStmt::Function(parse_fn_def(par, Some(pub_span))?)
                }
                TokenKind::Struct => ModuleStmt::Struct(parse_struct_def(par, Some(pub_span))?),
                TokenKind::Enum => ModuleStmt::Enum(parse_enum_def(par, Some(pub_span))?),
                TokenKind::Trait => ModuleStmt::Trait(parse_trait_def(par, Some(pub_span))?),
                TokenKind::Type => ModuleStmt::TypeAlias(parse_type_alias(par, Some(pub_span))?),
                TokenKind::Const => ModuleStmt::Constant(parse_constant(par, Some(pub_span))?),
                TokenKind::Contract => {
                    ModuleStmt::Contract(parse_contract_def(par, Some(pub_span))?)
                }
                _ => {
                    let tok = par.next()?;
                    par.unexpected_token_error(
                        &tok,
                        "failed to parse module",
                        vec!["Note: expected `fn`".into()],
                    );
                    return Err(ParseFailed);
                }
            }
        }
        TokenKind::Fn | TokenKind::Unsafe => ModuleStmt::Function(parse_fn_def(par, None)?),
        TokenKind::Hash => {
            let attr = par.expect(TokenKind::Hash, "expected `#`")?;
            let attr_name = par.expect_with_notes(TokenKind::Name, "failed to parse attribute definition", |_|
                vec!["Note: an attribute name must start with a letter or underscore, and contain letters, numbers, or underscores".into()])?;
            ModuleStmt::Attribute(Node::new(attr_name.text.into(), attr.span + attr_name.span))
        }
        _ => {
            let tok = par.next()?;
            par.unexpected_token_error(
                &tok,
                "failed to parse module",
                vec!["Note: expected import, contract, struct, type or const".into()],
            );
            return Err(ParseFailed);
        }
    };
    Ok(stmt)
}

/// Parse a constant, e.g. `const MAGIC_NUMBER: u256 = 4711`.
/// # Panics
/// Panics if the next token isn't `const`.
pub fn parse_constant(par: &mut Parser, pub_qual: Option<Span>) -> ParseResult<Node<ConstantDecl>> {
    let const_tok = par.assert(TokenKind::Const);
    let name = par.expect(TokenKind::Name, "failed to parse constant declaration")?;
    par.expect_with_notes(
        TokenKind::Colon,
        "failed to parse constant declaration",
        |_| {
            vec![
                "Note: constant name must be followed by a colon and a type description".into(),
                format!("Example: let `{}: u256 = 1000`", name.text),
            ]
        },
    )?;
    let typ = parse_type_desc(par)?;
    par.expect_with_notes(
        TokenKind::Eq,
        "failed to parse constant declaration",
        |_| {
            vec![
            "Note: the type of a constant must be followed by an equals sign and a value assignment"
                .into(),
                format!(
                    "Example: let `{}: u256 = 1000`",
                    name.text
                ),
        ]
        },
    )?;

    let exp = parse_expr(par)?;

    let span = const_tok.span + exp.span;
    Ok(Node::new(
        ConstantDecl {
            name: name.into(),
            typ,
            value: exp,
            pub_qual,
        },
        span,
    ))
}

/// Parse a `use` statement.
/// # Panics
/// Panics if the next token isn't `use`.
pub fn parse_use(par: &mut Parser) -> ParseResult<Node<Use>> {
    let use_tok = par.assert(TokenKind::Use);

    let tree = parse_use_tree(par)?;
    let tree_span = tree.span;

    Ok(Node::new(Use { tree }, use_tok.span + tree_span))
}

/// Parse a `use` tree.
pub fn parse_use_tree(par: &mut Parser) -> ParseResult<Node<UseTree>> {
    let (path, path_span, trailing_delim) = {
        let path_head =
            par.expect_with_notes(TokenKind::Name, "failed to parse `use` statement", |_| {
                vec![
                    "Note: `use` paths must start with a name".into(),
                    "Example: `use foo::bar`".into(),
                ]
            })?;
        parse_path_tail(par, path_head.into())
    };

    if trailing_delim.is_some() {
        match par.peek() {
            Some(TokenKind::BraceOpen) => {
                par.next()?;

                let mut children = vec![];
                let close_brace_span;

                loop {
                    children.push(parse_use_tree(par)?);
                    let tok = par.next()?;
                    match tok.kind {
                        TokenKind::Comma => {
                            continue;
                        }
                        TokenKind::BraceClose => {
                            close_brace_span = tok.span;
                            break;
                        }
                        _ => {
                            par.unexpected_token_error(
                                &tok,
                                "failed to parse `use` tree",
                                vec!["Note: expected a `,` or `}` token".to_string()],
                            );
                            return Err(ParseFailed);
                        }
                    }
                }

                Ok(Node::new(
                    UseTree::Nested {
                        prefix: path,
                        children,
                    },
                    close_brace_span,
                ))
            }
            Some(TokenKind::Star) => {
                par.next()?;
                Ok(Node::new(UseTree::Glob { prefix: path }, path_span))
            }
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(
                    &tok,
                    "failed to parse `use` tree",
                    vec!["Note: expected a `*`, `{` or name token".to_string()],
                );
                Err(ParseFailed)
            }
        }
    } else if par.peek() == Some(TokenKind::As) {
        par.next()?;

        let rename_tok = par.expect(TokenKind::Name, "failed to parse `use` tree")?;
        let span = path_span + rename_tok.span;
        let rename = Some(rename_tok.into());

        Ok(Node::new(UseTree::Simple { path, rename }, span))
    } else {
        Ok(Node::new(UseTree::Simple { path, rename: None }, path_span))
    }
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
                version_requirement: Node::new(version_string.into(), version_requirement_span),
            },
            tok.span + version_requirement_span,
        )),
        Err(err) => {
            par.fancy_error(
                format!("failed to parse pragma statement: {err}"),
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
