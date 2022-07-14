use crate::ast::{GenericParameter, TypeDesc};
use crate::node::Node;
use crate::{ParseFailed, ParseResult, Parser, TokenKind};

/// Parse a single generic parameter (eg. `T:SomeTrait` in `fn foo<T: SomeTrait>(some_arg: u256) -> bool`).
/// # Panics
/// Panics if the first token isn't `Name`.
pub fn parse_generic_param(par: &mut Parser) -> ParseResult<GenericParameter> {
    use TokenKind::*;

    let name = par.assert(Name);
    match par.optional(Colon) {
        Some(_) => {
            let bound = par.expect(TokenKind::Name, "failed to parse generic bound")?;
            Ok(GenericParameter::Bounded {
                name: Node::new(name.text.into(), name.span),
                bound: Node::new(
                    TypeDesc::Base {
                        base: bound.text.into(),
                    },
                    bound.span,
                ),
            })
        }
        None => Ok(GenericParameter::Unbounded(Node::new(
            name.text.into(),
            name.span,
        ))),
    }
}

/// Parse an angle-bracket-wrapped list of generic arguments (eg. `<T, R: SomeTrait>` in `fn foo<T, R: SomeTrait>(some_arg: u256) -> bool`).
/// # Panics
/// Panics if the first token isn't `<`.
pub fn parse_generic_params(par: &mut Parser) -> ParseResult<Node<Vec<GenericParameter>>> {
    use TokenKind::*;
    let mut span = par.assert(Lt).span;

    let mut args = vec![];

    let expect_end = |par: &mut Parser| {
        // If there's no comma, the next token must be `>`
        match par.peek_or_err()? {
            Gt => Ok(par.next()?.span),
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(
                    &tok,
                    "Unexpected token while parsing generic arg list",
                    vec!["Expected a `>` here".to_string()],
                );
                Err(ParseFailed)
            }
        }
    };

    loop {
        match par.peek_or_err()? {
            Gt => {
                span += par.next()?.span;
                break;
            }
            Name => {
                let typ = parse_generic_param(par)?;
                args.push(typ);
                if par.peek() == Some(Comma) {
                    par.next()?;
                } else {
                    span += expect_end(par)?;
                    break;
                }
            }

            // Invalid generic argument.
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(
                    &tok,
                    "failed to parse list of generic parameters",
                    vec!["Expected a generic parameter name such as `T` here".to_string()],
                );
                return Err(ParseFailed);
            }
        }
    }
    Ok(Node::new(args, span))
}
