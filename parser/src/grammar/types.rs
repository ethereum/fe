use crate::ast::{EventDef, EventField, Field, GenericArg, StructDef, TypeAlias, TypeDesc};
use crate::grammar::expressions::parse_expr;
use crate::grammar::functions::parse_single_word_stmt;
use crate::node::{Node, Span};
use crate::{ParseFailed, ParseResult, Parser, TokenKind};
use vec1::Vec1;

/// Parse a [`ModuleStmt::StructDef`].
/// # Panics
/// Panics if the next token isn't `struct`.
pub fn parse_struct_def(par: &mut Parser) -> ParseResult<Node<StructDef>> {
    use TokenKind::*;

    let struct_tok = par.assert(Struct);
    let name = par.expect_with_notes(Name, "failed to parse struct definition", || {
        vec!["Note: a struct name must start with a letter or underscore, and contain letters, numbers, or underscores".into()]
    })?;

    let mut fields = vec![];
    par.enter_block(struct_tok.span + name.span, "struct definition")?;
    loop {
        match par.peek() {
            Some(Name) | Some(Pub) | Some(Const) => {
                let pub_qual = parse_opt_qualifier(par, TokenKind::Pub);
                let const_qual = parse_opt_qualifier(par, TokenKind::Const);
                fields.push(parse_field(par, pub_qual, const_qual)?);
            }
            Some(Dedent) => {
                par.next()?;
                break;
            }
            Some(Pass) => {
                parse_single_word_stmt(par)?;
            }
            None => break,
            Some(_) => {
                let tok = par.next()?;
                par.unexpected_token_error(tok.span, "failed to parse struct def", vec![]);
                return Err(ParseFailed);
            }
        }
    }
    let span = struct_tok.span + name.span + fields.last();
    Ok(Node::new(
        StructDef {
            name: name.into(),
            fields,
        },
        span,
    ))
}

/// Parse a type alias definition, e.g. `type MyMap = Map<u8, address>`.
/// # Panics
/// Panics if the next token isn't `type`.
pub fn parse_type_alias(par: &mut Parser) -> ParseResult<Node<TypeAlias>> {
    let type_tok = par.assert(TokenKind::Type);
    let name = par.expect(TokenKind::Name, "failed to parse type declaration")?;
    par.expect_with_notes(TokenKind::Eq, "failed to parse type declaration", || {
        vec![
            "Note: a type alias name must be followed by an equals sign and a type description"
                .into(),
            format!("Example: `type {} = Map<address, u64>`", name.text),
        ]
    })?;
    let typ = parse_type_desc(par)?;
    let span = type_tok.span + typ.span;
    Ok(Node::new(
        TypeAlias {
            name: name.into(),
            typ,
        },
        span,
    ))
}

/// Parse an event definition.
/// # Panics
/// Panics if the next token isn't `event`.
pub fn parse_event_def(par: &mut Parser) -> ParseResult<Node<EventDef>> {
    use TokenKind::*;

    let event_tok = par.assert(Event);
    let name = par.expect(Name, "failed to parse event definition")?;

    let mut fields = vec![];
    par.enter_block(event_tok.span + name.span, "event definition")?;
    loop {
        match par.peek() {
            Some(Name) | Some(Idx) => {
                fields.push(parse_event_field(par)?);
            }
            Some(Pass) => {
                parse_single_word_stmt(par)?;
            }
            Some(Dedent) => {
                par.next()?;
                break;
            }
            None => break,
            Some(_) => {
                let tok = par.next()?;
                par.unexpected_token_error(tok.span, "failed to parse event definition", vec![]);
                return Err(ParseFailed);
            }
        }
    }
    let span = event_tok.span + name.span + fields.last();
    Ok(Node::new(
        EventDef {
            name: name.into(),
            fields,
        },
        span,
    ))
}

/// Parse an event field, e.g. `foo: u8` or `idx from: address`.
pub fn parse_event_field(par: &mut Parser) -> ParseResult<Node<EventField>> {
    let idx_qual = parse_opt_qualifier(par, TokenKind::Idx);
    let name = par.expect(TokenKind::Name, "failed to parse event field")?;
    par.expect_with_notes(TokenKind::Colon, "failed to parse event field", || {
        vec![
            "Note: event field name must be followed by a colon and a type description".into(),
            format!(
                "Example: `{}{}: address`",
                if idx_qual.is_some() { "idx " } else { "" },
                name.text
            ),
        ]
    })?;

    let typ = parse_type_desc(par)?;
    par.expect_newline("event field")?;
    let span = name.span + idx_qual + &typ;
    Ok(Node::new(
        EventField {
            is_idx: idx_qual.is_some(),
            name: Node::new(name.text.into(), name.span),
            typ,
        },
        span,
    ))
}

/// Parse a field for a struct or contract. The leading optional `pub` and
/// `const` qualifiers must be parsed by the caller, and passed in.
/// Note that `event` fields are handled in [`parse_event_field`].
pub fn parse_field(
    par: &mut Parser,
    pub_qual: Option<Span>,
    const_qual: Option<Span>,
) -> ParseResult<Node<Field>> {
    let name = par.expect(TokenKind::Name, "failed to parse field definition")?;
    par.expect_with_notes(TokenKind::Colon, "failed to parse field definition", || {
        vec![
            "Note: field name must be followed by a colon and a type description".into(),
            format!(
                "Example: {}{}{}: address",
                if pub_qual.is_some() { "pub " } else { "" },
                if const_qual.is_some() { "const " } else { "" },
                name.text
            ),
        ]
    })?;

    let typ = parse_type_desc(par)?;
    let value = if par.peek() == Some(TokenKind::Eq) {
        par.next()?;
        Some(parse_expr(par)?)
    } else {
        None
    };
    par.expect_newline("field definition")?;
    let span = name.span + pub_qual + const_qual + &typ;
    Ok(Node::new(
        Field {
            is_pub: pub_qual.is_some(),
            is_const: const_qual.is_some(),
            name: Node::new(name.text.into(), name.span),
            typ,
            value,
        },
        span,
    ))
}

/// Parse an optional qualifier (`pub`, `const`, or `idx`).
pub fn parse_opt_qualifier(par: &mut Parser, tk: TokenKind) -> Option<Span> {
    if par.peek() == Some(tk) {
        let tok = par.next().unwrap();
        Some(tok.span)
    } else {
        None
    }
}

/// Parse an angle-bracket-wrapped list of generic arguments (eg. the tail end
/// of `Map<address, u256>`).
/// # Panics
/// Panics if the first token isn't `<`.
pub fn parse_generic_args(par: &mut Parser) -> ParseResult<Node<Vec<GenericArg>>> {
    use TokenKind::*;
    let mut span = par.assert(Lt).span;

    let mut args = vec![];

    let expect_end = |par: &mut Parser| {
        // If there's no comma, the next token must be `>`
        match par.peek_or_err()? {
            Gt => Ok(par.next()?.span),
            GtGt => Ok(par.split_next()?.span),
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(
                    tok.span,
                    "Unexpected token while parsing generic arg list",
                    vec![],
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
            GtGt => {
                span += par.split_next()?.span;
                break;
            }
            Int => {
                let tok = par.next()?;
                if let Ok(num) = tok.text.parse() {
                    args.push(GenericArg::Int(Node::new(num, tok.span)));
                    if par.peek() == Some(Comma) {
                        par.next()?;
                    } else {
                        span += expect_end(par)?;
                        break;
                    }
                } else {
                    par.error(tok.span, "failed to parse integer literal");
                    return Err(ParseFailed);
                }
            }
            Name | ParenOpen => {
                let typ = parse_type_desc(par)?;
                args.push(GenericArg::TypeDesc(Node::new(typ.kind, typ.span)));
                if par.peek() == Some(Comma) {
                    par.next()?;
                } else {
                    span += expect_end(par)?;
                    break;
                }
            }
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(
                    tok.span,
                    "failed to parse generic type argument list",
                    vec![],
                );
                return Err(ParseFailed);
            }
        }
    }
    Ok(Node::new(args, span))
}

/// Parse a type description, e.g. `u8` or `Map<address, u256>`.
pub fn parse_type_desc(par: &mut Parser) -> ParseResult<Node<TypeDesc>> {
    use TokenKind::*;

    let mut typ = match par.peek_or_err()? {
        Name => {
            let name = par.next()?;
            match par.peek() {
                Some(Lt) => {
                    let args = parse_generic_args(par)?;
                    let span = name.span + args.span;
                    Node::new(
                        TypeDesc::Generic {
                            base: name.into(),
                            args,
                        },
                        span,
                    )
                }
                _ => Node::new(
                    TypeDesc::Base {
                        base: name.text.into(),
                    },
                    name.span,
                ),
            }
        }
        ParenOpen => {
            let mut span = par.next()?.span;
            let mut items = vec![];
            loop {
                match par.peek_or_err()? {
                    ParenClose => {
                        span += par.next()?.span;
                        break;
                    }

                    Name | ParenOpen => {
                        let item = parse_type_desc(par)?;
                        span += item.span;
                        items.push(item);
                        if par.peek_or_err()? == Comma {
                            par.next()?;
                        } else {
                            span += par
                                .expect(
                                    ParenClose,
                                    "Unexpected token while parsing tuple type description",
                                )?
                                .span;
                            break;
                        }
                    }

                    _ => {
                        let tok = par.next()?;
                        par.unexpected_token_error(
                            tok.span,
                            "failed to parse type description",
                            vec![],
                        );
                        return Err(ParseFailed);
                    }
                }
            }
            if items.is_empty() {
                Node::new(TypeDesc::Unit, span)
            } else {
                Node::new(
                    TypeDesc::Tuple {
                        items: Vec1::try_from_vec(items).expect("couldn't convert vec to vec1"),
                    },
                    span,
                )
            }
        }
        _ => {
            let tok = par.next()?;
            par.unexpected_token_error(tok.span, "failed to parse type description", vec![]);
            return Err(ParseFailed);
        }
    };

    while par.peek() == Some(BracketOpen) {
        let ctx = "Unexpected token while parsing array type description.";
        let mut span = typ.span + par.next()?.span;
        let num = par.expect(Int, ctx)?;
        if let Ok(dimension) = num.text.parse() {
            span += par.expect(BracketClose, ctx)?.span;
            typ = Node::new(
                TypeDesc::Array {
                    typ: Box::new(typ),
                    dimension,
                },
                span,
            );
        } else {
            par.error(num.span, "failed to parse number literal");
            return Err(ParseFailed);
        }
    }
    Ok(typ)
}
