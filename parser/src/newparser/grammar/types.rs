use crate::ast::{
    ConstQualifier,
    ContractStmt,
    EventField,
    Field,
    GenericArg,
    IdxQualifier,
    ModuleStmt,
    PubQualifier,
    TypeDesc,
};
use crate::lexer::{
    Token,
    TokenKind,
};
use crate::newparser::{
    grammar::expressions::parse_expr,
    Label,
    ParseResult,
    Parser,
};
use crate::node::{
    Node,
    Span,
};

pub fn parse_struct_def<'a>(par: &mut Parser<'a>) -> ParseResult<Node<ModuleStmt>> {
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
                let pub_qual = parse_opt_qualifier(par, TokenKind::Pub, PubQualifier {});
                let const_qual = parse_opt_qualifier(par, TokenKind::Const, ConstQualifier {});
                fields.push(parse_field_def(par, pub_qual, const_qual)?);
            }
            Some(Dedent) => {
                par.next()?;
                break;
            }
            None => break,
            Some(_) => {
                let tok = par.next()?;
                par.unexpected_token_error(tok.span, "failed to parse struct def", vec![]);
                return Err(());
            }
        }
    }
    let span = struct_tok.span + name.span + fields.last();
    Ok(Node::new(
        ModuleStmt::StructDef {
            name: name.into(),
            fields,
        },
        span,
    ))
}

pub fn parse_type_def<'a>(par: &mut Parser<'a>) -> ParseResult<Node<ModuleStmt>> {
    let type_tok = par.assert(TokenKind::Type);
    let name = par.expect(TokenKind::Name, "failed to parse type declaration")?;
    par.expect_with_notes(TokenKind::Eq, "failed to parse type declaration", || {
        vec![
            "Note: a type alias name must be followed by an equals sign and a type description"
                .into(),
            format!("Example: `type {} = map<address, u64>`", name.text),
        ]
    })?;
    let typ = parse_type_desc(par)?;
    let span = type_tok.span + typ.span;
    Ok(Node::new(
        ModuleStmt::TypeDef {
            name: name.into(),
            typ,
        },
        span,
    ))
}

/// Parse an event definition block
pub fn parse_event_def<'a>(par: &mut Parser<'a>) -> ParseResult<Node<ContractStmt>> {
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
            Some(Dedent) => {
                par.next()?;
                break;
            }
            None => break,
            Some(_) => {
                let tok = par.next()?;
                par.unexpected_token_error(tok.span, "failed to parse event definition", vec![]);
                return Err(());
            }
        }
    }
    let span = event_tok.span + name.span + fields.last();
    Ok(Node::new(
        ContractStmt::EventDef {
            name: name.into(),
            fields,
        },
        span,
    ))
}

pub fn parse_event_field<'a>(par: &mut Parser<'a>) -> ParseResult<Node<EventField>> {
    let idx_qual = parse_opt_qualifier(par, TokenKind::Idx, IdxQualifier {});
    let name = par.assert(TokenKind::Name);
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
    let span = name.span + idx_qual.as_ref() + &typ;
    Ok(Node::new(
        EventField {
            idx_qual,
            name: Node::new(name.text.into(), name.span),
            typ,
        },
        span,
    ))
}

pub fn parse_field_def<'a>(
    par: &mut Parser<'a>,
    pub_qual: Option<Node<PubQualifier>>,
    const_qual: Option<Node<ConstQualifier>>,
) -> ParseResult<Node<Field>> {
    let name = par.assert(TokenKind::Name);
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
    let span = name.span + pub_qual.as_ref() + const_qual.as_ref() + &typ;
    Ok(Node::new(
        Field {
            pub_qual,
            const_qual,
            name: Node::new(name.text.into(), name.span),
            typ,
            value,
        },
        span,
    ))
}

/// Parse an optional qualifier (pub, const, or idx)
pub fn parse_opt_qualifier<'a, T>(
    par: &mut Parser<'a>,
    tk: TokenKind,
    node_kind: T,
) -> Option<Node<T>> {
    if par.peek() == Some(tk) {
        let tok = par.next().unwrap();
        Some(Node::new(node_kind, tok.span))
    } else {
        None
    }
}

/// Parse an angle-bracket-wrapped list of generic arguments (eg. the tail end
/// of `map<address, u256>`)
fn parse_generic_args<'a>(par: &mut Parser<'a>) -> ParseResult<(Vec<Node<GenericArg>>, Span)> {
    use TokenKind::*;
    let mut span = par.assert(Lt).span;

    let mut args = vec![];

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
                    args.push(Node::new(GenericArg::Int(num), tok.span));
                } else {
                    par.error(tok.span, "failed to parse integer literal");
                    return Err(());
                }
            }
            Name | ParenOpen => {
                let typ = parse_type_desc(par)?;
                args.push(Node::new(GenericArg::TypeDesc(typ.kind), typ.span));
                if par.peek() == Some(Comma) {
                    par.next()?;
                } else {
                    // If there's no comma, the next token must be `>`
                    match par.peek_or_err()? {
                        Gt => {
                            span += par.next()?.span;
                            break;
                        }
                        GtGt => {
                            span += par.split_next()?.span;
                            break;
                        }
                        _ => {
                            let tok = par.next()?;
                            par.unexpected_token_error(
                                tok.span,
                                "Unexpected token while parsing generic arg list",
                                vec![],
                            );
                            return Err(());
                        }
                    }
                }
            }
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(
                    tok.span,
                    "failed to parse generic type argument list",
                    vec![],
                );
                return Err(());
            }
        }
    }
    Ok((args, span))
}

// TODO: remove TypeDesc::Map and this fn
fn temporary_backward_compatible_map_type(
    name: &Token,
    mut args: Vec<Node<GenericArg>>,
    span: Span,
) -> Node<TypeDesc> {
    let to_node = args.pop().unwrap();
    let from_node = args.pop().unwrap();
    if let (GenericArg::TypeDesc(from), GenericArg::TypeDesc(to)) = (from_node.kind, to_node.kind) {
        Node::new(
            TypeDesc::Map {
                from: Box::new(Node::new(from, from_node.span)),
                to: Box::new(Node::new(to, to_node.span)),
            },
            span,
        )
    } else {
        panic!()
    }
}

/// Parse a type description
pub fn parse_type_desc<'a>(par: &mut Parser<'a>) -> ParseResult<Node<TypeDesc>> {
    use TokenKind::*;

    let mut typ = match par.peek_or_err()? {
        Name => {
            let name = par.next()?;
            match par.peek() {
                Some(Lt) => {
                    let (args, argspan) = parse_generic_args(par)?;
                    let span = name.span + argspan;
                    if name.text == "map" {
                        temporary_backward_compatible_map_type(&name, args, span)
                    } else {
                        Node::new(
                            TypeDesc::Generic {
                                base: name.into(),
                                args,
                            },
                            span,
                        )
                    }
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
                            par.expect(
                                ParenClose,
                                "Unexpected token while parsing tuple type description",
                            )?;
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
                        return Err(());
                    }
                }
            }
            Node::new(TypeDesc::Tuple { items }, span)
        }
        _ => {
            let tok = par.next()?;
            par.unexpected_token_error(tok.span, "failed to parse type description", vec![]);
            return Err(());
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
            return Err(());
        }
    }
    Ok(typ)
}
