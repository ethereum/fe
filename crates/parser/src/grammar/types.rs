use crate::ast::{
    self, Enum, Field, GenericArg, Impl, Path, Trait, TypeAlias, TypeDesc, Variant, VariantKind,
};
use crate::grammar::expressions::parse_expr;
use crate::grammar::functions::{parse_fn_def, parse_fn_sig};
use crate::node::{Node, Span};
use crate::Token;
use crate::{ParseFailed, ParseResult, Parser, TokenKind};
use fe_common::diagnostics::Label;
use if_chain::if_chain;
use smol_str::SmolStr;
use vec1::Vec1;

/// Parse a [`ModuleStmt::Struct`].
/// # Panics
/// Panics if the next token isn't `struct`.
pub fn parse_struct_def(
    par: &mut Parser,
    pub_qual: Option<Span>,
) -> ParseResult<Node<ast::Struct>> {
    let struct_tok = par.assert(TokenKind::Struct);
    let name = par.expect_with_notes(TokenKind::Name, "failed to parse struct definition", |_| {
        vec!["Note: a struct name must start with a letter or underscore, and contain letters, numbers, or underscores".into()]
    })?;

    let mut span = struct_tok.span + name.span;
    let mut fields = vec![];
    let mut functions = vec![];
    par.enter_block(span, "struct body must start with `{`")?;

    loop {
        par.eat_newlines();

        let attributes = if let Some(attr) = par.optional(TokenKind::Hash) {
            let attr_name = par.expect_with_notes(TokenKind::Name, "failed to parse attribute definition", |_|
                vec!["Note: an attribute name must start with a letter or underscore, and contain letters, numbers, or underscores".into()])?;
            // This hints to a future where we would support multiple attributes per field. For now we don't need it.
            vec![Node::new(attr_name.text.into(), attr.span + attr_name.span)]
        } else {
            vec![]
        };

        par.eat_newlines();

        let pub_qual = par.optional(TokenKind::Pub).map(|tok| tok.span);
        match par.peek_or_err()? {
            TokenKind::Name => {
                let field = parse_field(par, attributes, pub_qual, None)?;
                if !functions.is_empty() {
                    par.error(
                        field.span,
                        "struct field definitions must come before any function definitions",
                    );
                }
                fields.push(field);
            }
            TokenKind::Fn | TokenKind::Unsafe => {
                functions.push(parse_fn_def(par, pub_qual)?);
            }
            TokenKind::BraceClose if pub_qual.is_none() => {
                span += par.next()?.span;
                break;
            }
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(&tok, "failed to parse struct definition", vec![]);
                return Err(ParseFailed);
            }
        }
    }
    Ok(Node::new(
        ast::Struct {
            name: name.into(),
            fields,
            functions,
            pub_qual,
        },
        span,
    ))
}

/// Parse a [`ModuleStmt::Enum`].
/// # Panics
/// Panics if the next token isn't [`TokenKind::Enum`].
pub fn parse_enum_def(par: &mut Parser, pub_qual: Option<Span>) -> ParseResult<Node<Enum>> {
    let enum_tok = par.assert(TokenKind::Enum);
    let name = par.expect_with_notes(
        TokenKind::Name,
        "failed to parse enum definition",
        |_| vec!["Note: `enum` must be followed by a name, which must start with a letter and contain only letters, numbers, or underscores".into()],
    )?;

    let mut span = enum_tok.span + name.span;
    let mut variants = vec![];
    let mut functions = vec![];

    par.enter_block(span, "enum definition")?;
    loop {
        par.eat_newlines();
        match par.peek_or_err()? {
            TokenKind::Name => {
                let variant = parse_variant(par)?;
                if !functions.is_empty() {
                    par.error(
                        variant.span,
                        "enum variant definitions must come before any function definitions",
                    );
                }
                variants.push(variant);
            }

            TokenKind::Fn | TokenKind::Unsafe => {
                functions.push(parse_fn_def(par, None)?);
            }

            TokenKind::Pub => {
                let pub_qual = Some(par.next().unwrap().span);
                match par.peek() {
                    Some(TokenKind::Fn | TokenKind::Unsafe) => {
                        functions.push(parse_fn_def(par, pub_qual)?);
                    }

                    _ => {
                        par.error(
                            pub_qual.unwrap(),
                            "expected `fn` or `unsafe fn` after `pub`",
                        );
                    }
                }
            }

            TokenKind::BraceClose => {
                span += par.next()?.span;
                break;
            }

            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(&tok, "failed to parse enum definition body", vec![]);
                return Err(ParseFailed);
            }
        };
    }

    Ok(Node::new(
        ast::Enum {
            name: name.into(),
            variants,
            functions,
            pub_qual,
        },
        span,
    ))
}

/// Parse a trait definition.
/// # Panics
/// Panics if the next token isn't `trait`.
pub fn parse_trait_def(par: &mut Parser, pub_qual: Option<Span>) -> ParseResult<Node<Trait>> {
    let trait_tok = par.assert(TokenKind::Trait);

    // trait Event {}
    let trait_name = par.expect_with_notes(
        TokenKind::Name,
        "failed to parse trait definition",
        |_| vec!["Note: `trait` must be followed by a name, which must start with a letter and contain only letters, numbers, or underscores".into()],
    )?;

    let header_span = trait_tok.span + trait_name.span;
    let mut functions = vec![];
    par.enter_block(header_span, "trait definition")?;

    loop {
        match par.peek_or_err()? {
            TokenKind::Fn => {
                // TODO: Traits should also be allowed to have functions that do contain a body.
                functions.push(parse_fn_sig(par, None)?);
                par.expect_with_notes(
                    TokenKind::Semi,
                    "failed to parse trait definition",
                    |_| vec!["Note: trait functions must appear without body and followed by a semicolon.".into()],
                )?;
                par.eat_newlines();
            }
            TokenKind::BraceClose => {
                par.next()?;
                break;
            }
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(&tok, "failed to parse trait definition body", vec![]);
                return Err(ParseFailed);
            }
        };
    }

    let span = header_span + pub_qual;
    Ok(Node::new(
        Trait {
            name: Node::new(trait_name.text.into(), trait_name.span),
            functions,
            pub_qual,
        },
        span,
    ))
}

/// Parse an impl block.
/// # Panics
/// Panics if the next token isn't `impl`.
pub fn parse_impl_def(par: &mut Parser) -> ParseResult<Node<Impl>> {
    let impl_tok = par.assert(TokenKind::Impl);

    // impl SomeTrait for SomeType {}
    let trait_name =
        par.expect_with_notes(TokenKind::Name, "failed to parse `impl` definition", |_| {
            vec!["Note: `impl` must be followed by the name of a trait".into()]
        })?;

    let for_tok =
        par.expect_with_notes(TokenKind::For, "failed to parse `impl` definition", |_| {
            vec![format!(
                "Note: `impl {}` must be followed by the keyword `for`",
                trait_name.text
            )]
        })?;

    let receiver = parse_type_desc(par)?;
    let mut functions = vec![];

    let header_span = impl_tok.span + trait_name.span + for_tok.span + receiver.span;

    par.enter_block(header_span, "impl definition")?;

    loop {
        par.eat_newlines();
        match par.peek_or_err()? {
            TokenKind::Fn => {
                functions.push(parse_fn_def(par, None)?);
            }
            TokenKind::BraceClose => {
                par.next()?;
                break;
            }
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(&tok, "failed to parse `impl` definition body", vec![]);
                return Err(ParseFailed);
            }
        };
    }

    Ok(Node::new(
        Impl {
            impl_trait: Node::new(trait_name.text.into(), trait_name.span),
            receiver,
            functions,
        },
        header_span,
    ))
}

/// Parse a type alias definition, e.g. `type MyMap = Map<u8, address>`.
/// # Panics
/// Panics if the next token isn't `type`.
pub fn parse_type_alias(par: &mut Parser, pub_qual: Option<Span>) -> ParseResult<Node<TypeAlias>> {
    let type_tok = par.assert(TokenKind::Type);
    let name = par.expect(TokenKind::Name, "failed to parse type declaration")?;
    par.expect_with_notes(TokenKind::Eq, "failed to parse type declaration", |_| {
        vec![
            "Note: a type alias name must be followed by an equals sign and a type description"
                .into(),
            format!("Example: `type {} = Map<address, u64>`", name.text),
        ]
    })?;
    let typ = parse_type_desc(par)?;
    let span = type_tok.span + pub_qual + typ.span;
    Ok(Node::new(
        TypeAlias {
            name: name.into(),
            typ,
            pub_qual,
        },
        span,
    ))
}

/// Parse a field for a struct or contract. The leading optional `pub` and
/// `const` qualifiers must be parsed by the caller, and passed in.
pub fn parse_field(
    par: &mut Parser,
    attributes: Vec<Node<SmolStr>>,
    pub_qual: Option<Span>,
    const_qual: Option<Span>,
) -> ParseResult<Node<Field>> {
    let name = par.expect(TokenKind::Name, "failed to parse field definition")?;
    par.expect_with_notes(
        TokenKind::Colon,
        "failed to parse field definition",
        |next| {
            let mut notes = vec![];
            if name.text == "def" && next.kind == TokenKind::Name {
                notes.push("Hint: use `fn` to define a function".into());
                notes.push(format!(
                    "Example: `{}fn {}( ...`",
                    if pub_qual.is_some() { "pub " } else { "" },
                    next.text
                ));
            }
            notes
                .push("Note: field name must be followed by a colon and a type description".into());
            notes.push(format!(
                "Example: {}{}{}: address",
                if pub_qual.is_some() { "pub " } else { "" },
                if const_qual.is_some() { "const " } else { "" },
                name.text
            ));
            notes
        },
    )?;

    let typ = parse_type_desc(par)?;
    let value = if par.peek() == Some(TokenKind::Eq) {
        par.next()?;
        Some(parse_expr(par)?)
    } else {
        None
    };
    par.expect_stmt_end("field definition")?;
    let span = name.span + pub_qual + const_qual + &typ;
    Ok(Node::new(
        Field {
            is_pub: pub_qual.is_some(),
            is_const: const_qual.is_some(),
            attributes,
            name: name.into(),
            typ,
            value,
        },
        span,
    ))
}

/// Parse a variant for a enum definition.
/// # Panics
/// Panics if the next token isn't [`TokenKind::Name`].
pub fn parse_variant(par: &mut Parser) -> ParseResult<Node<Variant>> {
    let name = par.expect(TokenKind::Name, "failed to parse enum variant")?;
    let mut span = name.span;

    let kind = match par.peek_or_err()? {
        TokenKind::ParenOpen => {
            span += par.next().unwrap().span;
            let mut tys = vec![];
            loop {
                match par.peek_or_err()? {
                    TokenKind::ParenClose => {
                        span += par.next().unwrap().span;
                        break;
                    }

                    _ => {
                        let ty = parse_type_desc(par)?;
                        span += ty.span;
                        tys.push(ty);
                        if par.peek_or_err()? == TokenKind::Comma {
                            par.next()?;
                        } else {
                            span += par
                                .expect(
                                    TokenKind::ParenClose,
                                    "unexpected token while parsing enum variant",
                                )?
                                .span;
                            break;
                        }
                    }
                }
            }

            VariantKind::Tuple(tys)
        }

        _ => VariantKind::Unit,
    };

    par.expect_stmt_end("enum variant")?;
    Ok(Node::new(
        Variant {
            name: name.into(),
            kind,
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
                    &tok,
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
            // Parse non-constant generic argument.
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
            // Parse literal-type constant generic argument.
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
            // Parse expr-type constant generic argument.
            BraceOpen => {
                let brace_open = par.next()?;
                let expr = parse_expr(par)?;
                if !matches!(par.next()?.kind, BraceClose) {
                    par.error(brace_open.span, "missing closing delimiter `}`");
                    return Err(ParseFailed);
                }

                args.push(GenericArg::ConstExpr(expr));

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
                    "failed to parse generic type argument list",
                    vec![],
                );
                return Err(ParseFailed);
            }
        }
    }
    Ok(Node::new(args, span))
}

/// Returns path and trailing `::` token, if present.
pub fn parse_path_tail<'a>(
    par: &mut Parser<'a>,
    head: Node<SmolStr>,
) -> (Path, Span, Option<Token<'a>>) {
    let mut span = head.span;
    let mut segments = vec![head];
    while let Some(delim) = par.optional(TokenKind::ColonColon) {
        if let Some(name) = par.optional(TokenKind::Name) {
            span += name.span;
            segments.push(name.into());
        } else {
            return (Path { segments }, span, Some(delim));
        }
    }
    (Path { segments }, span, None)
}

/// Parse a type description, e.g. `u8` or `Map<address, u256>`.
pub fn parse_type_desc(par: &mut Parser) -> ParseResult<Node<TypeDesc>> {
    use TokenKind::*;
    let mut typ = match par.peek_or_err()? {
        SelfType => {
            let _self = par.next()?;
            Node::new(TypeDesc::SelfType, _self.span)
        }
        Name => {
            let name = par.next()?;
            match par.peek() {
                Some(ColonColon) => {
                    let (path, span, trailing_delim) = parse_path_tail(par, name.into());
                    if let Some(colons) = trailing_delim {
                        let next = par.next()?;
                        par.fancy_error(
                            "failed to parse type description",
                            vec![
                                Label::secondary(colons.span, "path delimiter"),
                                Label::primary(next.span, "expected a name"),
                            ],
                            vec![],
                        );
                        return Err(ParseFailed);
                    }
                    Node::new(TypeDesc::Path(path), span)
                }
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
                            &tok,
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
            par.unexpected_token_error(&tok, "failed to parse type description", vec![]);
            return Err(ParseFailed);
        }
    };

    while par.peek() == Some(BracketOpen) {
        let l_brack = par.next()?.span;

        if_chain! {
            if let Some(size_token) = par.optional(TokenKind::Int);
            if let Ok(dimension) = size_token.text.parse::<usize>();
            if let Some(r_brack) = par.optional(TokenKind::BracketClose);
            then {
                let span = typ.span + l_brack + r_brack.span;
                par.fancy_error(
                    "Outdated array syntax",
                    vec![
                        Label::primary(
                            span,
                            ""
                        )
                    ],
                    vec![
                        format!("Hint: Use `Array<{}, {}>`", typ.kind, dimension)
                    ]
                );
                typ = Node::new(
                    TypeDesc::Generic {
                        base: Node::new("Array".into(), typ.span),
                        args: Node::new(vec![
                            GenericArg::TypeDesc(
                                Node::new(typ.kind, typ.span)
                            ),
                            GenericArg::Int(
                                Node::new(dimension, size_token.span)
                            )
                        ], span)
                    },
                    span,
                );
            } else {
                par.fancy_error(
                    "Unexpected token while parsing type description",
                    vec![
                        Label::primary(
                            l_brack,
                            "Unexpected token"
                        )
                    ],
                    vec![
                        format!("Hint: To define an array type use `Array<{}, 10>`", typ.kind)
                    ]
                );
                return Err(ParseFailed);
            }
        }
    }

    Ok(typ)
}
