use std::convert::TryFrom;

use crate::ast::ModuleStmt::*;
use crate::ast::*;
use crate::builders::{
    alt,
    many0,
    many1,
    map,
    opt,
    pair,
    preceded,
    separated_pair,
    terminated,
    verify,
};
use crate::errors::ParseError;
use crate::span::{
    Span,
    Spanned,
};
use crate::tokenizer::types::{
    Token,
    TokenType,
};
use crate::{
    Cursor,
    ParseResult,
};

/// Parse next token in input.
pub fn next(input: Cursor) -> ParseResult<&Token> {
    match input.first() {
        Some(tok) => Ok((&input[1..], tok)),
        None => Err(ParseError::eof(input)),
    }
}

/// Parse a token of a specific type.
pub fn token<'a>(typ: TokenType) -> impl Fn(Cursor<'a>) -> ParseResult<&Token> {
    verify(
        next,
        move |t| t.typ == typ,
        move |inp, _| ParseError::str(inp, format!("expected {:?} token", typ)),
    )
}

/// Parse a name token.
pub fn name_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::NAME)(input)
}

/// Parse a name token containing a specific string.
#[allow(clippy::needless_lifetimes)]
pub fn name<'a>(string: &'a str) -> impl Fn(Cursor<'a>) -> ParseResult<&Token> {
    verify(
        name_token,
        move |t| t.string == string,
        move |inp, _| ParseError::str(inp, format!("expected \"{}\" name token", string)),
    )
}

/// Parse an op token.
pub fn op_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::OP)(input)
}

/// Parse an op token containing a specific string.
#[allow(clippy::needless_lifetimes)]
pub fn op<'a>(string: &'a str) -> impl Fn(Cursor<'a>) -> ParseResult<&Token> {
    verify(
        op_token,
        move |t| t.string == string,
        move |inp, _| ParseError::str(inp, format!("expected \"{}\" op token", string)),
    )
}

/// Parse a number token.
pub fn number_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::NUMBER)(input)
}

/// Parse a string token.
pub fn string_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::STRING)(input)
}

/// Parse an indent token.
pub fn indent_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::INDENT)(input)
}

/// Parse a dedent token.
pub fn dedent_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::DEDENT)(input)
}

/// Parse a grammatically significant newline token.
pub fn newline_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::NEWLINE)(input)
}

/// Parse an endmarker token.
pub fn endmarker_token(input: Cursor) -> ParseResult<&Token> {
    token(TokenType::ENDMARKER)(input)
}

/// Parse a module definition.
pub fn file_input(input: Cursor) -> ParseResult<Spanned<Module>> {
    alt((empty_file_input, non_empty_file_input))(input)
}

/// Parse an empty module definition.
pub fn empty_file_input(input: Cursor) -> ParseResult<Spanned<Module>> {
    // ENDMARKER
    let (input, end_tok) = endmarker_token(input)?;

    Ok((
        input,
        Spanned {
            node: Module { body: vec![] },
            span: end_tok.span,
        },
    ))
}

/// Parse a non-empty module definition.
pub fn non_empty_file_input(input: Cursor) -> ParseResult<Spanned<Module>> {
    // module_stmt+
    let (input, body) = many1(module_stmt)(input)?;

    // ENDMARKER
    let (input, _) = endmarker_token(input)?;

    let span = {
        let first = body.first().unwrap();
        let last = body.last().unwrap();

        Span::from_pair(first, last)
    };

    Ok((
        input,
        Spanned {
            node: Module { body },
            span,
        },
    ))
}

/// Parse a module statement, such as a contract definition.
pub fn module_stmt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    alt((import_stmt, contract_def))(input)
}

/// Parse an import statement.
pub fn import_stmt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    terminated(alt((simple_import, from_import)), newline_token)(input)
}

/// Parse an import statement beginning with the "import" keyword.
pub fn simple_import(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, import_kw) = name("import")(input)?;
    let (input, first_name) = simple_import_name(input)?;
    let (input, mut other_names) = many0(preceded(op(","), simple_import_name))(input)?;

    let mut result = vec![first_name];
    result.append(&mut other_names);

    let span = {
        let last = result.last().unwrap();
        Span::from_pair(import_kw, last)
    };

    Ok((
        input,
        Spanned {
            node: SimpleImport { names: result },
            span,
        },
    ))
}

pub fn simple_import_name(input: Cursor) -> ParseResult<Spanned<SimpleImportName>> {
    let (input, path) = dotted_name(input)?;
    let (input, alias) = opt(preceded(name("as"), name_token))(input)?;

    let span = {
        match alias {
            Some(alias_tok) => Span::from_pair(&path, alias_tok),
            None => path.span,
        }
    };

    Ok((
        input,
        Spanned {
            node: SimpleImportName {
                path: path.node,
                alias: alias.map(|t| t.string),
            },
            span,
        },
    ))
}

/// Parse an import statement beginning with the "from" keyword.
pub fn from_import(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    alt((from_import_parent_alt, from_import_sub_alt))(input)
}

/// Parse a "from" import with a path that contains only parent module
/// components.
pub fn from_import_parent_alt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, from_kw) = name("from")(input)?;
    let (input, parent_level) = dots_to_int(input)?;
    let (input, _) = name("import")(input)?;
    let (input, names) = from_import_names(input)?;

    let path = Spanned {
        node: FromImportPath::Relative {
            parent_level: parent_level.node,
            path: vec![],
        },
        span: parent_level.span,
    };
    let span = Span::from_pair(from_kw, names.span);

    Ok((
        input,
        Spanned {
            node: FromImport { path, names },
            span,
        },
    ))
}

/// Parse a "from" import with a path that contains sub module components.
pub fn from_import_sub_alt(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, from_kw) = name("from")(input)?;
    let (input, path) = from_import_sub_path(input)?;
    let (input, _) = name("import")(input)?;
    let (input, names) = from_import_names(input)?;

    let span = Span::from_pair(from_kw, names.span);

    Ok((
        input,
        Spanned {
            node: FromImport { path, names },
            span,
        },
    ))
}

/// Parse a path containing sub module components in a "from" import statement.
pub fn from_import_sub_path(input: Cursor) -> ParseResult<Spanned<FromImportPath>> {
    let (input, opt_parent_level) = opt(dots_to_int)(input)?;
    let (input, dotted_name) = dotted_name(input)?;

    let result = match opt_parent_level {
        Some(parent_level) => {
            let span = Span::from_pair(&parent_level, &dotted_name);
            Spanned {
                node: FromImportPath::Relative {
                    parent_level: parent_level.node,
                    path: dotted_name.node,
                },
                span,
            }
        }
        None => Spanned {
            node: FromImportPath::Absolute {
                path: dotted_name.node,
            },
            span: dotted_name.span,
        },
    };

    Ok((input, result))
}

/// Parse the names to be imported by a "from" import statement.
pub fn from_import_names(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    alt((
        from_import_names_star,
        from_import_names_parens,
        from_import_names_list,
    ))(input)
}

/// Parse a wildcard token ("*") in a "from" import statement.
pub fn from_import_names_star(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    let (input, star) = op("*")(input)?;

    Ok((
        input,
        Spanned {
            node: FromImportNames::Star,
            span: star.span,
        },
    ))
}

/// Parse a parenthesized list of names to be imported by a "from" import
/// statement.
pub fn from_import_names_parens(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    let (input, l_paren) = op("(")(input)?;
    let (input, names) = from_import_names_list(input)?;
    let (input, r_paren) = op(")")(input)?;

    Ok((
        input,
        Spanned {
            node: names.node,
            span: Span::from_pair(l_paren, r_paren),
        },
    ))
}

/// Parse a list of names to be imported by a "from" import statement.
pub fn from_import_names_list(input: Cursor) -> ParseResult<Spanned<FromImportNames>> {
    let (input, first_name) = from_import_name(input)?;
    let (input, mut other_names) = many0(preceded(op(","), from_import_name))(input)?;
    let (input, comma_tok) = opt(op(","))(input)?;

    let mut names = vec![first_name];
    names.append(&mut other_names);

    let span = {
        let first = names.first().unwrap();
        match comma_tok {
            Some(tok) => Span::from_pair(first, tok),
            None => {
                let last = names.last().unwrap();
                Span::from_pair(first, last)
            }
        }
    };

    Ok((
        input,
        Spanned {
            node: FromImportNames::List(names),
            span,
        },
    ))
}

/// Parse an import name with an optional alias in a "from" import statement.
pub fn from_import_name(input: Cursor) -> ParseResult<Spanned<FromImportName>> {
    let (input, name_tok) = name_token(input)?;
    let (input, alias) = opt(preceded(name("as"), name_token))(input)?;

    let span = match alias {
        Some(alias_tok) => Span::from_pair(name_tok, alias_tok),
        None => name_tok.span,
    };

    Ok((
        input,
        Spanned {
            node: FromImportName {
                name: name_tok.string,
                alias: alias.map(|t| t.string),
            },
            span,
        },
    ))
}

/// Parse a dotted import name.
pub fn dotted_name(input: Cursor) -> ParseResult<Spanned<Vec<&str>>> {
    let (input, first_part) = name_token(input)?;
    let (input, other_parts) = many0(preceded(op("."), name_token))(input)?;

    let mut path = vec![first_part.string];
    path.extend(other_parts.iter().map(|t| t.string));

    let span = if other_parts.is_empty() {
        first_part.span
    } else {
        let last_part = other_parts.last().unwrap();
        Span::from_pair(first_part, *last_part)
    };

    Ok((input, Spanned { node: path, span }))
}

/// Parse preceding dots used to indicate parent module imports in import
/// statements.
pub fn dots_to_int(input: Cursor) -> ParseResult<Spanned<usize>> {
    let (input, toks) = many1(alt((op("."), op("..."))))(input)?;

    let value = toks
        .iter()
        .map(|t| if t.string == "." { 1 } else { 3 })
        .sum::<usize>()
        - 1;

    let span = {
        let first = toks.first().unwrap();
        let last = toks.last().unwrap();

        Span::from_pair(*first, *last)
    };

    Ok((input, Spanned { node: value, span }))
}

/// Parse a contract definition statement.
pub fn contract_def(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    // "contract" name ":" NEWLINE
    let (input, contract_kw) = name("contract")(input)?;
    let (input, name_tok) = name_token(input)?;
    let (input, _) = op(":")(input)?;
    let (input, _) = newline_token(input)?;

    // INDENT contract_stmt+ DEDENT
    let (input, _) = indent_token(input)?;
    let (input, body) = many1(contract_stmt)(input)?;
    let (input, _) = dedent_token(input)?;

    let last_stmt = body.last().unwrap();
    let span = Span::from_pair(contract_kw, last_stmt);

    Ok((
        input,
        Spanned {
            node: ContractDef {
                name: name_tok.string,
                body,
            },
            span,
        },
    ))
}

/// Parse a contract statement.
pub fn contract_stmt(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {
    event_def(input)
}

/// Parse an event definition statement.
pub fn event_def(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {
    // "event" name ":" NEWLINE
    let (input, event_kw) = name("event")(input)?;
    let (input, name_tok) = name_token(input)?;
    let (input, _) = op(":")(input)?;
    let (input, _) = newline_token(input)?;

    // INDENT event_field+ DEDENT
    let (input, _) = indent_token(input)?;
    let (input, fields) = many1(event_field)(input)?;
    let (input, _) = dedent_token(input)?;

    let last_field = fields.last().unwrap();
    let span = Span::from_pair(event_kw, last_field);

    Ok((
        input,
        Spanned {
            node: ContractStmt::EventDef {
                name: name_tok.string,
                fields,
            },
            span,
        },
    ))
}

/// Parse an event field definition.
pub fn event_field(input: Cursor) -> ParseResult<Spanned<EventField>> {
    let (input, name_tok) = name_token(input)?;
    let (input, _) = op(":")(input)?;
    let (input, typ) = name_token(input)?;
    let (input, _) = newline_token(input)?;

    let span = Span::from_pair(name_tok, typ);

    Ok((
        input,
        Spanned {
            node: EventField {
                name: name_tok.string,
                typ: typ.into(),
            },
            span,
        },
    ))
}

pub fn type_desc(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    alt((map_type, base_type))(input)
}

pub fn map_type(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    alt((map_type_double, map_type_single))(input)
}

pub fn map_type_double(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    let (input, map_kw_1) = name("map")(input)?;
    let (input, _) = op("<")(input)?;
    let (input, from_1) = base_type(input)?;
    let (input, _) = op(",")(input)?;

    let (input, map_kw_2) = name("map")(input)?;
    let (input, _) = op("<")(input)?;
    let (input, from_2) = base_type(input)?;
    let (input, _) = op(",")(input)?;

    let (input, to) = type_desc(input)?;
    let (input, r_bracket) = op(">>")(input)?;

    let inner_map = Spanned {
        node: TypeDesc::Map {
            from: Box::new(from_2),
            to: Box::new(to),
        },
        span: Span::new(map_kw_2.span.start, r_bracket.span.end - 1),
    };

    Ok((
        input,
        Spanned {
            node: TypeDesc::Map {
                from: Box::new(from_1),
                to: Box::new(inner_map),
            },
            span: Span::from_pair(map_kw_1, r_bracket),
        },
    ))
}

pub fn map_type_single(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    let (input, map_kw) = name("map")(input)?;
    let (input, _) = op("<")(input)?;
    let (input, from) = base_type(input)?;
    let (input, _) = op(",")(input)?;
    let (input, to) = type_desc(input)?;
    let (input, r_bracket) = op(">")(input)?;

    Ok((
        input,
        Spanned {
            node: TypeDesc::Map {
                from: Box::new(from),
                to: Box::new(to),
            },
            span: Span::from_pair(map_kw, r_bracket),
        },
    ))
}

pub fn base_type(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    let (input, base) = name_token(input)?;
    let (input, dims) = arr_list(input)?;

    let mut result = Spanned {
        node: TypeDesc::Base { base: base.string },
        span: base.into(),
    };
    for dim in dims {
        let span = Span::from_pair(&result, &dim);

        result = Spanned {
            node: TypeDesc::Array {
                typ: Box::new(result),
                dimension: dim.node,
            },
            span,
        };
    }

    Ok((input, result))
}

pub fn arr_list(input: Cursor) -> ParseResult<Vec<Spanned<usize>>> {
    many0(arr_dim)(input)
}

pub fn arr_dim(input: Cursor) -> ParseResult<Spanned<usize>> {
    let (num_input, l_bracket) = op("[")(input)?;
    let (input, num_tok) = number_token(num_input)?;
    let (input, r_bracket) = op("]")(input)?;

    let n: usize = match num_tok.string.parse() {
        Ok(n) => n,
        Err(_) => {
            return Err(ParseError::str(
                num_input,
                format!("invalid integer literal \"{}\"", num_tok.string),
            ))
        }
    };
    if n < 1 {
        return Err(ParseError::static_str(
            num_input,
            "array dimensions must be positive",
        ));
    }

    Ok((
        input,
        Spanned {
            node: n,
            span: Span::from_pair(l_bracket, r_bracket),
        },
    ))
}

/// Parse a constant expression that can be evaluated at compile-time.
pub fn const_expr(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    let (input, head) = const_term(input)?;
    let (input, tail) = many0(alt((pair(op("+"), const_term), pair(op("-"), const_term))))(input)?;

    let mut left_expr = head;
    for (op_tok, right_expr) in tail {
        let span = Span::from_pair(&left_expr, &right_expr);

        left_expr = Spanned {
            node: ConstExpr::BinOp {
                left: Box::new(left_expr),
                op: Operator::try_from(op_tok.string).unwrap(),
                right: Box::new(right_expr),
            },
            span,
        };
    }

    Ok((input, left_expr))
}

/// Parse a constant term that may appear as the operand of an addition or
/// subtraction.
pub fn const_term(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    let (input, head) = const_factor(input)?;
    let (input, tail) = many0(alt((
        pair(op("*"), const_factor),
        pair(op("/"), const_factor),
        pair(op("%"), const_factor),
    )))(input)?;

    let mut left_expr = head;
    for (op_tok, right_expr) in tail {
        let span = Span::from_pair(&left_expr, &right_expr);

        left_expr = Spanned {
            node: ConstExpr::BinOp {
                left: Box::new(left_expr),
                op: Operator::try_from(op_tok.string).unwrap(),
                right: Box::new(right_expr),
            },
            span,
        };
    }

    Ok((input, left_expr))
}

/// Parse a constant factor that may appear as the operand of a multiplication,
/// division, modulus, or unary op or as the exponent of a power expression.
pub fn const_factor(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    let unary_op = map(
        pair(alt((op("+"), op("-"), op("~"))), const_factor),
        |res| {
            let (op_tok, operand) = res;
            let span = Span::from_pair(op_tok, &operand);

            Spanned {
                node: ConstExpr::UnaryOp {
                    op: UnaryOp::try_from(op_tok.string).unwrap(),
                    operand: Box::new(operand),
                },
                span,
            }
        },
    );

    alt((unary_op, const_power))(input)
}

/// Parse a constant power expression that may appear in the position of a
/// constant factor.
pub fn const_power(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    let bin_op = map(separated_pair(const_atom, op("**"), const_factor), |res| {
        let (left, right) = res;
        let span = Span::from_pair(&left, &right);

        Spanned {
            node: ConstExpr::BinOp {
                left: Box::new(left),
                op: Operator::Pow,
                right: Box::new(right),
            },
            span,
        }
    });

    alt((bin_op, const_atom))(input)
}

/// Parse a constant atom expression that may appear in the position of a
/// constant power or as the base of a constant power expression.
pub fn const_atom(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    alt((
        const_group,
        map(name_token, |t| Spanned {
            node: ConstExpr::Name { name: t.string },
            span: t.span,
        }),
        map(number_token, |t| Spanned {
            node: ConstExpr::Num { num: t.string },
            span: t.span,
        }),
    ))(input)
}

/// Parse a parenthesized constant group that may appear in the position of a
/// constant atom.
pub fn const_group(input: Cursor) -> ParseResult<Spanned<ConstExpr>> {
    let (input, l_paren) = op("(")(input)?;
    let (input, spanned_expr) = const_expr(input)?;
    let (input, r_paren) = op(")")(input)?;

    Ok((
        input,
        Spanned {
            node: spanned_expr.node,
            span: Span::from_pair(l_paren, r_paren),
        },
    ))
}
