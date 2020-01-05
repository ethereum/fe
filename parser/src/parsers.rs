use std::convert::TryFrom;

use crate::ast::ModuleStmt::*;
use crate::ast::*;
use crate::builders::{
    alt,
    delimited,
    many0,
    many1,
    map,
    op_expr_builder,
    opt,
    pair,
    preceded,
    separated,
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
    let (input, newline_tok) = newline_token(input)?;
    let (input, _) = endmarker_token(input)?;

    Ok((
        input,
        Spanned {
            node: Module { body: vec![] },
            span: newline_tok.span,
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
    alt((contract_field, event_def))(input)
}

/// Parse a contract field definition.
pub fn contract_field(input: Cursor) -> ParseResult<Spanned<ContractStmt>> {
    let (input, (qual, name_tok)) = alt((
        // Look for a qualifier and field name first...
        map(pair(contract_field_qual, name_token), |res| {
            let (qual, tok) = res;
            (Some(qual), tok)
        }),
        // ...then fall back to just a field name
        map(name_token, |tok| (None, tok)),
    ))(input)?;

    let (input, _) = op(":")(input)?;
    let (input, typ) = type_desc(input)?;
    let (input, _) = newline_token(input)?;

    let span = match &qual {
        Some(spanned) => Span::from_pair(spanned, &typ),
        None => Span::from_pair(name_tok, &typ),
    };

    Ok((
        input,
        Spanned {
            node: ContractStmt::ContractField {
                qual,
                name: name_tok.string,
                typ,
            },
            span,
        },
    ))
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
    let (input, (qual, name_tok)) = alt((
        // Look for a qualifier and field name first...
        map(pair(event_field_qual, name_token), |res| {
            let (qual, tok) = res;
            (Some(qual), tok)
        }),
        // ...then fall back to just a field name
        map(name_token, |tok| (None, tok)),
    ))(input)?;

    let (input, _) = op(":")(input)?;
    let (input, typ) = type_desc(input)?;
    let (input, _) = newline_token(input)?;

    let span = match &qual {
        Some(spanned) => Span::from_pair(spanned, &typ),
        None => Span::from_pair(name_tok, &typ),
    };

    Ok((
        input,
        Spanned {
            node: EventField {
                qual,
                name: name_tok.string,
                typ,
            },
            span,
        },
    ))
}

/// Parse a type definition (type alias).
pub fn type_def(input: Cursor) -> ParseResult<Spanned<ModuleStmt>> {
    let (input, type_kw) = name("type")(input)?;
    let (input, name) = name_token(input)?;
    let (input, _) = op("=")(input)?;
    let (input, type_desc) = type_desc(input)?;

    let span = Span::from_pair(type_kw, &type_desc);

    Ok((
        input,
        Spanned {
            node: ModuleStmt::TypeDef {
                name: name.string,
                typ: type_desc,
            },
            span,
        },
    ))
}

/// Parse a type description e.g. "uint256" or "map<address, bool>".
pub fn type_desc(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    alt((map_type, base_type))(input)
}

/// Parse a map type e.g. "map<address, bool".
pub fn map_type(input: Cursor) -> ParseResult<Spanned<TypeDesc>> {
    alt((map_type_double, map_type_single))(input)
}

/// Parse a map type ending with a right-shift token.
///
/// Example:
/// map<address, map<uint256, bool>>
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

/// Parse a map type ending with a greater-than token.
///
/// Example:
/// map< address, map<uint256, map<bool, int128>> >
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

/// Parse a base type along with an optional array dimension list.
///
/// Example:
/// int128[2][3]
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

/// Parse an array dimension list e.g. "[2][3]"
pub fn arr_list(input: Cursor) -> ParseResult<Vec<Spanned<usize>>> {
    many0(arr_dim)(input)
}

/// Parse an array dimension e.g. "[2]"
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

    Ok((
        input,
        Spanned {
            node: n,
            span: Span::from_pair(l_bracket, r_bracket),
        },
    ))
}

pub fn try_from_tok<'a, P, O>(parser: P) -> impl Fn(Cursor<'a>) -> ParseResult<O>
where
    O: TryFrom<&'a Token<'a>>,
    <O as TryFrom<&'a Token<'a>>>::Error: std::fmt::Debug,
    P: Fn(Cursor<'a>) -> ParseResult<&Token>,
{
    map(parser, |tok| TryFrom::try_from(tok).unwrap())
}

/// Parse a contract field qualifier keyword e.g. "const".
pub fn contract_field_qual(input: Cursor) -> ParseResult<Spanned<ContractFieldQual>> {
    try_from_tok(alt((name("const"), name("pub"))))(input)
}

/// Parse an event field qualifier keyword i.e. "idx".
pub fn event_field_qual(input: Cursor) -> ParseResult<Spanned<EventFieldQual>> {
    try_from_tok(name("idx"))(input)
}

/// Parse a function qualifier keyword i.e. "pub".
pub fn func_qual(input: Cursor) -> ParseResult<Spanned<FuncQual>> {
    try_from_tok(name("pub"))(input)
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

/// Parse a comma-separated list of expressions.
pub fn exprs(input: Cursor) -> ParseResult<Vec<Spanned<Expr>>> {
    separated(expr, op(","), true)(input)
}

pub fn expr(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let (input, test) = disjunct(input)?;
    let (input, ternary) = opt(|input| {
        let (input, _) = name("if")(input)?;
        let (input, if_expr) = disjunct(input)?;
        let (input, _) = name("else")(input)?;
        let (input, else_expr) = expr(input)?;
        Ok((input, (if_expr, else_expr)))
    })(input)?;

    let result = match ternary {
        Some((if_expr, else_expr)) => {
            let span = Span::from_pair(&test, &else_expr);

            Spanned {
                node: Expr::Ternary {
                    test: Box::new(test),
                    if_expr: Box::new(if_expr),
                    else_expr: Box::new(else_expr),
                },
                span,
            }
        }
        None => test,
    };

    Ok((input, result))
}

#[inline]
pub fn bool_op_builder<'a>(
    left: Spanned<Expr<'a>>,
    op: &'a Token,
    right: Spanned<Expr<'a>>,
) -> Expr<'a> {
    Expr::BoolOperation {
        left: Box::new(left),
        op: TryFrom::try_from(op).unwrap(),
        right: Box::new(right),
    }
}

#[inline]
pub fn bin_op_builder<'a>(
    left: Spanned<Expr<'a>>,
    op: &'a Token,
    right: Spanned<Expr<'a>>,
) -> Expr<'a> {
    Expr::BinOperation {
        left: Box::new(left),
        op: TryFrom::try_from(op).unwrap(),
        right: Box::new(right),
    }
}

#[inline]
pub fn unary_op_builder<'a>(op: &'a Token, operand: Spanned<Expr<'a>>) -> Expr<'a> {
    Expr::UnaryOperation {
        op: TryFrom::try_from(op).unwrap(),
        operand: Box::new(operand),
    }
}

#[inline]
pub fn comp_op_builder<'a>(
    left: Spanned<Expr<'a>>,
    op: Spanned<CompOperator>,
    right: Spanned<Expr<'a>>,
) -> Expr<'a> {
    Expr::CompOperation {
        left: Box::new(left),
        op,
        right: Box::new(right),
    }
}

pub fn disjunct(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(conjunct, name("or"), bool_op_builder)(input)
}

pub fn conjunct(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(comparison, name("and"), bool_op_builder)(input)
}

pub fn comparison(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let (input, nots) = many0(name("not"))(input)?;
    let (input, op_expr) = op_expr_builder(bitwise_or, comp_op, comp_op_builder)(input)?;

    let mut result = op_expr;
    for not_tok in nots.into_iter().rev() {
        let span = Span::from_pair(not_tok, &result);

        result = Spanned {
            node: unary_op_builder(not_tok, result),
            span,
        };
    }

    Ok((input, result))
}

pub fn comp_op(input: Cursor) -> ParseResult<Spanned<CompOperator>> {
    alt((
        map(
            alt((pair(name("not"), name("in")), pair(name("is"), name("not")))),
            |toks| {
                let (fst, snd) = toks;
                TryFrom::try_from(&[fst, snd][..]).unwrap()
            },
        ),
        map(
            alt((
                op("<"),
                op("<="),
                op("=="),
                op(">="),
                op(">"),
                op("!="),
                name("in"),
                name("is"),
            )),
            |tok| TryFrom::try_from(&[tok][..]).unwrap(),
        ),
    ))(input)
}

pub fn bitwise_or(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(bitwise_xor, op("|"), bin_op_builder)(input)
}

pub fn bitwise_xor(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(bitwise_and, op("^"), bin_op_builder)(input)
}

pub fn bitwise_and(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(shift_expr, op("&"), bin_op_builder)(input)
}

pub fn shift_expr(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(sum, alt((op("<<"), op(">>"))), bin_op_builder)(input)
}

pub fn sum(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(term, alt((op("+"), op("-"))), bin_op_builder)(input)
}

pub fn term(input: Cursor) -> ParseResult<Spanned<Expr>> {
    op_expr_builder(
        factor,
        alt((op("*"), op("/"), op("//"), op("%"))),
        bin_op_builder,
    )(input)
}

pub fn factor(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let unary_op = |input| {
        let (input, op_tok) = alt((op("+"), op("-"), op("~")))(input)?;
        let (input, factor_expr) = factor(input)?;

        let span = Span::from_pair(op_tok, &factor_expr);

        Ok((
            input,
            Spanned {
                node: unary_op_builder(op_tok, factor_expr),
                span,
            },
        ))
    };

    alt((unary_op, power))(input)
}

pub fn power(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let power_op = |input| {
        let (input, primary_expr) = primary(input)?;
        let (input, op_tok) = op("**")(input)?;
        let (input, factor_expr) = factor(input)?;

        let span = Span::from_pair(&primary_expr, &factor_expr);

        Ok((
            input,
            Spanned {
                node: bin_op_builder(primary_expr, op_tok, factor_expr),
                span,
            },
        ))
    };

    alt((power_op, primary))(input)
}

pub fn primary(input: Cursor) -> ParseResult<Spanned<Expr>> {
    let (input, atom_expr) = atom(input)?;

    let mut input = input;
    let mut result = atom_expr;

    loop {
        if let Ok((input_, name_tok)) = attr_tail(input) {
            input = input_;

            let span = Span::from_pair(&result, name_tok);

            result = Spanned {
                node: Expr::Attribute {
                    value: Box::new(result),
                    attr: name_tok.into(),
                },
                span,
            };
        } else if let Ok((input_, slices)) = index_tail(input) {
            input = input_;

            let span = Span::from_pair(&result, &slices);

            result = Spanned {
                node: Expr::Subscript {
                    value: Box::new(result),
                    slices,
                },
                span,
            };
        } else if let Ok((input_, args)) = call_tail(input) {
            input = input_;

            let span = Span::from_pair(&result, &args);

            result = Spanned {
                node: Expr::Call {
                    func: Box::new(result),
                    args,
                },
                span,
            };
        } else {
            break;
        }
    }

    Ok((input, result))
}

pub fn slices(input: Cursor) -> ParseResult<Vec<Spanned<Slice>>> {
    separated(slice, op(","), true)(input)
}

pub fn slice(input: Cursor) -> ParseResult<Spanned<Slice>> {
    alt((
        |input| {
            let boxed_expr = map(expr, Box::new);

            let (input, lower) = opt(&boxed_expr)(input)?;
            let (input, colon) = op(":")(input)?;
            let (input, upper) = opt(&boxed_expr)(input)?;
            let (input, step) = opt(preceded(op(":"), &boxed_expr))(input)?;

            let first = match &lower {
                Some(bx) => bx.span,
                None => colon.span,
            };
            let last = match (&upper, &step) {
                (_, Some(bx)) => bx.span,
                (Some(bx), _) => bx.span,
                _ => colon.span,
            };

            let span = Span::from_pair(first, last);

            Ok((
                input,
                Spanned {
                    node: Slice::Slice { lower, upper, step },
                    span,
                },
            ))
        },
        map(expr, |e| Spanned {
            node: Slice::Index(Box::new(e.node)),
            span: e.span,
        }),
    ))(input)
}

pub fn atom(input: Cursor) -> ParseResult<Spanned<Expr>> {
    alt((
        list,
        map(group, |exp| Spanned {
            node: exp.node.node,
            span: exp.span,
        }),
        tuple,
        map(name_token, |tok| Spanned {
            node: Expr::Name(tok.string),
            span: tok.span,
        }),
        map(number_token, |tok| Spanned {
            node: Expr::Num(tok.string),
            span: tok.span,
        }),
        map(many1(string_token), |toks| {
            let tok_strings: Vec<_> = toks.iter().map(|t| t.string).collect();

            let fst = toks.first().unwrap();
            let snd = toks.last().unwrap();

            Spanned {
                node: Expr::Str(tok_strings),
                span: Span::from_pair(*fst, *snd),
            }
        }),
        map(op("..."), |tok| Spanned {
            node: Expr::Ellipsis,
            span: tok.span,
        }),
    ))(input)
}

pub fn list(input: Cursor) -> ParseResult<Spanned<Expr>> {
    map(delimited(op("["), opt(exprs), op("]")), |spanned_exprs| {
        let Spanned { node, span } = spanned_exprs;
        let elts = node.unwrap_or(vec![]);

        Spanned {
            node: Expr::List { elts },
            span,
        }
    })(input)
}

pub fn tuple(input: Cursor) -> ParseResult<Spanned<Expr>> {
    map(delimited(op("("), opt(exprs), op(")")), |spanned_exprs| {
        let Spanned { node, span } = spanned_exprs;
        let elts = node.unwrap_or(vec![]);

        Spanned {
            node: Expr::Tuple { elts },
            span,
        }
    })(input)
}

pub fn group(input: Cursor) -> ParseResult<Spanned<Spanned<Expr>>> {
    delimited(op("("), expr, op(")"))(input)
}

pub fn args(input: Cursor) -> ParseResult<Vec<Spanned<CallArg>>> {
    let kw_result = kwargs(input);
    if kw_result.is_ok() {
        return kw_result;
    }

    let (input, first) = expr(input)?;
    let (input, rest) = opt(preceded(op(","), args))(input)?;

    let mut results = vec![Spanned {
        node: CallArg::Arg(first.node),
        span: first.span,
    }];
    if let Some(mut rest) = rest {
        results.append(&mut rest);
    }

    Ok((input, results))
}

pub fn kwargs(input: Cursor) -> ParseResult<Vec<Spanned<CallArg>>> {
    separated(kwarg, op(","), false)(input)
}

pub fn kwarg(input: Cursor) -> ParseResult<Spanned<CallArg>> {
    let (input, name_tok) = name_token(input)?;
    let (input, _) = op("=")(input)?;
    let (input, value_expr) = expr(input)?;

    let span = Span::from_pair(name_tok, &value_expr);

    Ok((
        input,
        Spanned {
            node: CallArg::Kwarg(Kwarg {
                name: name_tok.into(),
                value: Box::new(value_expr),
            }),
            span,
        },
    ))
}

pub fn attr_tail(input: Cursor) -> ParseResult<&Token> {
    preceded(op("."), name_token)(input)
}

pub fn index_tail(input: Cursor) -> ParseResult<Spanned<Vec<Spanned<Slice>>>> {
    delimited(op("["), slices, op("]"))(input)
}

pub fn call_tail(input: Cursor) -> ParseResult<Spanned<Vec<Spanned<CallArg>>>> {
    map(delimited(op("("), opt(args), op(")")), |spanned| Spanned {
        node: spanned.node.unwrap_or(vec![]),
        span: spanned.span,
    })(input)
}
