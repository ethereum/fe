use crate::ast::{self, CallArg, Expr, GenericArg, Path};
use crate::node::Node;
use crate::{Label, ParseFailed, ParseResult, Parser, Token, TokenKind};

use super::types::parse_generic_args;

use if_chain::if_chain;

// Expressions are parsed in Pratt's top-down operator precedence style.
// See this article for a nice explanation:
// <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>

/// Parse an expression, starting with the next token.
pub fn parse_expr(par: &mut Parser) -> ParseResult<Node<Expr>> {
    parse_expr_with_min_bp(par, 0)
}

/// Parse an expression, stopping if/when we reach an operator that binds less
/// tightly than given binding power.
pub fn parse_expr_with_min_bp(par: &mut Parser, min_bp: u8) -> ParseResult<Node<Expr>> {
    let mut expr_head = parse_expr_head(par)?;

    while let Some(op) = par.peek() {
        if let Some(lbp) = postfix_binding_power(op) {
            if lbp < min_bp {
                break;
            }

            expr_head = match op {
                TokenKind::ParenOpen => {
                    let args = parse_call_args(par)?;
                    let span = expr_head.span + args.span;
                    Node::new(
                        Expr::Call {
                            func: Box::new(expr_head),
                            generic_args: None,
                            args,
                        },
                        span,
                    )
                }
                TokenKind::BracketOpen => {
                    par.next()?;
                    let index = parse_expr(par)?;
                    let rbracket = par.expect(
                        TokenKind::BracketClose,
                        "failed to parse subscript expression",
                    )?;
                    let span = expr_head.span + rbracket.span;
                    Node::new(
                        Expr::Subscript {
                            value: Box::new(expr_head),
                            index: Box::new(index),
                        },
                        span,
                    )
                }
                TokenKind::If => {
                    par.next()?;
                    let test = parse_expr(par)?;
                    par.expect(
                        TokenKind::Else,
                        "failed to parse ternary `if-else` expression",
                    )?;
                    let else_val = parse_expr(par)?;
                    let span = expr_head.span + else_val.span;
                    Node::new(
                        Expr::Ternary {
                            if_expr: Box::new(expr_head),
                            test: Box::new(test),
                            else_expr: Box::new(else_val),
                        },
                        span,
                    )
                }
                _ => unreachable!(), // patterns above must match those in `postfix_binding_power`
            };
            continue;
        }

        if matches!(op, TokenKind::Lt) {
            let mut bt_par = par.as_bt_parser();
            if_chain! {
                if let Ok(generic_args) = parse_generic_args(&mut bt_par);
                if matches!(bt_par.peek(), Some(TokenKind::ParenOpen));
                if let Ok(args) = parse_call_args(&mut bt_par);
                then {
                    let span = expr_head.span + args.span;
                    expr_head = Node::new(
                        Expr::Call {
                            func: Box::new(expr_head),
                            generic_args: Some(generic_args),
                            args,
                        },
                        span,
                    );
                    bt_par.accept();
                    continue;
                }
            }
        }

        if let Some((lbp, rbp)) = infix_binding_power(op) {
            if lbp < min_bp {
                break;
            }

            let op_tok = par.next()?;
            let rhs = parse_expr_with_min_bp(par, rbp)?;
            expr_head = infix_op(par, expr_head, &op_tok, rhs)?;
            continue;
        }
        break;
    }

    Ok(expr_head)
}

/// Parse call arguments
pub fn parse_call_args(par: &mut Parser) -> ParseResult<Node<Vec<Node<CallArg>>>> {
    use TokenKind::*;
    let lparen = par.assert(ParenOpen);
    let mut args = vec![];
    let mut span = lparen.span;
    loop {
        if par.peek_or_err()? == ParenClose {
            span += par.next()?.span;
            break;
        }

        let arg = parse_expr(par)?;
        match par.peek_or_err()? {
            TokenKind::Eq => {
                let eq = par.next()?;
                if let Expr::Name(name) = arg.kind {
                    par.fancy_error(
                        "Syntax error in argument list",
                        vec![Label::primary(eq.span, "unexpected `=`".to_string())],
                        vec![
                            "Argument labels should be followed by `:`.".to_string(),
                            format!("Hint: try `{name}:`"),
                            "If this is a variable assignment, it must be a standalone statement."
                                .to_string(),
                        ],
                    );
                    let value = parse_expr(par)?;
                    let span = arg.span + value.span;
                    args.push(Node::new(
                        CallArg {
                            label: Some(Node::new(name, arg.span)),
                            value,
                        },
                        span,
                    ));
                } else {
                    par.fancy_error(
                        "Syntax error in argument list",
                        vec![Label::primary(eq.span, "unexpected `=`".to_string())],
                        vec![],
                    );
                }
            }

            TokenKind::Colon => {
                let sep_tok = par.next()?;
                let value = parse_expr(par)?;
                if let Expr::Name(name) = arg.kind {
                    let span = arg.span + value.span;
                    args.push(Node::new(
                        CallArg {
                            label: Some(Node::new(name, arg.span)),
                            value,
                        },
                        span,
                    ));
                } else {
                    par.fancy_error(
                        "Syntax error in function call argument list",
                        vec![
                            Label::primary(
                                sep_tok.span,
                                "In a function call, `:` is used for named arguments".to_string(),
                            ),
                            Label::secondary(
                                arg.span,
                                "this should be a function parameter name".to_string(),
                            ),
                        ],
                        vec![],
                    );
                    return Err(ParseFailed);
                }
            }
            _ => {
                let span = arg.span;
                args.push(Node::new(
                    CallArg {
                        label: None,
                        value: arg,
                    },
                    span,
                ));
            }
        }
        if par.peek_or_err()? == Comma {
            par.next()?;
        } else {
            span += par
                .expect(ParenClose, "failed to parse function call argument list")?
                .span;
            break;
        }
    }

    Ok(Node::new(args, span))
}

/// Try to build an expression starting with the given token.
fn parse_expr_head(par: &mut Parser) -> ParseResult<Node<Expr>> {
    use TokenKind::*;

    match par.peek_or_err()? {
        Name | SelfValue | Int | Hex | Octal | Binary | Text | True | False => {
            let tok = par.next()?;
            Ok(atom(par, &tok))
        }
        Plus | Minus | Not | Tilde => {
            let op = par.next()?;
            let operand = parse_expr_with_min_bp(par, prefix_binding_power(op.kind))?;
            unary_op(par, &op, operand)
        }
        ParenOpen => parse_group_or_tuple(par),
        BracketOpen => parse_list_or_repeat(par),
        _ => {
            let tok = par.next()?;
            par.unexpected_token_error(
                &tok,
                format!("Unexpected token while parsing expression: `{}`", tok.text),
                vec![],
            );
            Err(ParseFailed)
        }
    }
}

/// Specifies how tightly a prefix unary operator binds to its operand.
fn prefix_binding_power(op: TokenKind) -> u8 {
    use TokenKind::*;
    match op {
        Not => 65,
        Plus | Minus | Tilde => 135,
        _ => panic!("Unexpected unary op token: {op:?}"),
    }
}

/// Specifies how tightly does an infix operator bind to its left and right
/// operands. See https://docs.python.org/3/reference/expressions.html#operator-precedence
fn infix_binding_power(op: TokenKind) -> Option<(u8, u8)> {
    use TokenKind::*;

    let bp = match op {
        // assignment expr `:=`?
        // lambda?
        // Comma => (40, 41),
        Or => (50, 51),
        And => (60, 61),
        // prefix Not => 65

        // all comparisons are the same
        Lt | LtEq | Gt | GtEq | NotEq | EqEq => (70, 71),

        Pipe => (80, 81),
        Hat => (90, 91),
        Amper => (100, 101),
        LtLt | GtGt => (110, 111),
        Plus | Minus => (120, 121),
        Star | Slash | Percent => (130, 131),
        // Prefix Plus | Minus | Tilde => 135
        StarStar => (141, 140),
        Dot => (150, 151),
        ColonColon => (160, 161),
        _ => return None,
    };
    Some(bp)
}

/// Specifies how tightly a postfix operator binds to its operand.
/// We don't have any "real" postfix operators (like `?` in rust),
/// but we treat `[`, `(`, and ternary `if` as though they're postfix
/// operators.
fn postfix_binding_power(op: TokenKind) -> Option<u8> {
    use TokenKind::*;
    match op {
        If => Some(35), // ternary
        BracketOpen | ParenOpen => Some(150),
        _ => None,
    }
}

/// Parse a square-bracket list expression, eg. `[1, 2, x]` or `[true; 42]`
fn parse_list_or_repeat(par: &mut Parser) -> ParseResult<Node<Expr>> {
    let lbracket = par.assert(TokenKind::BracketOpen);
    let elts = parse_expr_list(par, &[TokenKind::BracketClose, TokenKind::Semi], None)?;

    if elts.len() == 1 {
        if par.peek() == Some(TokenKind::BracketClose) {
            let rbracket = par.assert(TokenKind::BracketClose);
            let span = lbracket.span + rbracket.span;
            Ok(Node::new(Expr::List { elts }, span))
        } else if par.peek() == Some(TokenKind::Semi) {
            par.assert(TokenKind::Semi);

            let len = if par.peek() == Some(TokenKind::BraceOpen) {
                // handle `{ ... }` const expression
                let brace_open = par.next()?;
                let expr = parse_expr(par)?;
                if !matches!(par.peek(), Some(TokenKind::BraceClose)) {
                    par.error(brace_open.span, "missing closing delimiter `}`");
                    return Err(ParseFailed);
                }
                let brace_close = par.assert(TokenKind::BraceClose);
                let span = brace_open.span + brace_close.span;
                Box::new(Node::new(GenericArg::ConstExpr(expr), span))
            } else {
                // handle const expression without braces
                let expr = parse_expr(par)?;
                let span = expr.span;
                Box::new(Node::new(GenericArg::ConstExpr(expr), span))
            };

            let rbracket = par.assert(TokenKind::BracketClose);
            let span = lbracket.span + rbracket.span;
            Ok(Node::new(
                Expr::Repeat {
                    value: Box::new(elts[0].clone()),
                    len,
                },
                span,
            ))
        } else {
            par.error(lbracket.span, "expected `]` or `;`");
            Err(ParseFailed)
        }
    } else {
        let rbracket = par.assert(TokenKind::BracketClose);
        let span = lbracket.span + rbracket.span;
        Ok(Node::new(Expr::List { elts }, span))
    }
}

/// Parse a paren-wrapped expression, which might turn out to be a tuple
/// if it contains commas.
fn parse_group_or_tuple(par: &mut Parser) -> ParseResult<Node<Expr>> {
    use TokenKind::*;
    let lparen = par.assert(ParenOpen);
    if par.peek_or_err()? == ParenClose {
        let rparen = par.next()?;
        let span = lparen.span + rparen.span;
        return Ok(Node::new(Expr::Unit, span));
    }

    let elem = parse_expr(par)?;
    match par.peek_or_err()? {
        ParenClose => {
            // expr wrapped in parens
            let rparen = par.next()?;
            let span = lparen.span + rparen.span;
            Ok(Node::new(elem.kind, span))
        }
        Comma => {
            // tuple
            par.next()?;
            let elts = parse_expr_list(par, &[ParenClose], Some(elem))?;
            let rparen = par.expect(ParenClose, "failed to parse tuple expression")?;
            let span = lparen.span + rparen.span;
            Ok(Node::new(Expr::Tuple { elts }, span))
        }
        _ => {
            let tok = par.next()?;
            par.unexpected_token_error(
                &tok,
                "Unexpected token while parsing expression in parentheses",
                vec![],
            );
            Err(ParseFailed)
        }
    }
}

/// Parse some number of comma-separated expressions, until `end_marker` is
/// `peek()`ed.
fn parse_expr_list(
    par: &mut Parser,
    end_markers: &[TokenKind],
    head: Option<Node<Expr>>,
) -> ParseResult<Vec<Node<Expr>>> {
    let mut elts = vec![];
    if let Some(elem) = head {
        elts.push(elem);
    }
    loop {
        let next = par.peek_or_err()?;
        if end_markers.contains(&next) {
            break;
        }
        elts.push(parse_expr(par)?);
        match par.peek_or_err()? {
            TokenKind::Comma => {
                par.next()?;
            }
            tk if end_markers.contains(&tk) => break,
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(
                    &tok,
                    "Unexpected token while parsing list of expressions",
                    vec![],
                );
                return Err(ParseFailed);
            }
        }
    }

    Ok(elts)
}

/* node building utils */

/// Create an "atom" expr from the given `Token` (`Name`, `Num`, `Bool`, etc)
fn atom(par: &mut Parser, tok: &Token) -> Node<Expr> {
    use TokenKind::*;

    let expr = match tok.kind {
        Name | SelfValue => Expr::Name(tok.text.into()),
        Int | Hex | Octal | Binary => Expr::Num(tok.text.into()),
        True | False => Expr::Bool(tok.kind == True),
        Text => {
            if let Some(string) = unescape_string(tok.text) {
                Expr::Str(string.into())
            } else {
                par.error(tok.span, "String contains an invalid escape sequence");
                Expr::Str(tok.text.into())
            }
        }
        _ => panic!("Unexpected atom token: {tok:?}"),
    };
    Node::new(expr, tok.span)
}

fn unescape_string(quoted_string: &str) -> Option<String> {
    let inner = &quoted_string[1..quoted_string.len() - 1];
    unescape::unescape(inner)
}

/// Create an expr from the given infix operator and operands.
fn infix_op(
    par: &mut Parser,
    left: Node<Expr>,
    op: &Token,
    right: Node<Expr>,
) -> ParseResult<Node<Expr>> {
    use TokenKind::*;
    let expr = match op.kind {
        Or | And => bool_op(left, op, right),

        Amper | Hat | Pipe | LtLt | GtGt | Plus | Minus | Star | Slash | Percent | StarStar => {
            bin_op(left, op, right)
        }

        Lt | LtEq | Gt | GtEq | NotEq | EqEq => comp_op(left, op, right),

        Dot => {
            let span = left.span + right.span;
            match right.kind {
                Expr::Name(name) => Node::new(
                    Expr::Attribute {
                        value: Box::new(left),
                        attr: Node::new(name, right.span),
                    },
                    span,
                ),
                Expr::Num(_num) => {
                    par.error(span, "floats not supported");
                    return Err(ParseFailed);
                }
                Expr::Call {
                    func,
                    generic_args,
                    args,
                } => {
                    let func_span = left.span + func.span;
                    let func = Box::new(Node::new(
                        Expr::Attribute {
                            value: Box::new(left),
                            attr: {
                                if let Expr::Name(name) = func.kind {
                                    Node::new(name, func.span)
                                } else {
                                    par.fancy_error(
                                        "failed to parse attribute expression",
                                        vec![Label::primary(func.span, "expected a name")],
                                        vec![],
                                    );
                                    return Err(ParseFailed);
                                }
                            },
                        },
                        func_span,
                    ));

                    Node::new(
                        Expr::Call {
                            func,
                            generic_args,
                            args,
                        },
                        span,
                    )
                }
                _ => {
                    par.fancy_error(
                        "failed to parse attribute expression",
                        vec![Label::primary(right.span, "expected a name")],
                        vec![],
                    );
                    return Err(ParseFailed);
                }
            }
        }
        ColonColon => {
            let mut path = match left.kind {
                Expr::Name(name) => Path {
                    segments: vec![Node::new(name, left.span)],
                },
                Expr::Path(path) => path,
                _ => {
                    par.fancy_error(
                        "failed to parse path expression",
                        vec![
                            Label::secondary(op.span, "path delimiter".to_string()),
                            Label::primary(left.span, "expected a name"),
                        ],
                        vec![],
                    );
                    return Err(ParseFailed);
                }
            };

            // `right` can't be a Path (rbp > lbp); only valid option is `Name`
            match right.kind {
                Expr::Name(name) => {
                    path.segments.push(Node::new(name, right.span));
                    Node::new(Expr::Path(path), left.span + right.span)
                }
                _ => {
                    par.fancy_error(
                        "failed to parse path expression",
                        vec![
                            Label::secondary(op.span, "path delimiter".to_string()),
                            Label::primary(right.span, "expected a name"),
                        ],
                        vec![],
                    );
                    return Err(ParseFailed);
                }
            }
        }
        _ => panic!("Unexpected infix op token: {op:?}"),
    };
    Ok(expr)
}

/// Create an `Expr::BoolOperation` node for the given operator and operands.
fn bool_op(left: Node<Expr>, op: &Token, right: Node<Expr>) -> Node<Expr> {
    use TokenKind::*;
    let astop = match op.kind {
        And => ast::BoolOperator::And,
        Or => ast::BoolOperator::Or,
        _ => panic!(),
    };

    let span = left.span + right.span;
    Node::new(
        Expr::BoolOperation {
            left: Box::new(left),
            op: Node::new(astop, op.span),
            right: Box::new(right),
        },
        span,
    )
}

/// Create an `Expr::BinOperation` node for the given operator and operands.
fn bin_op(left: Node<Expr>, op: &Token, right: Node<Expr>) -> Node<Expr> {
    use ast::BinOperator::*;
    use TokenKind::*;

    let astop = match op.kind {
        Amper => BitAnd,
        Hat => BitXor,
        Pipe => BitOr,
        LtLt => LShift,
        GtGt => RShift,
        Plus => Add,
        Minus => Sub,
        Star => Mult,
        Slash => Div,
        Percent => Mod,
        StarStar => Pow,
        _ => panic!(),
    };

    let span = left.span + right.span;
    Node::new(
        Expr::BinOperation {
            left: Box::new(left),
            op: Node::new(astop, op.span),
            right: Box::new(right),
        },
        span,
    )
}

/// Create an `Expr::UnaryOperation` node for the given operator and operands.
fn unary_op(par: &mut Parser, op: &Token, operand: Node<Expr>) -> ParseResult<Node<Expr>> {
    use ast::UnaryOperator;
    use TokenKind::*;

    let astop = match op.kind {
        Tilde => UnaryOperator::Invert,
        Not => UnaryOperator::Not,
        Minus => UnaryOperator::USub,
        Plus => {
            par.fancy_error(
                "unary plus not supported",
                vec![Label::primary(op.span, "consider removing the '+'")],
                vec![],
            );
            return Err(ParseFailed);
        }
        _ => panic!(),
    };

    let span = op.span + operand.span;
    Ok(Node::new(
        Expr::UnaryOperation {
            op: Node::new(astop, op.span),
            operand: Box::new(operand),
        },
        span,
    ))
}

/// Create an `Expr::CompOperation` node for the given operator and operands.
fn comp_op(left: Node<Expr>, op: &Token, right: Node<Expr>) -> Node<Expr> {
    use ast::CompOperator;
    use TokenKind::*;
    let astop = match op.kind {
        In => todo!("in"), // CompOperator::In,
        Lt => CompOperator::Lt,
        LtEq => CompOperator::LtE,
        Gt => CompOperator::Gt,
        GtEq => CompOperator::GtE,
        NotEq => CompOperator::NotEq,
        EqEq => CompOperator::Eq,
        _ => panic!(),
    };
    let span = left.span + right.span;
    Node::new(
        Expr::CompOperation {
            left: Box::new(left),
            op: Node::new(astop, op.span),
            right: Box::new(right),
        },
        span,
    )
}
