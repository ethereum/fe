use crate::SyntaxKind;

use super::{
    define_scope, expr_atom,
    param::{CallArgListScope, GenericArgListScope},
    token_stream::{LexicalToken, TokenStream},
    Checkpoint, Parser,
};

/// Parses expression.
pub fn parse_expr<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parse_expr_with_min_bp(parser, 0, true)
}

/// Parses expression except for `struct` initialization expression.
pub fn parse_expr_no_struct<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parse_expr_with_min_bp(parser, 0, false)
}

// Expressions are parsed in Pratt's top-down operator precedence style.
// <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html>
/// Parse an expression, stopping if/when we reach an operator that binds less
/// tightly than given binding power.
///
/// Returns `true` if parsing succeeded, `false` otherwise.
fn parse_expr_with_min_bp<S: TokenStream>(
    parser: &mut Parser<S>,
    min_bp: u8,
    allow_struct_init: bool,
) -> bool {
    let (ok, checkpoint) = parse_expr_atom(parser, allow_struct_init);
    if !ok {
        return false;
    }

    loop {
        let Some(kind) = parser.current_kind() else {
            break;
        };
        // Parse postfix operators.
        match postfix_binding_power(kind) {
            Some(lbp) if lbp < min_bp => break,
            Some(_) => {
                match kind {
                    SyntaxKind::LBracket => {
                        parser.parse(IndexExprScope::default(), Some(checkpoint));
                        continue;
                    }

                    SyntaxKind::LParen => {
                        if parser.parse(CallExprScope::default(), Some(checkpoint)).0 {
                            continue;
                        }
                    }

                    // `expr<generic_param_args>()`.
                    SyntaxKind::Lt => {
                        //let is_call_expr =
                        //    parser.dry_run(|parser| parser.parse(CallExprScope::default(),
                        // None).0);
                        if is_call_expr(parser) {
                            parser.parse(CallExprScope::default(), Some(checkpoint));
                            continue;
                        }
                    }

                    // `expr.method<T, i32>()`
                    SyntaxKind::Dot => {
                        if is_method_call(parser) {
                            parser.parse(MethodExprScope::default(), Some(checkpoint));
                            continue;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            None => {}
        }

        if let Some((lbp, _)) = infix_binding_power(parser) {
            if lbp < min_bp {
                break;
            }

            if !match kind {
                // Method call is already handled as the postfix operator.
                SyntaxKind::Dot => parser.parse(FieldExprScope::default(), Some(checkpoint)).0,
                _ => {
                    // 1. Try to parse the expression as an augmented assignment expression.
                    // 2. If 1. fails, try to parse the expression as an assignment expression.
                    // 3. If 2. fails, try to parse the expression as a binary expression.
                    parser.parse(BinExprScope::default(), Some(checkpoint)).0
                }
            } {
                return false;
            }

            continue;
        }
        break;
    }

    true
}

fn parse_expr_atom<S: TokenStream>(
    parser: &mut Parser<S>,
    allow_struct_init: bool,
) -> (bool, Checkpoint) {
    match parser.current_kind() {
        Some(kind) if prefix_binding_power(kind).is_some() => {
            parser.parse(UnExprScope::default(), None)
        }
        Some(_) => expr_atom::parse_expr_atom(parser, allow_struct_init),
        None => {
            parser.error_and_recover("expected expression", None);
            (false, parser.checkpoint())
        }
    }
}

/// Specifies how tightly a prefix unary operator binds to its operand.
fn prefix_binding_power(kind: SyntaxKind) -> Option<u8> {
    use SyntaxKind::*;
    match kind {
        Not | Plus | Minus | Tilde => Some(145),
        _ => None,
    }
}

/// Specifies how tightly a postfix operator binds to its operand.
fn postfix_binding_power(kind: SyntaxKind) -> Option<u8> {
    use SyntaxKind::*;
    match kind {
        LBracket | LParen | Lt => Some(147),
        Dot => Some(151),
        _ => None,
    }
}

/// Specifies how tightly does an infix operator bind to its left and right
/// operands.
fn infix_binding_power<S: TokenStream>(parser: &mut Parser<S>) -> Option<(u8, u8)> {
    use SyntaxKind::*;

    let bp = match parser.current_kind()? {
        Pipe2 => (50, 51),
        Amp2 => (60, 61),
        NotEq | Eq2 => (70, 71),
        Lt => {
            if is_lshift(parser) {
                (110, 111)
            } else {
                // `LT` and `LtEq` has the same binding power.
                (70, 71)
            }
        }
        Gt => {
            if is_rshift(parser) {
                (110, 111)
            } else {
                // `Gt` and `GtEq` has the same binding power.
                (70, 71)
            }
        }
        Pipe => (80, 81),
        Hat => (90, 91),
        Amp => (100, 101),
        LShift | RShift => (110, 111),
        Plus | Minus => (120, 121),
        Star | Slash | Percent => {
            if is_aug(parser) {
                (11, 10)
            } else {
                (130, 131)
            }
        }
        Star2 => (141, 140),
        Dot => (151, 150),
        Eq => {
            // `Assign` and `AugAssign` have the same binding power
            (11, 10)
        }
        _ => return None,
    };

    Some(bp)
}

define_scope! { UnExprScope, UnExpr, Inheritance }
impl super::Parse for UnExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        let kind = parser.current_kind().unwrap();
        let bp = prefix_binding_power(kind).unwrap();
        parser.bump();
        parse_expr_with_min_bp(parser, bp, true);
    }
}

define_scope! { BinExprScope, BinExpr,Inheritance  }
impl super::Parse for BinExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);

        let (_, rbp) = infix_binding_power(parser).unwrap();
        if is_aug(parser) {
            self.set_kind(SyntaxKind::AugAssignExpr);
            bump_aug_assign_op(parser);
            parser.bump_expected(SyntaxKind::Eq);
            parse_expr_with_min_bp(parser, rbp, true);
        } else if is_asn(parser) {
            self.set_kind(SyntaxKind::AssignExpr);
            parser.set_newline_as_trivia(false);
            parser.bump_expected(SyntaxKind::Eq);
            parse_expr_with_min_bp(parser, rbp, true);
        } else {
            bump_bin_op(parser);
            parse_expr_with_min_bp(parser, rbp, false);
        }
    }
}

define_scope! { IndexExprScope, IndexExpr, Override(RBracket) }
impl super::Parse for IndexExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::LBracket);
        parser.with_next_expected_tokens(parse_expr, &[SyntaxKind::RBracket]);
        parser.bump_or_recover(SyntaxKind::RBracket, "expected `]`", None);
    }
}

define_scope! { CallExprScope, CallExpr, Inheritance }
impl super::Parse for CallExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.with_next_expected_tokens(
                |parser| {
                    parser.parse(GenericArgListScope::default(), None);
                },
                &[SyntaxKind::LParen],
            );
        }

        if parser.current_kind() != Some(SyntaxKind::LParen) {
            parser.error_and_recover("expected `(`", None);
            return;
        }
        parser.parse(CallArgListScope::default(), None);
    }
}

define_scope! { MethodExprScope, MethodCallExpr, Inheritance }
impl super::Parse for MethodExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Dot);

        parser.bump_or_recover(SyntaxKind::Ident, "expected identifier", None);

        parser.with_next_expected_tokens(
            |parser| {
                if parser.current_kind() == Some(SyntaxKind::Lt) {
                    parser.parse(GenericArgListScope::default(), None);
                }
            },
            &[SyntaxKind::LParen],
        );

        if parser.current_kind() != Some(SyntaxKind::LParen) {
            parser.error_and_recover("expected `(`", None);
            return;
        }
        parser.parse(CallArgListScope::default(), None);
    }
}

define_scope! { FieldExprScope, FieldExpr, Inheritance }
impl super::Parse for FieldExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        parser.bump_expected(SyntaxKind::Dot);
        match parser.current_token() {
            Some(token) if token.syntax_kind() == SyntaxKind::Ident => {
                parser.bump();
            }
            Some(token) if token.syntax_kind() == SyntaxKind::Int => {
                let text = token.text();
                if !text.chars().all(|c| c.is_ascii_digit()) {
                    parser
                        .error_and_recover("expected integer decimal literal without prefix", None);
                    return;
                }
                parser.bump();
            }
            _ => {
                parser.error_and_recover("expected identifier or integer literal", None);
            }
        }
    }
}

define_scope! { pub(super) LShiftScope, LShift, Inheritance }
impl super::Parse for LShiftScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_or_recover(SyntaxKind::Lt, "expected `<<`", None);
        parser.bump_or_recover(SyntaxKind::Lt, "expected `<<`", None);
    }
}

define_scope! { pub(super) RShiftScope, RShift, Inheritance }
impl super::Parse for RShiftScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_or_recover(SyntaxKind::Gt, "expected `>>`", None);
        parser.bump_or_recover(SyntaxKind::Gt, "expected `>>`", None);
    }
}

define_scope! { pub(super) LtEqScope, LtEq, Inheritance }
impl super::Parse for LtEqScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_or_recover(SyntaxKind::Lt, "expected `<=`", None);
        parser.bump_or_recover(SyntaxKind::Eq, "expected `<=`", None);
    }
}

define_scope! { pub(super) GtEqScope, GtEq, Inheritance }
impl super::Parse for GtEqScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_or_recover(SyntaxKind::Gt, "expected `>=`", None);
        parser.bump_or_recover(SyntaxKind::Eq, "expected `>=`", None);
    }
}

pub(crate) fn is_lshift<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| parser.parse(LShiftScope::default(), None).0)
}

fn is_aug<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| {
        if !bump_aug_assign_op(parser) {
            return false;
        }

        parser.set_newline_as_trivia(false);
        parser.current_kind() == Some(SyntaxKind::Eq)
    })
}
fn is_asn<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| {
        parser.set_newline_as_trivia(false);
        if parser.current_kind() == Some(SyntaxKind::Eq) {
            parser.bump();
            true
        } else {
            false
        }
    })
}

pub(crate) fn is_rshift<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| parser.parse(RShiftScope::default(), None).0)
}

fn is_lt_eq<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| parser.parse(LtEqScope::default(), None).0)
}

fn is_gt_eq<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| parser.parse(GtEqScope::default(), None).0)
}

fn bump_bin_op<S: TokenStream>(parser: &mut Parser<S>) {
    match parser.current_kind() {
        Some(SyntaxKind::Lt) => {
            if is_lshift(parser) {
                parser.parse(LShiftScope::default(), None);
            } else if is_lt_eq(parser) {
                parser.parse(LtEqScope::default(), None);
            } else {
                parser.bump();
            }
        }
        Some(SyntaxKind::Gt) => {
            if is_rshift(parser) {
                parser.parse(RShiftScope::default(), None);
            } else if is_gt_eq(parser) {
                parser.parse(GtEqScope::default(), None);
            } else {
                parser.bump();
            }
        }
        _ => {
            parser.bump();
        }
    }
}

fn is_call_expr<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| {
        parser.set_newline_as_trivia(false);

        let mut is_call = true;
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            is_call &= parser.parse(GenericArgListScope::default(), None).0;
        }

        if parser.current_kind() != Some(SyntaxKind::LParen) {
            false
        } else {
            is_call && parser.parse(CallArgListScope::default(), None).0
        }
    })
}

fn is_method_call<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| {
        parser.set_newline_as_trivia(false);
        if !parser.bump_if(SyntaxKind::Dot) {
            return false;
        }

        if !parser.bump_if(SyntaxKind::Ident) {
            return false;
        }

        if parser.current_kind() == Some(SyntaxKind::Lt)
            && !parser.parse(GenericArgListScope::default(), None).0
        {
            return false;
        }

        if parser.current_kind() != Some(SyntaxKind::LParen) {
            false
        } else {
            parser.parse(CallArgListScope::default(), None).0
        }
    })
}

fn bump_aug_assign_op<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    use SyntaxKind::*;
    match parser.current_kind() {
        Some(Pipe | Hat | Amp | Plus | Minus | Star | Slash | Percent | Star2) => {
            parser.bump();
            true
        }
        Some(Lt) => {
            if is_lshift(parser) {
                parser.parse(LShiftScope::default(), None);
                true
            } else {
                false
            }
        }
        Some(Gt) => {
            if is_rshift(parser) {
                parser.parse(RShiftScope::default(), None);
                true
            } else {
                false
            }
        }
        _ => false,
    }
}
