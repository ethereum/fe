use crate::SyntaxKind;

use super::{
    define_scope, expr_atom,
    param::{CallArgListScope, GenericArgListScope},
    token_stream::{SyntaxToken, TokenStream},
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
            break
        };

        // Parse postfix operators.
        match postfix_binding_power(kind) {
            Some(lbp) if lbp < min_bp => break,
            Some(_) => {
                match kind {
                    SyntaxKind::LBracket => {
                        if parser.parse(IndexExprScope::default(), Some(checkpoint)).0 {
                            continue;
                        } else {
                            return false;
                        }
                    }

                    SyntaxKind::LParen => {
                        if parser.parse(CallExprScope::default(), Some(checkpoint)).0 {
                            continue;
                        } else {
                            return false;
                        }
                    }

                    // `expr<generic_param_args>()`.
                    SyntaxKind::Lt => {
                        let is_call_expr =
                            parser.dry_run(|parser| parser.parse(CallExprScope::default(), None).0);
                        if is_call_expr {
                            parser.parse(CallExprScope::default(), Some(checkpoint));
                            continue;
                        }
                    }

                    // `expr.method<T, i32>()`
                    SyntaxKind::Dot => {
                        let is_method_call = parser
                            .dry_run(|parser| parser.parse(MethodExprScope::default(), None).0);
                        if is_method_call {
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
                _ => parser.parse(BinExprScope::default(), Some(checkpoint)).0,
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
        Star | Slash | Percent => (130, 131),
        Star2 => (141, 140),
        Dot => (151, 150),
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

define_scope! { BinExprScope, BinExpr, Inheritance }
impl super::Parse for BinExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);

        let (_, rbp) = infix_binding_power(parser).unwrap();
        bump_bin_op(parser);

        parse_expr_with_min_bp(parser, rbp, true);
    }
}

define_scope! { IndexExprScope, IndexExpr, Override(RBracket) }
impl super::Parse for IndexExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.bump_expected(SyntaxKind::LBracket);
        parse_expr(parser);
        if !parser.bump_if(SyntaxKind::RBracket) {
            parser.error_and_recover("expected `]`", None);
        }
    }
}

define_scope! { CallExprScope, CallExpr, Inheritance }
impl super::Parse for CallExprScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        parser.set_newline_as_trivia(false);
        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default(), None);
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

        if !parser.bump_if(SyntaxKind::Ident) {
            parser.error_and_recover("expected identifier", None);
        }

        if parser.current_kind() == Some(SyntaxKind::Lt) {
            parser.parse(GenericArgListScope::default(), None);
        }

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
        if !parser.bump_if(SyntaxKind::Lt) {
            parser.error_and_recover("expected `<<`", None);
        }
        if !parser.bump_if(SyntaxKind::Lt) {
            parser.error_and_recover("expected `<<`", None);
        }
    }
}

define_scope! { pub(super) RShiftScope, RShift, Inheritance }
impl super::Parse for RShiftScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.bump_if(SyntaxKind::Gt) {
            parser.error_and_recover("expected `>>`", None);
        }
        if !parser.bump_if(SyntaxKind::Gt) {
            parser.error_and_recover("expected `>>`", None);
        }
    }
}

define_scope! { pub(super) LtEqScope, LtEq, Inheritance }
impl super::Parse for LtEqScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.bump_if(SyntaxKind::Lt) {
            parser.error_and_recover("expected `<=`", None);
        }
        if !parser.bump_if(SyntaxKind::Eq) {
            parser.error_and_recover("expected `<=`", None);
        }
    }
}

define_scope! { pub(super) GtEqScope, GtEq, Inheritance }
impl super::Parse for GtEqScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) {
        if !parser.bump_if(SyntaxKind::Gt) {
            parser.error_and_recover("expected `>=`", None);
        }
        if !parser.bump_if(SyntaxKind::Eq) {
            parser.error_and_recover("expected `>=`", None);
        }
    }
}

pub(crate) fn is_lshift<S: TokenStream>(parser: &mut Parser<S>) -> bool {
    parser.dry_run(|parser| parser.parse(LShiftScope::default(), None).0)
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
