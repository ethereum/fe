use super::functions::parse_fn_def;
use super::types::{parse_field, parse_opt_qualifier};

use crate::ast::{Contract, ContractStmt};
use crate::node::{Node, Span};
use crate::{ParseFailed, ParseResult, Parser, TokenKind};

// Rule: all "statement" level parse functions consume their trailing
// newline(s), either directly or via a function they call.
// This is required to parse an `if` block, because we need to peek past the
// trailing newlines to check whether it's followed by an `else` block, and is
// done for all statements for consistency.

/// Parse a contract definition.
/// # Panics
/// Panics if the next token isn't `contract`.
pub fn parse_contract_def(
    par: &mut Parser,
    contract_pub_qual: Option<Span>,
) -> ParseResult<Node<Contract>> {
    let contract_tok = par.assert(TokenKind::Contract);
    let contract_name = par.expect_with_notes(
        TokenKind::Name,
        "failed to parse contract definition",
        |_| vec!["Note: `contract` must be followed by a name, which must start with a letter and contain only letters, numbers, or underscores".into()],
    )?;

    let mut span = contract_tok.span + contract_name.span;
    par.enter_block(span, "contract definition")?;

    let mut fields = vec![];
    let mut defs = vec![];

    loop {
        par.eat_newlines();
        let mut pub_qual = parse_opt_qualifier(par, TokenKind::Pub);
        let const_qual = parse_opt_qualifier(par, TokenKind::Const);
        if pub_qual.is_none() && const_qual.is_some() && par.peek() == Some(TokenKind::Pub) {
            pub_qual = parse_opt_qualifier(par, TokenKind::Pub);
            par.error(
                pub_qual.unwrap() + const_qual,
                "`const pub` should be written `pub const`",
            );
        }

        match par.peek_or_err()? {
            TokenKind::Name => {
                let field = parse_field(par, vec![], pub_qual, const_qual)?;
                if !defs.is_empty() {
                    par.error(
                        field.span,
                        "contract field definitions must come before any function definitions",
                    );
                }
                fields.push(field);
            }
            TokenKind::Fn | TokenKind::Unsafe => {
                if let Some(span) = const_qual {
                    par.error(
                        span,
                        "`const` qualifier can't be used with function definitions",
                    );
                }
                defs.push(ContractStmt::Function(parse_fn_def(par, pub_qual)?));
            }
            TokenKind::BraceClose => {
                span += par.next()?.span;
                break;
            }
            _ => {
                let tok = par.next()?;
                par.unexpected_token_error(
                    &tok,
                    "failed to parse contract definition body",
                    vec![],
                );
                return Err(ParseFailed);
            }
        };
    }

    Ok(Node::new(
        Contract {
            name: Node::new(contract_name.text.into(), contract_name.span),
            fields,
            body: defs,
            pub_qual: contract_pub_qual,
        },
        span,
    ))
}
