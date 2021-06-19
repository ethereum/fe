use super::functions::parse_fn_def;
use super::types::{parse_event_def, parse_field, parse_opt_qualifier};

use crate::ast::{Contract, ContractStmt};
use crate::grammar::functions::parse_single_word_stmt;
use crate::node::Node;
use crate::{ParseFailed, ParseResult, Parser, TokenKind};

// Rule: all "statement" level parse functions consume their trailing
// newline(s), either directly or via a function they call.
// This is required to parse an `if` block, because we need to peek past the
// trailing newlines to check whether it's followed by an `else` block, and is
// done for all statements for consistency.

/// Parse a contract definition.
/// # Panics
/// Panics if the next token isn't `contract`.
pub fn parse_contract_def(par: &mut Parser) -> ParseResult<Node<Contract>> {
    let contract_tok = par.assert(TokenKind::Contract);

    // contract Foo:
    //   x: Map<address, u256>
    //   pub y: u8
    //   const z: u256 = 10
    //
    //   event Sent:
    //     idx sender: address
    //     val: u256
    //
    //   pub def foo() -> address:
    //     return abc
    //

    let contract_name = par.expect_with_notes(
        TokenKind::Name,
        "failed to parse contract definition",
        || vec!["Note: `contract` must be followed by a name, which must start with a letter and contain only letters, numbers, or underscores".into()],
    )?;

    let header_span = contract_tok.span + contract_name.span;
    par.enter_block(header_span, "contract definition")?;

    let mut fields = vec![];
    let mut defs = vec![];

    loop {
        let mut pub_qual = parse_opt_qualifier(par, TokenKind::Pub);
        let const_qual = parse_opt_qualifier(par, TokenKind::Const);
        if pub_qual.is_none() && const_qual.is_some() && par.peek() == Some(TokenKind::Pub) {
            pub_qual = parse_opt_qualifier(par, TokenKind::Pub);
            par.error(
                pub_qual.unwrap() + const_qual,
                "`const pub` should be written `pub const`",
            );
        }

        match par.peek() {
            Some(TokenKind::Name) => {
                let field = parse_field(par, pub_qual, const_qual)?;
                if !defs.is_empty() {
                    par.error(field.span, "contract field definitions must come before any function or event definitions");
                }
                fields.push(field);
            }
            Some(TokenKind::Def) => {
                if let Some(span) = const_qual {
                    par.error(
                        span,
                        "`const` qualifier can't be used with function definitions",
                    );
                }
                defs.push(ContractStmt::Function(parse_fn_def(par, pub_qual)?));
            }
            Some(TokenKind::Event) => {
                if let Some(span) = pub_qual {
                    par.error(span, "`pub` qualifier can't be used with event definitions");
                }
                if let Some(span) = const_qual {
                    par.error(
                        span,
                        "`const` qualifier can't be used with event definitions",
                    );
                }
                defs.push(ContractStmt::Event(parse_event_def(par)?));
            }
            Some(TokenKind::Pass) => {
                parse_single_word_stmt(par)?;
            }
            Some(TokenKind::Dedent) => {
                par.next()?;
                break;
            }
            None => break,
            Some(_) => {
                let tok = par.next()?;
                par.unexpected_token_error(
                    tok.span,
                    "failed to parse contract definition body",
                    vec![],
                );
                return Err(ParseFailed);
            }
        };
    }

    let span = header_span + fields.last() + defs.last();
    Ok(Node::new(
        Contract {
            name: Node::new(contract_name.text.to_string(), contract_name.span),
            fields,
            body: defs,
        },
        span,
    ))
}
