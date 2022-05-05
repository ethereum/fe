use crate::ast::Trait;
use crate::grammar::functions::parse_single_word_stmt;
use crate::node::{Node, Span};
use crate::{ParseFailed, ParseResult, Parser, TokenKind};

/// Parse a trait definition.
/// # Panics
/// Panics if the next token isn't `trait`.
pub fn parse_trait_def(par: &mut Parser, trait_pub_qual: Option<Span>) -> ParseResult<Node<Trait>> {
    let trait_tok = par.assert(TokenKind::Trait);

    // trait Event:
    //   pass
    //

    let trait_name = par.expect_with_notes(
        TokenKind::Name,
        "failed to parse trait definition",
        |_| vec!["Note: `trait` must be followed by a name, which must start with a letter and contain only letters, numbers, or underscores".into()],
    )?;

    let header_span = trait_tok.span + trait_name.span;
    par.enter_block(header_span, "trait definition")?;

    loop {
        match par.peek() {
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
                    "failed to parse trait definition body",
                    vec![],
                );
                return Err(ParseFailed);
            }
        };
    }

    let span = header_span + trait_pub_qual;
    Ok(Node::new(
        Trait {
            name: Node::new(trait_name.text.into(), trait_name.span),
            pub_qual: trait_pub_qual,
        },
        span,
    ))
}
