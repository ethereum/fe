use smol_str::SmolStr;

use crate::{node::Node, ParseResult, Parser, TokenKind};

pub fn parse_attributes(par: &mut Parser) -> ParseResult<Vec<Node<SmolStr>>> {
    let mut attributes = Vec::new();
    while let Some(TokenKind::Hash) = par.peek() {
        let attr = parse_attribute(par)?;
        attributes.push(attr);
        par.eat_newlines();
    }
    Ok(attributes)
}

// TODO: Parse `attr(arg)`.
pub fn parse_attribute(par: &mut Parser) -> ParseResult<Node<SmolStr>> {
    let start = par.assert(TokenKind::Hash);
    let attr_name = par.expect_with_notes(TokenKind::Name, "failed to parse attribute definition", |_|
                vec!["Note: an attribute name must start with a letter or underscore, and contain letters, numbers, or underscores".into()])?;
    Ok(Node::new(
        attr_name.text.into(),
        start.span + attr_name.span,
    ))
}
