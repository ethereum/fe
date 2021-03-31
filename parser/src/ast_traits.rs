use std::convert::TryFrom;

use crate::ast::*;
use crate::node::{
    Node,
    Span,
};
use crate::tokenizer::Token;

impl<'a> From<&'a Token<'a>> for Node<TypeDesc> {
    fn from(token: &'a Token<'a>) -> Self {
        Node::new(
            TypeDesc::Base {
                base: token.string.to_string(),
            },
            token.span,
        )
    }
}

impl TryFrom<&Token<'_>> for Node<BoolOperator> {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, rustfmt::skip)]
    fn try_from(tok: &Token) -> Result<Self, Self::Error> {
        use BoolOperator::*;

        let node = match tok.string {
            "and" => And,
            "or" => Or,
            _ => return Err("unrecognized token"),
        };

        Ok(Node::new(
            node,
            tok.span,
        ))
    }
}

impl TryFrom<&Token<'_>> for Node<BinOperator> {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, rustfmt::skip)]
    fn try_from(tok: &Token) -> Result<Self, Self::Error> {
        use BinOperator::*;

        let node = match tok.string {
            "+" => Add,
            "-" => Sub,
            "*" => Mult,
            "/" => Div,
            "%" => Mod,
            "**" => Pow,
            "<<" => LShift,
            ">>" => RShift,
            "|" => BitOr,
            "^" => BitXor,
            "&" => BitAnd,
            "//" => FloorDiv,
            "+=" => Add,
            "-=" => Sub,
            "*=" => Mult,
            "/=" => Div,
            "%=" => Mod,
            "**=" => Pow,
            "<<=" => LShift,
            ">>=" => RShift,
            "|=" => BitOr,
            "^=" => BitXor,
            "&=" => BitAnd,
            "//=" => FloorDiv,
            _ => return Err("unrecognized token"),
        };

        Ok(Node::new(
            node,
            tok.span,
        ))
    }
}

impl TryFrom<&Token<'_>> for Node<UnaryOperator> {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, rustfmt::skip)]
    fn try_from(tok: &Token) -> Result<Self, Self::Error> {
        use UnaryOperator::*;

        let node = match tok.string {
            "~" => Invert,
            "not" => Not,
            "+" => UAdd,
            "-" => USub,
            _ => return Err("unrecognized string"),
        };

        Ok(Node::new(
            node,
            tok.span,
        ))
    }
}

impl TryFrom<&[&Token<'_>]> for Node<CompOperator> {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, rustfmt::skip)]
    fn try_from(toks: &[&Token]) -> Result<Self, Self::Error> {
        use CompOperator::*;

        let tok_strings: Vec<_> = toks.iter().map(|t| t.string).collect();

        let node = match &tok_strings[..] {
            ["=="] => Eq,
            ["!="] => NotEq,
            ["<"] => Lt,
            ["<="] => LtE,
            [">"] => Gt,
            [">="] => GtE,
            ["is"] => Is,
            ["is", "not"] => IsNot,
            ["in"] => In,
            ["not", "in"] => NotIn,
            _ => return Err("unrecognized strings"),
        };

        let first = toks.first().unwrap();
        let last = toks.last().unwrap();
        let span = Span::from_pair(*first, *last);

        Ok(Node::new(node, span))
    }
}
