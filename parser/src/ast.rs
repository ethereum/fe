use std::convert::TryFrom;

use serde::{
    Deserialize,
    Serialize,
};

use crate::span::Spanned;
use crate::tokenizer::types::Token;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Module<'a> {
    #[serde(borrow)]
    pub body: Vec<Spanned<ModuleStmt<'a>>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ModuleStmt<'a> {
    ContractDef {
        name: &'a str,
        #[serde(borrow)]
        body: Vec<Spanned<ContractStmt<'a>>>,
    },
    SimpleImport {
        #[serde(borrow)]
        names: Vec<Spanned<SimpleImportName<'a>>>,
    },
    FromImport {
        #[serde(borrow)]
        path: Spanned<FromImportPath<'a>>,
        #[serde(borrow)]
        names: Spanned<FromImportNames<'a>>,
    },
    TypeDef {
        name: &'a str,
        #[serde(borrow)]
        typ: Spanned<TypeDesc<'a>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ContractStmt<'a> {
    EventDef {
        name: &'a str,
        #[serde(borrow)]
        fields: Vec<Spanned<EventField<'a>>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ContractFieldQual {
    Const,
    Pub,
}

impl TryFrom<&Token<'_>> for Spanned<ContractFieldQual> {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, skip)]
    fn try_from(tok: &Token) -> Result<Self, Self::Error> {
        use ContractFieldQual::*;

        let span = tok.span;

        Ok(match tok.string {
            "const" => Spanned { node: Const, span },
            "pub" => Spanned { node: Pub, span },
            _ => return Err("unrecognized string"),
        })
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum EventFieldQual {
    Idx,
}

impl TryFrom<&Token<'_>> for Spanned<EventFieldQual> {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, skip)]
    fn try_from(tok: &Token) -> Result<Self, Self::Error> {
        Ok(match tok.string {
            "idx" => Spanned {
                node: EventFieldQual::Idx,
                span: tok.span,
            },
            _ => return Err("unrecognized string"),
        })
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum FuncQual {
    Pub,
}

impl TryFrom<&Token<'_>> for Spanned<FuncQual> {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, skip)]
    fn try_from(tok: &Token) -> Result<Self, Self::Error> {
        Ok(match tok.string {
            "pub" => Spanned {
                node: FuncQual::Pub,
                span: tok.span,
            },
            _ => return Err("unrecognized string"),
        })
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct SimpleImportName<'a> {
    pub path: Vec<&'a str>,
    pub alias: Option<&'a str>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum FromImportPath<'a> {
    Absolute {
        #[serde(borrow)]
        path: Vec<&'a str>,
    },
    Relative {
        parent_level: usize,
        #[serde(borrow)]
        path: Vec<&'a str>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum FromImportNames<'a> {
    Star,
    #[serde(borrow)]
    List(Vec<Spanned<FromImportName<'a>>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct FromImportName<'a> {
    pub name: &'a str,
    pub alias: Option<&'a str>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum TypeDesc<'a> {
    Base {
        base: &'a str,
    },
    Array {
        typ: Box<Spanned<TypeDesc<'a>>>,
        dimension: usize,
    },
    Map {
        from: Box<Spanned<TypeDesc<'a>>>,
        to: Box<Spanned<TypeDesc<'a>>>,
    },
}

impl<'a> From<&'a Token<'a>> for Spanned<TypeDesc<'a>> {
    fn from(token: &'a Token<'a>) -> Self {
        Spanned {
            node: TypeDesc::Base { base: token.string },
            span: token.span,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct EventField<'a> {
    pub name: &'a str,
    #[serde(borrow)]
    pub typ: Spanned<TypeDesc<'a>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Pow,
    LShift,
    RShift,
    BitOr,
    BitXor,
    BitAnd,
}

impl TryFrom<&str> for Operator {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, skip)]
    fn try_from(string: &str) -> Result<Self, Self::Error> {
        match string {
            "+" => Ok(Self::Add),
            "-" => Ok(Self::Sub),
            "*" => Ok(Self::Mult),
            "/" => Ok(Self::Div),
            "%" => Ok(Self::Mod),
            "**" => Ok(Self::Pow),
            "<<" => Ok(Self::LShift),
            ">>" => Ok(Self::RShift),
            "|" => Ok(Self::BitOr),
            "^" => Ok(Self::BitXor),
            "&" => Ok(Self::BitAnd),
            _ => Err("unrecognized binary operator string"),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum UnaryOp {
    Invert,
    Not,
    UAdd,
    USub,
}

impl TryFrom<&str> for UnaryOp {
    type Error = &'static str;

    #[cfg_attr(tarpaulin, skip)]
    fn try_from(string: &str) -> Result<Self, Self::Error> {
        match string {
            "~" => Ok(Self::Invert),
            "not" => Ok(Self::Not),
            "+" => Ok(Self::UAdd),
            "-" => Ok(Self::USub),
            _ => Err("unrecognized unary operator string"),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ConstExpr<'a> {
    BinOp {
        left: Box<Spanned<ConstExpr<'a>>>,
        op: Operator,
        right: Box<Spanned<ConstExpr<'a>>>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<Spanned<ConstExpr<'a>>>,
    },
    Name {
        name: &'a str,
    },
    Num {
        num: &'a str,
    },
}
