use std::convert::TryFrom;

use serde::{
    Deserialize,
    Serialize,
};

use crate::span::{
    GetSpan,
    Span,
};
use crate::tokenizer::types::Token;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Module<'a> {
    #[serde(borrow)]
    pub body: Vec<ModuleStmt<'a>>,
    pub span: Span,
}

impl<'a> GetSpan for Module<'a> {
    fn get_span(&self) -> &Span {
        &self.span
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ModuleStmt<'a> {
    EventDef {
        name: &'a str,
        fields: Vec<EventField<'a>>,
        span: Span,
    },
}

impl<'a> GetSpan for ModuleStmt<'a> {
    fn get_span(&self) -> &Span {
        match self {
            Self::EventDef { span, .. } => span,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TypeDesc<'a> {
    pub base: &'a str,
    pub dimensions: Vec<u32>,
    pub annotations: Vec<&'a str>,
    pub span: Span,
}

impl<'a> From<&'a Token<'a>> for TypeDesc<'a> {
    fn from(token: &'a Token<'a>) -> Self {
        Self {
            base: token.string,
            dimensions: vec![],
            annotations: vec![],
            span: token.span,
        }
    }
}

impl<'a> GetSpan for TypeDesc<'a> {
    fn get_span(&self) -> &Span {
        &self.span
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct EventField<'a> {
    pub name: &'a str,
    pub typ: TypeDesc<'a>,
    pub span: Span,
}

impl<'a> GetSpan for EventField<'a> {
    fn get_span(&self) -> &Span {
        &self.span
    }
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
        left: Box<ConstExpr<'a>>,
        op: Operator,
        right: Box<ConstExpr<'a>>,
        span: Span,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<ConstExpr<'a>>,
        span: Span,
    },
    Name {
        name: &'a str,
        span: Span,
    },
    Num {
        num: &'a str,
        span: Span,
    },
}

impl<'a> GetSpan for ConstExpr<'a> {
    fn get_span(&self) -> &Span {
        match self {
            Self::BinOp { span, .. } => span,
            Self::UnaryOp { span, .. } => span,
            Self::Name { span, .. } => span,
            Self::Num { span, .. } => span,
        }
    }
}
