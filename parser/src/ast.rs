use std::convert::TryFrom;

use serde::{
    Deserialize,
    Serialize,
};

use crate::span::{
    GetSourceSpan,
    SourceSpan,
};
use crate::tokenizer::types::TokenInfo;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Module<'a> {
    #[serde(borrow)]
    pub body: Vec<ModuleStmt<'a>>,
    pub source_span: SourceSpan,
}

impl<'a> GetSourceSpan for Module<'a> {
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ModuleStmt<'a> {
    EventDef {
        name: &'a str,
        fields: Vec<EventField<'a>>,
        source_span: SourceSpan,
    },
}

impl<'a> GetSourceSpan for ModuleStmt<'a> {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            Self::EventDef { source_span, .. } => source_span,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TypeDesc<'a> {
    pub base: &'a str,
    pub dimensions: Vec<u32>,
    pub annotations: Vec<&'a str>,
    pub source_span: SourceSpan,
}

impl<'a> From<&'a TokenInfo<'a>> for TypeDesc<'a> {
    fn from(token_info: &'a TokenInfo<'a>) -> Self {
        Self {
            base: token_info.string,
            dimensions: vec![],
            annotations: vec![],
            source_span: token_info.source_span,
        }
    }
}

impl<'a> GetSourceSpan for TypeDesc<'a> {
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct EventField<'a> {
    pub name: &'a str,
    pub typ: TypeDesc<'a>,
    pub source_span: SourceSpan,
}

impl<'a> GetSourceSpan for EventField<'a> {
    fn get_source_span(&self) -> &SourceSpan {
        &self.source_span
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
        source_span: SourceSpan,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<ConstExpr<'a>>,
        source_span: SourceSpan,
    },
    Name {
        name: &'a str,
        source_span: SourceSpan,
    },
    Num {
        num: &'a str,
        source_span: SourceSpan,
    },
}

impl<'a> GetSourceSpan for ConstExpr<'a> {
    fn get_source_span(&self) -> &SourceSpan {
        match self {
            Self::BinOp { source_span, .. } => source_span,
            Self::UnaryOp { source_span, .. } => source_span,
            Self::Name { source_span, .. } => source_span,
            Self::Num { source_span, .. } => source_span,
        }
    }
}
