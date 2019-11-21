use std::convert::TryFrom;

use serde::{
    Deserialize,
    Serialize,
};

use crate::tokenizer::types::{
    Offset,
    Position,
    TokenInfo,
};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct SourceSpan {
    start_pos: Position,
    start_off: Offset,
    end_pos: Position,
    end_off: Offset,
}

impl From<&TokenInfo<'_>> for SourceSpan {
    fn from(token_info: &TokenInfo) -> Self {
        Self {
            start_pos: token_info.start_pos,
            start_off: token_info.start_off,
            end_pos: token_info.end_pos,
            end_off: token_info.end_off,
        }
    }
}

impl From<(&TokenInfo<'_>, &TokenInfo<'_>)> for SourceSpan {
    fn from(tokens: (&TokenInfo, &TokenInfo)) -> Self {
        let (start, end) = tokens;

        Self {
            start_pos: start.start_pos,
            start_off: start.start_off,
            end_pos: end.end_pos,
            end_off: end.end_off,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Module<'a> {
    #[serde(borrow)]
    pub body: Vec<ModuleStmt<'a>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ModuleStmt<'a> {
    EventDef {
        name: &'a str,
        fields: Vec<EventField<'a>>,
    },
    /* InterfaceDef {
     *     name: Name,
     *     fields: Vec<InterfaceField>,
     *     methods: Vec<InterfaceMethodDef>,
     * },
     * ContractDef {
     *     name: Name,
     *     fields: Vec<ContractField>,
     *     methods: Vec<ContractMethodDef>,
     * }, */
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct TypeDesc<'a> {
    base: &'a str,
    dimensions: Vec<u32>,
    annotations: Vec<&'a str>,
}

impl<'a> From<&'a str> for TypeDesc<'a> {
    fn from(string: &'a str) -> Self {
        Self {
            base: string,
            dimensions: vec![],
            annotations: vec![],
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct EventField<'a> {
    pub name: &'a str,
    pub typ: TypeDesc<'a>,
    //pub indexed: bool,
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
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<ConstExpr<'a>>,
    },
    Name(&'a str),
    Num(&'a str),
}
