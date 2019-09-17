use std::convert::TryFrom;

pub type Name = String;
pub type Type = String;

#[derive(Debug, PartialEq)]
pub struct Module {
    pub body: Vec<ModuleStmt>,
}

#[derive(Debug, PartialEq)]
pub enum ModuleStmt {
    EventDef { name: Name, fields: Vec<EventField> },
    //InterfaceDef {
    //    name: Name,
    //    fields: Vec<InterfaceField>,
    //    methods: Vec<InterfaceMethodDef>,
    //},
    //ContractDef {
    //    name: Name,
    //    fields: Vec<ContractField>,
    //    methods: Vec<ContractMethodDef>,
    //},
}

#[derive(Debug, PartialEq)]
pub struct TypeDesc {
    base: String,
    dimensions: Vec<u32>,
    annotations: Vec<String>,
}

impl From<&str> for TypeDesc {
    fn from(string: &str) -> Self {
        Self {
            base: string.into(),
            dimensions: vec![],
            annotations: vec![],
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct EventField {
    pub name: Name,
    pub typ: TypeDesc,
    //pub indexed: bool,
}

#[derive(Debug, PartialEq)]
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
            _ => Err("unrecognized operator string"),
        }
    }
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum ConstExpr {
    BinOp {
        left: Box<ConstExpr>,
        op: Operator,
        right: Box<ConstExpr>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<ConstExpr>,
    },
    Name(String),
    Num(String),
}
