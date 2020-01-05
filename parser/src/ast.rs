use serde::{
    Deserialize,
    Serialize,
};

use crate::span::Spanned;

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
    ContractField {
        qual: Option<Spanned<ContractFieldQual>>,
        #[serde(borrow)]
        name: &'a str,
        typ: Spanned<TypeDesc<'a>>,
    },
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum EventFieldQual {
    Idx,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum FuncQual {
    Pub,
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct EventField<'a> {
    pub qual: Option<Spanned<EventFieldQual>>,
    #[serde(borrow)]
    pub name: &'a str,
    pub typ: Spanned<TypeDesc<'a>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Expr<'a> {
    Ternary {
        test: Box<Spanned<Expr<'a>>>,
        if_expr: Box<Spanned<Expr<'a>>>,
        else_expr: Box<Spanned<Expr<'a>>>,
    },
    BoolOperation {
        left: Box<Spanned<Expr<'a>>>,
        op: Spanned<BoolOperator>,
        right: Box<Spanned<Expr<'a>>>,
    },
    BinOperation {
        left: Box<Spanned<Expr<'a>>>,
        op: Spanned<BinOperator>,
        right: Box<Spanned<Expr<'a>>>,
    },
    UnaryOperation {
        op: Spanned<UnaryOperator>,
        operand: Box<Spanned<Expr<'a>>>,
    },
    CompOperation {
        left: Box<Spanned<Expr<'a>>>,
        op: Spanned<CompOperator>,
        right: Box<Spanned<Expr<'a>>>,
    },
    Attribute {
        value: Box<Spanned<Expr<'a>>>,
        attr: Spanned<&'a str>,
    },
    Subscript {
        value: Box<Spanned<Expr<'a>>>,
        slices: Spanned<Vec<Spanned<Slice<'a>>>>,
    },
    Call {
        func: Box<Spanned<Expr<'a>>>,
        args: Spanned<Vec<Spanned<CallArg<'a>>>>,
    },
    List {
        elts: Vec<Spanned<Expr<'a>>>,
    },
    ListComp {
        elt: Box<Spanned<Expr<'a>>>,
        comps: Vec<Spanned<Comprehension<'a>>>,
    },
    Tuple {
        elts: Vec<Spanned<Expr<'a>>>,
    },
    Name(&'a str),
    Num(&'a str),
    Str(Vec<&'a str>),
    Ellipsis,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Slice<'a> {
    Slice {
        #[serde(borrow)]
        lower: Option<Box<Spanned<Expr<'a>>>>,
        upper: Option<Box<Spanned<Expr<'a>>>>,
        step: Option<Box<Spanned<Expr<'a>>>>,
    },
    Index(Box<Expr<'a>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum CallArg<'a> {
    #[serde(borrow)]
    Arg(Expr<'a>),
    Kwarg(Kwarg<'a>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Kwarg<'a> {
    #[serde(borrow)]
    pub name: Spanned<&'a str>,
    pub value: Box<Spanned<Expr<'a>>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Comprehension<'a> {
    #[serde(borrow)]
    pub target: Box<Spanned<Expr<'a>>>,
    pub iter: Box<Spanned<Expr<'a>>>,
    pub ifs: Vec<Spanned<Expr<'a>>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum BoolOperator {
    And,
    Or,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum BinOperator {
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
    FloorDiv,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum UnaryOperator {
    Invert,
    Not,
    UAdd,
    USub,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum CompOperator {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
    Is,
    IsNot,
    In,
    NotIn,
}
