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
    TypeDef {
        name: Spanned<&'a str>,
        #[serde(borrow)]
        typ: Spanned<TypeDesc<'a>>,
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
    ContractDef {
        name: Spanned<&'a str>,
        #[serde(borrow)]
        body: Vec<Spanned<ContractStmt<'a>>>,
    },
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
pub struct SimpleImportName<'a> {
    #[serde(borrow)]
    pub path: Vec<Spanned<&'a str>>,
    pub alias: Option<Spanned<&'a str>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum FromImportPath<'a> {
    Absolute {
        #[serde(borrow)]
        path: Vec<Spanned<&'a str>>,
    },
    Relative {
        parent_level: usize,
        #[serde(borrow)]
        path: Vec<Spanned<&'a str>>,
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
    #[serde(borrow)]
    pub name: Spanned<&'a str>,
    pub alias: Option<Spanned<&'a str>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ContractStmt<'a> {
    ContractField {
        qual: Option<Spanned<ContractFieldQual>>,
        #[serde(borrow)]
        name: Spanned<&'a str>,
        typ: Spanned<TypeDesc<'a>>,
    },
    EventDef {
        name: Spanned<&'a str>,
        fields: Vec<Spanned<EventField<'a>>>,
    },
    FuncDef {
        qual: Option<Spanned<FuncQual>>,
        name: Spanned<&'a str>,
        args: Vec<Spanned<FuncDefArg<'a>>>,
        return_type: Option<Spanned<TypeDesc<'a>>>,
        body: Vec<Spanned<FuncStmt<'a>>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum ContractFieldQual {
    Const,
    Pub,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct EventField<'a> {
    pub qual: Option<Spanned<EventFieldQual>>,
    #[serde(borrow)]
    pub name: Spanned<&'a str>,
    pub typ: Spanned<TypeDesc<'a>>,
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
pub struct FuncDefArg<'a> {
    #[serde(borrow)]
    pub name: Spanned<&'a str>,
    pub typ: Spanned<TypeDesc<'a>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum FuncStmt<'a> {
    Return {
        #[serde(borrow)]
        value: Option<Spanned<Expr<'a>>>,
    },
    VarDecl {
        target: Spanned<Expr<'a>>,
        typ: Spanned<TypeDesc<'a>>,
        value: Option<Spanned<Expr<'a>>>,
    },
    Assign {
        targets: Vec<Spanned<Expr<'a>>>,
        value: Spanned<Expr<'a>>,
    },
    AugAssign {
        target: Spanned<Expr<'a>>,
        op: Spanned<BinOperator>,
        value: Spanned<Expr<'a>>,
    },
    For {
        target: Spanned<Expr<'a>>,
        iter: Spanned<Expr<'a>>,
        body: Vec<Spanned<FuncStmt<'a>>>,
        or_else: Vec<Spanned<FuncStmt<'a>>>,
    },
    While {
        test: Spanned<Expr<'a>>,
        body: Vec<Spanned<FuncStmt<'a>>>,
        or_else: Vec<Spanned<FuncStmt<'a>>>,
    },
    If {
        test: Spanned<Expr<'a>>,
        body: Vec<Spanned<FuncStmt<'a>>>,
        or_else: Vec<Spanned<FuncStmt<'a>>>,
    },
    Assert {
        test: Spanned<Expr<'a>>,
        msg: Option<Spanned<Expr<'a>>>,
    },
    Emit {
        value: Spanned<Expr<'a>>,
    },
    Expr {
        value: Expr<'a>,
    },
    Pass,
    Break,
    Continue,
    Revert,
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
