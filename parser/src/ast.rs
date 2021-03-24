use serde::{
    Deserialize,
    Serialize,
};

use crate::node::Node;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Module {
    pub body: Vec<Node<ModuleStmt>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ModuleStmt {
    TypeDef {
        name: Node<String>,
        typ: Node<TypeDesc>,
    },
    SimpleImport {
        names: Vec<Node<SimpleImportName>>,
    },
    FromImport {
        path: Node<FromImportPath>,
        names: Node<FromImportNames>,
    },
    ContractDef {
        name: Node<String>,
        body: Vec<Node<ContractStmt>>,
    },
    StructDef {
        name: Node<String>,
        body: Vec<Node<StructStmt>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum TypeDesc {
    Base {
        base: String,
    },
    Array {
        typ: Box<Node<TypeDesc>>,
        dimension: usize,
    },
    Map {
        from: Box<Node<TypeDesc>>,
        to: Box<Node<TypeDesc>>,
    },
    Tuple {
        items: Vec<Node<TypeDesc>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct SimpleImportName {
    pub path: Vec<Node<String>>,
    pub alias: Option<Node<String>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum FromImportPath {
    Absolute {
        path: Vec<Node<String>>,
    },
    Relative {
        parent_level: usize,
        path: Vec<Node<String>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum FromImportNames {
    Star,
    List(Vec<Node<FromImportName>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct FromImportName {
    pub name: Node<String>,
    pub alias: Option<Node<String>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ContractStmt {
    ContractField {
        qual: Option<Node<ContractFieldQual>>,
        name: Node<String>,
        typ: Node<TypeDesc>,
    },
    EventDef {
        name: Node<String>,
        fields: Vec<Node<EventField>>,
    },
    FuncDef {
        qual: Option<Node<FuncQual>>,
        name: Node<String>,
        args: Vec<Node<FuncDefArg>>,
        return_type: Option<Node<TypeDesc>>,
        body: Vec<Node<FuncStmt>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum StructStmt {
    StructField {
        qual: Option<Node<StructFieldQual>>,
        name: Node<String>,
        typ: Node<TypeDesc>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ContractFieldQual {
    Const,
    Pub,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum StructFieldQual {
    Const,
    Pub,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct EventField {
    pub qual: Option<Node<EventFieldQual>>,
    pub name: Node<String>,
    pub typ: Node<TypeDesc>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum EventFieldQual {
    Idx,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum FuncQual {
    Pub,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct FuncDefArg {
    pub name: Node<String>,
    pub typ: Node<TypeDesc>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum FuncStmt {
    Return {
        value: Option<Node<Expr>>,
    },
    VarDecl {
        target: Node<Expr>,
        typ: Node<TypeDesc>,
        value: Option<Node<Expr>>,
    },
    Assign {
        targets: Vec<Node<Expr>>,
        value: Node<Expr>,
    },
    AugAssign {
        target: Node<Expr>,
        op: Node<BinOperator>,
        value: Node<Expr>,
    },
    For {
        target: Node<Expr>,
        iter: Node<Expr>,
        body: Vec<Node<FuncStmt>>,
        or_else: Vec<Node<FuncStmt>>,
    },
    While {
        test: Node<Expr>,
        body: Vec<Node<FuncStmt>>,
        or_else: Vec<Node<FuncStmt>>,
    },
    If {
        test: Node<Expr>,
        body: Vec<Node<FuncStmt>>,
        or_else: Vec<Node<FuncStmt>>,
    },
    Assert {
        test: Node<Expr>,
        msg: Option<Node<Expr>>,
    },
    Emit {
        value: Node<Expr>,
    },
    Expr {
        value: Node<Expr>,
    },
    Pass,
    Break,
    Continue,
    Revert,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Expr {
    Ternary {
        if_expr: Box<Node<Expr>>,
        test: Box<Node<Expr>>,
        else_expr: Box<Node<Expr>>,
    },
    BoolOperation {
        left: Box<Node<Expr>>,
        op: Node<BoolOperator>,
        right: Box<Node<Expr>>,
    },
    BinOperation {
        left: Box<Node<Expr>>,
        op: Node<BinOperator>,
        right: Box<Node<Expr>>,
    },
    UnaryOperation {
        op: Node<UnaryOperator>,
        operand: Box<Node<Expr>>,
    },
    CompOperation {
        left: Box<Node<Expr>>,
        op: Node<CompOperator>,
        right: Box<Node<Expr>>,
    },
    Attribute {
        value: Box<Node<Expr>>,
        attr: Node<String>,
    },
    Subscript {
        value: Box<Node<Expr>>,
        slices: Node<Vec<Node<Slice>>>,
    },
    Call {
        func: Box<Node<Expr>>,
        args: Node<Vec<Node<CallArg>>>,
    },
    List {
        elts: Vec<Node<Expr>>,
    },
    ListComp {
        elt: Box<Node<Expr>>,
        comps: Vec<Node<Comprehension>>,
    },
    Tuple {
        elts: Vec<Node<Expr>>,
    },
    Bool(bool),
    Name(String),
    Num(String),
    Str(Vec<String>),
    Ellipsis,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Slice {
    Slice {
        lower: Option<Box<Node<Expr>>>,
        upper: Option<Box<Node<Expr>>>,
        step: Option<Box<Node<Expr>>>,
    },
    Index(Box<Node<Expr>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum CallArg {
    Arg(Node<Expr>),
    Kwarg(Kwarg),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Kwarg {
    pub name: Node<String>,
    pub value: Box<Node<Expr>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Comprehension {
    pub target: Box<Node<Expr>>,
    pub iter: Box<Node<Expr>>,
    pub ifs: Vec<Node<Expr>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum BoolOperator {
    And,
    Or,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Invert,
    Not,
    UAdd,
    USub,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
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
