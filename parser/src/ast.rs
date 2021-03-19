use serde::{
    Deserialize,
    Serialize,
};

use crate::node::Node;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Module<'a> {
    #[serde(borrow)]
    pub body: Vec<Node<ModuleStmt<'a>>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ModuleStmt<'a> {
    TypeDef {
        name: Node<&'a str>,
        #[serde(borrow)]
        typ: Node<TypeDesc<'a>>,
    },
    SimpleImport {
        #[serde(borrow)]
        names: Vec<Node<SimpleImportName<'a>>>,
    },
    FromImport {
        #[serde(borrow)]
        path: Node<FromImportPath<'a>>,
        #[serde(borrow)]
        names: Node<FromImportNames<'a>>,
    },
    ContractDef {
        name: Node<&'a str>,
        #[serde(borrow)]
        body: Vec<Node<ContractStmt<'a>>>,
    },
    StructDef {
        name: Node<&'a str>,
        #[serde(borrow)]
        body: Vec<Node<StructStmt<'a>>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum TypeDesc<'a> {
    Base {
        base: &'a str,
    },
    Array {
        typ: Box<Node<TypeDesc<'a>>>,
        dimension: usize,
    },
    Map {
        from: Box<Node<TypeDesc<'a>>>,
        to: Box<Node<TypeDesc<'a>>>,
    },
    Tuple {
        items: Vec<Node<TypeDesc<'a>>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct SimpleImportName<'a> {
    #[serde(borrow)]
    pub path: Vec<Node<&'a str>>,
    pub alias: Option<Node<&'a str>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum FromImportPath<'a> {
    Absolute {
        #[serde(borrow)]
        path: Vec<Node<&'a str>>,
    },
    Relative {
        parent_level: usize,
        #[serde(borrow)]
        path: Vec<Node<&'a str>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum FromImportNames<'a> {
    Star,
    #[serde(borrow)]
    List(Vec<Node<FromImportName<'a>>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct FromImportName<'a> {
    #[serde(borrow)]
    pub name: Node<&'a str>,
    pub alias: Option<Node<&'a str>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ContractStmt<'a> {
    ContractField {
        qual: Option<Node<ContractFieldQual>>,
        #[serde(borrow)]
        name: Node<&'a str>,
        typ: Node<TypeDesc<'a>>,
    },
    EventDef {
        name: Node<&'a str>,
        fields: Vec<Node<EventField<'a>>>,
    },
    FuncDef {
        qual: Option<Node<FuncQual>>,
        name: Node<&'a str>,
        args: Vec<Node<FuncDefArg<'a>>>,
        return_type: Option<Node<TypeDesc<'a>>>,
        body: Vec<Node<FuncStmt<'a>>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum StructStmt<'a> {
    StructField {
        qual: Option<Node<StructFieldQual>>,
        #[serde(borrow)]
        name: Node<&'a str>,
        typ: Node<TypeDesc<'a>>,
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
pub struct EventField<'a> {
    pub qual: Option<Node<EventFieldQual>>,
    #[serde(borrow)]
    pub name: Node<&'a str>,
    pub typ: Node<TypeDesc<'a>>,
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
pub struct FuncDefArg<'a> {
    #[serde(borrow)]
    pub name: Node<&'a str>,
    pub typ: Node<TypeDesc<'a>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum FuncStmt<'a> {
    Return {
        #[serde(borrow)]
        value: Option<Node<Expr<'a>>>,
    },
    VarDecl {
        target: Node<Expr<'a>>,
        typ: Node<TypeDesc<'a>>,
        value: Option<Node<Expr<'a>>>,
    },
    Assign {
        targets: Vec<Node<Expr<'a>>>,
        value: Node<Expr<'a>>,
    },
    AugAssign {
        target: Node<Expr<'a>>,
        op: Node<BinOperator>,
        value: Node<Expr<'a>>,
    },
    For {
        target: Node<Expr<'a>>,
        iter: Node<Expr<'a>>,
        body: Vec<Node<FuncStmt<'a>>>,
        or_else: Vec<Node<FuncStmt<'a>>>,
    },
    While {
        test: Node<Expr<'a>>,
        body: Vec<Node<FuncStmt<'a>>>,
        or_else: Vec<Node<FuncStmt<'a>>>,
    },
    If {
        test: Node<Expr<'a>>,
        body: Vec<Node<FuncStmt<'a>>>,
        or_else: Vec<Node<FuncStmt<'a>>>,
    },
    Assert {
        test: Node<Expr<'a>>,
        msg: Option<Node<Expr<'a>>>,
    },
    Emit {
        value: Node<Expr<'a>>,
    },
    Expr {
        value: Node<Expr<'a>>,
    },
    Pass,
    Break,
    Continue,
    Revert,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    Ternary {
        if_expr: Box<Node<Expr<'a>>>,
        test: Box<Node<Expr<'a>>>,
        else_expr: Box<Node<Expr<'a>>>,
    },
    BoolOperation {
        left: Box<Node<Expr<'a>>>,
        op: Node<BoolOperator>,
        right: Box<Node<Expr<'a>>>,
    },
    BinOperation {
        left: Box<Node<Expr<'a>>>,
        op: Node<BinOperator>,
        right: Box<Node<Expr<'a>>>,
    },
    UnaryOperation {
        op: Node<UnaryOperator>,
        operand: Box<Node<Expr<'a>>>,
    },
    CompOperation {
        left: Box<Node<Expr<'a>>>,
        op: Node<CompOperator>,
        right: Box<Node<Expr<'a>>>,
    },
    Attribute {
        value: Box<Node<Expr<'a>>>,
        attr: Node<&'a str>,
    },
    Subscript {
        value: Box<Node<Expr<'a>>>,
        slices: Node<Vec<Node<Slice<'a>>>>,
    },
    Call {
        func: Box<Node<Expr<'a>>>,
        args: Node<Vec<Node<CallArg<'a>>>>,
    },
    List {
        elts: Vec<Node<Expr<'a>>>,
    },
    ListComp {
        elt: Box<Node<Expr<'a>>>,
        comps: Vec<Node<Comprehension<'a>>>,
    },
    Tuple {
        elts: Vec<Node<Expr<'a>>>,
    },
    Bool(bool),
    Name(&'a str),
    Num(&'a str),
    Str(Vec<&'a str>),
    Ellipsis,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Slice<'a> {
    Slice {
        #[serde(borrow)]
        lower: Option<Box<Node<Expr<'a>>>>,
        upper: Option<Box<Node<Expr<'a>>>>,
        step: Option<Box<Node<Expr<'a>>>>,
    },
    Index(Box<Node<Expr<'a>>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum CallArg<'a> {
    #[serde(borrow)]
    Arg(Node<Expr<'a>>),
    Kwarg(Kwarg<'a>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Kwarg<'a> {
    #[serde(borrow)]
    pub name: Node<&'a str>,
    pub value: Box<Node<Expr<'a>>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Comprehension<'a> {
    #[serde(borrow)]
    pub target: Box<Node<Expr<'a>>>,
    pub iter: Box<Node<Expr<'a>>>,
    pub ifs: Vec<Node<Expr<'a>>>,
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
