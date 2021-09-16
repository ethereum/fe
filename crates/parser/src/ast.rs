use crate::node::Node;
use fe_common::{Span, Spanned};
use indenter::indented;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Formatter;
use std::fmt::Write;
use vec1::Vec1;

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Module {
    pub body: Vec<ModuleStmt>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum ModuleStmt {
    Pragma(Node<Pragma>),
    Import(Node<Import>),
    TypeAlias(Node<TypeAlias>),
    Contract(Node<Contract>),
    Struct(Node<Struct>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Pragma {
    pub version_requirement: Node<String>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum Import {
    Simple {
        names: Vec<Node<SimpleImportName>>,
    },
    From {
        path: Node<FromImportPath>,
        names: Node<FromImportNames>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypeAlias {
    pub name: Node<String>,
    pub typ: Node<TypeDesc>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Contract {
    pub name: Node<String>,
    pub fields: Vec<Node<Field>>,
    pub body: Vec<ContractStmt>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Struct {
    pub name: Node<String>,
    pub fields: Vec<Node<Field>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum TypeDesc {
    Unit,
    Base {
        base: String,
    },
    Array {
        typ: Box<Node<TypeDesc>>,
        dimension: usize,
    },
    Tuple {
        items: Vec1<Node<TypeDesc>>,
    },
    Generic {
        base: Node<String>,
        args: Node<Vec<GenericArg>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum GenericArg {
    TypeDesc(Node<TypeDesc>),
    Int(Node<usize>),
}

impl Spanned for GenericArg {
    fn span(&self) -> Span {
        match self {
            GenericArg::TypeDesc(node) => node.span,
            GenericArg::Int(node) => node.span,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct SimpleImportName {
    pub path: Vec<Node<String>>,
    pub alias: Option<Node<String>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum FromImportPath {
    Absolute {
        path: Vec<Node<String>>,
    },
    Relative {
        parent_level: usize,
        path: Vec<Node<String>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum FromImportNames {
    Star,
    List(Vec<Node<FromImportName>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct FromImportName {
    pub name: Node<String>,
    pub alias: Option<Node<String>>,
}

/// struct or contract field, with optional 'pub' and 'const' qualifiers
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Field {
    pub is_pub: bool,
    pub is_const: bool,
    pub name: Node<String>,
    pub typ: Node<TypeDesc>,
    pub value: Option<Node<Expr>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum ContractStmt {
    Event(Node<Event>),
    Function(Node<Function>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Event {
    pub name: Node<String>,
    pub fields: Vec<Node<EventField>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function {
    pub is_pub: bool,
    pub name: Node<String>,
    pub args: Vec<Node<FunctionArg>>,
    pub return_type: Option<Node<TypeDesc>>,
    pub body: Vec<Node<FuncStmt>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct EventField {
    pub is_idx: bool,
    pub name: Node<String>,
    pub typ: Node<TypeDesc>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct RegularFunctionArg {
    pub name: Node<String>,
    pub typ: Node<TypeDesc>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum FunctionArg {
    Regular(RegularFunctionArg),
    Zelf,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum FuncStmt {
    Return {
        value: Option<Node<Expr>>,
    },
    VarDecl {
        target: Node<VarDeclTarget>,
        typ: Node<TypeDesc>,
        value: Option<Node<Expr>>,
    },
    Assign {
        target: Node<Expr>,
        value: Node<Expr>,
    },
    AugAssign {
        target: Node<Expr>,
        op: Node<BinOperator>,
        value: Node<Expr>,
    },
    For {
        target: Node<String>,
        iter: Node<Expr>,
        body: Vec<Node<FuncStmt>>,
    },
    While {
        test: Node<Expr>,
        body: Vec<Node<FuncStmt>>,
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
        name: Node<String>,
        args: Node<Vec<Node<CallArg>>>,
    },
    Expr {
        value: Node<Expr>,
    },
    Pass,
    Break,
    Continue,
    Revert {
        error: Option<Node<Expr>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum VarDeclTarget {
    Name(String),
    Tuple(Vec<Node<VarDeclTarget>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
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
        index: Box<Node<Expr>>,
    },
    Call {
        func: Box<Node<Expr>>,
        generic_args: Option<Node<Vec<GenericArg>>>,
        args: Node<Vec<Node<CallArg>>>,
    },
    List {
        elts: Vec<Node<Expr>>,
    },
    Tuple {
        elts: Vec<Node<Expr>>,
    },
    Bool(bool),
    Name(String),
    Num(String),
    Str(String),
    Unit,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct CallArg {
    pub label: Option<Node<String>>,
    pub value: Node<Expr>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum BoolOperator {
    And,
    Or,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
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
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum UnaryOperator {
    Invert,
    Not,
    USub,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum CompOperator {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
}

impl Node<Contract> {
    pub fn name(&self) -> &str {
        &self.kind.name.kind
    }
}

impl Node<Struct> {
    pub fn name(&self) -> &str {
        &self.kind.name.kind
    }
}

impl Node<Event> {
    pub fn name(&self) -> &str {
        &self.kind.name.kind
    }
}

impl Node<Field> {
    pub fn name(&self) -> &str {
        &self.kind.name.kind
    }
}

impl Node<Function> {
    pub fn name(&self) -> &str {
        &self.kind.name.kind
    }
}

impl Node<FunctionArg> {
    pub fn name(&self) -> &str {
        match &self.kind {
            FunctionArg::Regular(arg) => &arg.name.kind,
            FunctionArg::Zelf => "self",
        }
    }
}

impl Node<TypeAlias> {
    pub fn name(&self) -> &str {
        &self.kind.name.kind
    }
}

impl Spanned for ModuleStmt {
    fn span(&self) -> Span {
        match self {
            ModuleStmt::Pragma(inner) => inner.span,
            ModuleStmt::Import(inner) => inner.span,
            ModuleStmt::TypeAlias(inner) => inner.span,
            ModuleStmt::Contract(inner) => inner.span,
            ModuleStmt::Struct(inner) => inner.span,
        }
    }
}

impl Spanned for ContractStmt {
    fn span(&self) -> Span {
        match self {
            ContractStmt::Event(inner) => inner.span,
            ContractStmt::Function(inner) => inner.span,
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", double_line_joined(&self.body))
    }
}

impl fmt::Display for ModuleStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ModuleStmt::Pragma(node) => write!(f, "{}", node.kind),
            ModuleStmt::Import(node) => write!(f, "{}", node.kind),
            ModuleStmt::TypeAlias(node) => write!(f, "{}", node.kind),
            ModuleStmt::Contract(node) => write!(f, "{}", node.kind),
            ModuleStmt::Struct(node) => write!(f, "{}", node.kind),
        }
    }
}

impl fmt::Display for Pragma {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "pragma {}", self.version_requirement.kind)
    }
}

impl fmt::Display for Import {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl fmt::Display for TypeAlias {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "type {} = {}", self.name.kind, self.typ.kind)
    }
}

impl fmt::Display for Contract {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "contract {}:", self.name.kind)?;
        if !self.fields.is_empty() {
            write!(indented(f), "{}\n\n", node_line_joined(&self.fields))?;
        }
        if !self.body.is_empty() {
            write!(indented(f), "{}", double_line_joined(&self.body))?;
        }

        Ok(())
    }
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "struct {}:", self.name.kind)?;
        if self.fields.is_empty() {
            write!(indented(f), "pass")
        } else {
            write!(indented(f), "{}", node_line_joined(&self.fields))
        }
    }
}

impl fmt::Display for TypeDesc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeDesc::Unit => write!(f, "()"),
            TypeDesc::Base { base } => write!(f, "{}", base),
            TypeDesc::Array { typ, dimension } => write!(f, "{}[{}]", typ.kind, dimension),
            TypeDesc::Tuple { items } => write!(f, "({})", node_comma_joined(items)),
            TypeDesc::Generic { base, args } => {
                write!(f, "{}<{}>", base.kind, comma_joined(&args.kind))
            }
        }
    }
}

impl fmt::Display for GenericArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GenericArg::TypeDesc(node) => write!(f, "{}", node.kind),
            GenericArg::Int(node) => write!(f, "{}", node.kind),
        }
    }
}

impl fmt::Display for SimpleImportName {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl fmt::Display for FromImportPath {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl fmt::Display for FromImportNames {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl fmt::Display for FromImportName {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_pub {
            write!(f, "pub ")?;
        }
        if self.is_const {
            write!(f, "const ")?;
        }
        write!(f, "{}: {}", self.name.kind, self.typ.kind)
    }
}

impl fmt::Display for ContractStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ContractStmt::Event(node) => write!(f, "{}", node.kind),
            ContractStmt::Function(node) => write!(f, "{}", node.kind),
        }
    }
}

impl fmt::Display for Event {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "event {}:", self.name.kind)?;
        if self.fields.is_empty() {
            write!(indented(f), "pass")
        } else {
            write!(indented(f), "{}", node_line_joined(&self.fields))
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_pub {
            write!(f, "pub ")?;
        }
        write!(
            f,
            "fn {}({})",
            self.name.kind,
            node_comma_joined(&self.args)
        )?;
        if let Some(return_type) = self.return_type.as_ref() {
            writeln!(f, " -> {}:", return_type.kind)?;
        } else {
            writeln!(f, ":")?;
        }
        write!(indented(f), "{}", node_line_joined(&self.body))
    }
}

impl fmt::Display for EventField {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_idx {
            write!(f, "idx ")?;
        }
        write!(f, "{}: {}", self.name.kind, self.typ.kind)
    }
}

impl fmt::Display for RegularFunctionArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name.kind, self.typ.kind)
    }
}

impl fmt::Display for FunctionArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FunctionArg::Regular(arg) => write!(f, "{}", arg),
            FunctionArg::Zelf => write!(f, "self"),
        }
    }
}

impl fmt::Display for FuncStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FuncStmt::Return { value } => {
                if let Some(value) = value {
                    write!(f, "return {}", value.kind)
                } else {
                    write!(f, "return")
                }
            }
            FuncStmt::VarDecl { target, typ, value } => {
                if let Some(value) = value {
                    write!(f, "let {}: {} = {}", target.kind, typ.kind, value.kind)
                } else {
                    write!(f, "let {}: {}", target.kind, typ.kind)
                }
            }
            FuncStmt::Assign { target, value } => write!(f, "{} = {}", target.kind, value.kind),
            FuncStmt::AugAssign { target, op, value } => {
                write!(f, "{} {}= {}", target.kind, op.kind, value.kind)
            }
            FuncStmt::For { target, iter, body } => {
                writeln!(f, "for {} in {}:", target.kind, iter.kind)?;
                writeln!(indented(f), "{}", node_line_joined(body))
            }
            FuncStmt::While { test, body } => {
                writeln!(f, "while {}:", test.kind)?;
                writeln!(indented(f), "{}", node_line_joined(body))
            }
            FuncStmt::If {
                test,
                body,
                or_else,
            } => {
                writeln!(f, "if {}:", test.kind)?;
                writeln!(indented(f), "{}", node_line_joined(body))?;
                if !or_else.is_empty() {
                    writeln!(f, "else {}:", test.kind)?;
                    writeln!(indented(f), "{}", node_line_joined(or_else))
                } else {
                    Ok(())
                }
            }
            FuncStmt::Assert { test, msg } => {
                if let Some(msg) = msg {
                    write!(f, "assert {}, {}", test.kind, msg.kind)
                } else {
                    write!(f, "assert {}", test.kind)
                }
            }
            FuncStmt::Emit { name, args } => {
                write!(f, "emit {}({})", name.kind, node_comma_joined(&args.kind))
            }
            FuncStmt::Expr { value } => write!(f, "{}", value.kind),
            FuncStmt::Pass => write!(f, "pass"),
            FuncStmt::Break => write!(f, "break"),
            FuncStmt::Continue => write!(f, "continue"),
            FuncStmt::Revert { error } => {
                if let Some(error) = error {
                    write!(f, "revert {}", error.kind)
                } else {
                    write!(f, "revert")
                }
            }
        }
    }
}

impl fmt::Display for VarDeclTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            VarDeclTarget::Name(name) => write!(f, "{}", name),
            VarDeclTarget::Tuple(elts) => write!(f, "{}", node_comma_joined(elts)),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ternary {
                if_expr,
                test,
                else_expr,
            } => write!(
                f,
                "{} if {} else {}",
                if_expr.kind, test.kind, else_expr.kind
            ),
            Expr::BoolOperation { left, op, right } => {
                let left = maybe_fmt_left_with_parens(&op.kind, &left.kind);
                let right = maybe_fmt_right_with_parens(&op.kind, &right.kind);
                write!(f, "{} {} {}", left, op.kind, right)
            }
            Expr::BinOperation { left, op, right } => {
                let left = maybe_fmt_left_with_parens(&op.kind, &left.kind);
                let right = maybe_fmt_right_with_parens(&op.kind, &right.kind);
                write!(f, "{} {} {}", left, op.kind, right)
            }
            Expr::UnaryOperation { op, operand } => {
                let operand = maybe_fmt_operand_with_parens(&op.kind, &operand.kind);
                if op.kind == UnaryOperator::Not {
                    write!(f, "{} {}", op.kind, operand)
                } else {
                    write!(f, "{}{}", op.kind, operand)
                }
            }
            Expr::CompOperation { left, op, right } => {
                let left = maybe_fmt_left_with_parens(&op.kind, &left.kind);
                let right = maybe_fmt_right_with_parens(&op.kind, &right.kind);
                write!(f, "{} {} {}", left, op.kind, right)
            }
            Expr::Attribute { value, attr } => write!(f, "{}.{}", value.kind, attr.kind),
            Expr::Subscript { value, index } => write!(f, "{}[{}]", value.kind, index.kind),
            Expr::Call {
                func,
                generic_args,
                args,
            } => {
                write!(f, "{}", func.kind)?;
                if let Some(generic_args) = generic_args {
                    write!(f, "<{}>", comma_joined(&generic_args.kind))?;
                }
                write!(f, "({})", node_comma_joined(&args.kind))
            }
            Expr::List { elts } => write!(f, "[{}]", node_comma_joined(elts)),
            Expr::Tuple { elts } => write!(f, "({})", node_comma_joined(elts)),
            Expr::Bool(bool) => write!(f, "{}", bool),
            Expr::Name(name) => write!(f, "{}", name),
            Expr::Num(num) => write!(f, "{}", num),
            Expr::Str(str) => write!(f, "\"{}\"", str),
            Expr::Unit => write!(f, "()"),
        }
    }
}

impl fmt::Display for CallArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(label) = self.label.as_ref() {
            write!(f, "{}={}", label.kind, self.value.kind)
        } else {
            write!(f, "{}", self.value.kind)
        }
    }
}

impl fmt::Display for BoolOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BoolOperator::*;
        match self {
            And => write!(f, "and"),
            Or => write!(f, "or"),
        }
    }
}

impl fmt::Display for BinOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinOperator::*;
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mult => write!(f, "*"),
            Div => write!(f, "/"),
            Mod => write!(f, "%"),
            Pow => write!(f, "**"),
            LShift => write!(f, "<<"),
            RShift => write!(f, ">>"),
            BitOr => write!(f, "|"),
            BitXor => write!(f, "^"),
            BitAnd => write!(f, "&"),
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UnaryOperator::*;
        match self {
            Invert => write!(f, "~"),
            Not => write!(f, "not"),
            USub => write!(f, "-"),
        }
    }
}

impl fmt::Display for CompOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CompOperator::*;
        match self {
            Eq => write!(f, "=="),
            NotEq => write!(f, "!="),
            Lt => write!(f, "<"),
            LtE => write!(f, "<="),
            Gt => write!(f, ">"),
            GtE => write!(f, ">="),
        }
    }
}

fn node_comma_joined(nodes: &[Node<impl fmt::Display>]) -> String {
    comma_joined(&nodes.iter().map(|node| &node.kind).collect::<Vec<_>>())
}

fn comma_joined(items: &[impl fmt::Display]) -> String {
    items
        .iter()
        .map(|item| format!("{}", item))
        .collect::<Vec<_>>()
        .join(", ")
}

fn node_line_joined(nodes: &[Node<impl fmt::Display>]) -> String {
    line_joined(&nodes.iter().map(|node| &node.kind).collect::<Vec<_>>())
}

fn line_joined(items: &[impl fmt::Display]) -> String {
    items
        .iter()
        .map(|item| format!("{}", item))
        .collect::<Vec<_>>()
        .join("\n")
}

fn double_line_joined(items: &[impl fmt::Display]) -> String {
    items
        .iter()
        .map(|item| format!("{}", item))
        .collect::<Vec<_>>()
        .join("\n\n")
}

trait InfixBindingPower {
    fn infix_binding_power(&self) -> (u8, u8);
}

trait PrefixBindingPower {
    fn prefix_binding_power(&self) -> u8;
}

impl InfixBindingPower for BoolOperator {
    fn infix_binding_power(&self) -> (u8, u8) {
        use BoolOperator::*;

        match self {
            Or => (50, 51),
            And => (60, 61),
        }
    }
}

impl InfixBindingPower for BinOperator {
    fn infix_binding_power(&self) -> (u8, u8) {
        use BinOperator::*;

        match self {
            Add | Sub => (120, 121),
            Mult | Div | Mod => (130, 131),
            Pow => (141, 140),
            LShift | RShift => (110, 111),
            BitOr => (80, 81),
            BitXor => (90, 91),
            BitAnd => (100, 101),
        }
    }
}

impl InfixBindingPower for CompOperator {
    fn infix_binding_power(&self) -> (u8, u8) {
        (70, 71)
    }
}

impl PrefixBindingPower for UnaryOperator {
    fn prefix_binding_power(&self) -> u8 {
        use UnaryOperator::*;

        match self {
            Not => 65,
            Invert | USub => 135,
        }
    }
}

fn maybe_fmt_left_with_parens(op: &impl InfixBindingPower, expr: &Expr) -> String {
    if expr_right_binding_power(expr) < op.infix_binding_power().0 {
        format!("({})", expr)
    } else {
        format!("{}", expr)
    }
}

fn maybe_fmt_right_with_parens(op: &impl InfixBindingPower, expr: &Expr) -> String {
    if op.infix_binding_power().1 > expr_left_binding_power(expr) {
        format!("({})", expr)
    } else {
        format!("{}", expr)
    }
}

fn maybe_fmt_operand_with_parens(op: &impl PrefixBindingPower, expr: &Expr) -> String {
    if op.prefix_binding_power() > expr_left_binding_power(expr) {
        format!("({})", expr)
    } else {
        format!("{}", expr)
    }
}

fn expr_left_binding_power(expr: &Expr) -> u8 {
    let max_power = u8::MAX;

    match expr {
        Expr::Ternary { .. } => max_power,
        Expr::BoolOperation { op, .. } => op.kind.infix_binding_power().0,
        Expr::BinOperation { op, .. } => op.kind.infix_binding_power().0,
        Expr::UnaryOperation { op, .. } => op.kind.prefix_binding_power(),
        Expr::CompOperation { op, .. } => op.kind.infix_binding_power().0,
        Expr::Attribute { .. } => max_power,
        Expr::Subscript { .. } => max_power,
        Expr::Call { .. } => max_power,
        Expr::List { .. } => max_power,
        Expr::Tuple { .. } => max_power,
        Expr::Bool(_) => max_power,
        Expr::Name(_) => max_power,
        Expr::Num(_) => max_power,
        Expr::Str(_) => max_power,
        Expr::Unit => max_power,
    }
}

fn expr_right_binding_power(expr: &Expr) -> u8 {
    let max_power = u8::MAX;

    match expr {
        Expr::Ternary { .. } => max_power,
        Expr::BoolOperation { op, .. } => op.kind.infix_binding_power().1,
        Expr::BinOperation { op, .. } => op.kind.infix_binding_power().1,
        Expr::UnaryOperation { op, .. } => op.kind.prefix_binding_power(),
        Expr::CompOperation { op, .. } => op.kind.infix_binding_power().1,
        Expr::Attribute { .. } => max_power,
        Expr::Subscript { .. } => max_power,
        Expr::Call { .. } => max_power,
        Expr::List { .. } => max_power,
        Expr::Tuple { .. } => max_power,
        Expr::Bool(_) => max_power,
        Expr::Name(_) => max_power,
        Expr::Num(_) => max_power,
        Expr::Str(_) => max_power,
        Expr::Unit => max_power,
    }
}
