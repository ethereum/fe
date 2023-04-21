use crate::node::Node;
use fe_common::{Span, Spanned};
use indenter::indented;
use serde::{Deserialize, Serialize};
pub use smol_str::SmolStr;
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
    Use(Node<Use>),
    TypeAlias(Node<TypeAlias>),
    Contract(Node<Contract>),
    Constant(Node<ConstantDecl>),
    Struct(Node<Struct>),
    Enum(Node<Enum>),
    Trait(Node<Trait>),
    Impl(Node<Impl>),
    Function(Node<Function>),
    Attribute(Node<SmolStr>),
    ParseError(Span),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Pragma {
    pub version_requirement: Node<SmolStr>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Path {
    pub segments: Vec<Node<SmolStr>>,
}

impl Path {
    pub fn remove_last(&self) -> Path {
        Path {
            segments: self.segments[0..self.segments.len() - 1].to_vec(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Use {
    pub tree: Node<UseTree>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum UseTree {
    Glob {
        prefix: Path,
    },
    Nested {
        prefix: Path,
        children: Vec<Node<UseTree>>,
    },
    Simple {
        path: Path,
        rename: Option<Node<SmolStr>>,
    },
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct ConstantDecl {
    pub name: Node<SmolStr>,
    pub typ: Node<TypeDesc>,
    pub value: Node<Expr>,
    pub pub_qual: Option<Span>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypeAlias {
    pub name: Node<SmolStr>,
    pub typ: Node<TypeDesc>,
    pub pub_qual: Option<Span>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Contract {
    pub name: Node<SmolStr>,
    pub fields: Vec<Node<Field>>,
    pub body: Vec<ContractStmt>,
    pub pub_qual: Option<Span>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Struct {
    pub name: Node<SmolStr>,
    pub fields: Vec<Node<Field>>,
    pub functions: Vec<Node<Function>>,
    pub pub_qual: Option<Span>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Enum {
    pub name: Node<SmolStr>,
    pub variants: Vec<Node<Variant>>,
    pub functions: Vec<Node<Function>>,
    pub pub_qual: Option<Span>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Trait {
    pub name: Node<SmolStr>,
    pub functions: Vec<Node<FunctionSignature>>,
    pub pub_qual: Option<Span>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Impl {
    pub impl_trait: Node<SmolStr>,
    pub receiver: Node<TypeDesc>,
    pub functions: Vec<Node<Function>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum TypeDesc {
    Unit,
    // TODO: replace with `Name(SmolStr)`, or eliminate in favor of `Path`?
    Base {
        base: SmolStr,
    },
    Path(Path),
    Tuple {
        items: Vec1<Node<TypeDesc>>,
    },
    Generic {
        // TODO: when we support user-defined generic types,
        // this will have to be a `Path`
        base: Node<SmolStr>,
        args: Node<Vec<GenericArg>>,
    },
    SelfType,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum GenericArg {
    TypeDesc(Node<TypeDesc>),
    Int(Node<usize>),
    ConstExpr(Node<Expr>),
}

impl Spanned for GenericArg {
    fn span(&self) -> Span {
        match self {
            GenericArg::TypeDesc(node) => node.span,
            GenericArg::Int(node) => node.span,
            GenericArg::ConstExpr(node) => node.span,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum GenericParameter {
    Unbounded(Node<SmolStr>),
    Bounded {
        name: Node<SmolStr>,
        bound: Node<TypeDesc>,
    },
}

impl GenericParameter {
    pub fn name(&self) -> SmolStr {
        self.name_node().kind
    }

    pub fn name_node(&self) -> Node<SmolStr> {
        match self {
            GenericParameter::Unbounded(node) => node.clone(),
            GenericParameter::Bounded { name, .. } => name.clone(),
        }
    }
}

impl Spanned for GenericParameter {
    fn span(&self) -> Span {
        match self {
            GenericParameter::Unbounded(node) => node.span,
            GenericParameter::Bounded { name, bound } => name.span + bound.span,
        }
    }
}

/// struct or contract field, with optional 'pub' and 'const' qualifiers
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Field {
    pub is_pub: bool,
    pub is_const: bool,
    pub attributes: Vec<Node<SmolStr>>,
    pub name: Node<SmolStr>,
    pub typ: Node<TypeDesc>,
    pub value: Option<Node<Expr>>,
}

/// Enum variant definition.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Variant {
    pub name: Node<SmolStr>,
    pub kind: VariantKind,
}

/// Enum variant kind.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum VariantKind {
    /// Unit variant.
    /// E.g., `Bar` in
    ///
    /// ```fe
    /// enum Foo {
    ///     Bar
    ///     Baz(u32, i32)
    /// }
    /// ```
    Unit,

    /// Tuple variant.
    /// E.g., `Baz(u32, i32)` in
    ///
    /// ```fe
    /// enum Foo {
    ///     Bar
    ///     Baz(u32, i32)
    /// }
    /// ```
    Tuple(Vec<Node<TypeDesc>>),
}

#[allow(clippy::large_enum_variant)]
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum ContractStmt {
    Function(Node<Function>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct FunctionSignature {
    // qualifier order: `pub unsafe fn`
    pub pub_: Option<Span>,
    pub unsafe_: Option<Span>,
    pub name: Node<SmolStr>,
    pub generic_params: Node<Vec<GenericParameter>>,
    pub args: Vec<Node<FunctionArg>>,
    pub return_type: Option<Node<TypeDesc>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function {
    pub sig: Node<FunctionSignature>,
    pub body: Vec<Node<FuncStmt>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum FunctionArg {
    Regular {
        mut_: Option<Span>,
        label: Option<Node<SmolStr>>,
        name: Node<SmolStr>,
        typ: Node<TypeDesc>,
    },
    Self_ {
        mut_: Option<Span>,
    },
}
impl FunctionArg {
    pub fn label_span(&self) -> Option<Span> {
        match self {
            Self::Regular { label, .. } => label.as_ref().map(|label| label.span),
            Self::Self_ { .. } => None,
        }
    }

    pub fn name_span(&self) -> Option<Span> {
        match self {
            Self::Regular { name, .. } => Some(name.span),
            Self::Self_ { .. } => None,
        }
    }

    pub fn typ_span(&self) -> Option<Span> {
        match self {
            Self::Regular { typ, .. } => Some(typ.span),
            Self::Self_ { .. } => None,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum FuncStmt {
    Return {
        value: Option<Node<Expr>>,
    },
    VarDecl {
        mut_: Option<Span>,
        target: Node<VarDeclTarget>,
        typ: Node<TypeDesc>,
        value: Option<Node<Expr>>,
    },
    ConstantDecl {
        name: Node<SmolStr>,
        typ: Node<TypeDesc>,
        value: Node<Expr>,
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
        target: Node<SmolStr>,
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
    Match {
        expr: Node<Expr>,
        arms: Vec<Node<MatchArm>>,
    },
    Assert {
        test: Node<Expr>,
        msg: Option<Node<Expr>>,
    },
    Expr {
        value: Node<Expr>,
    },
    Break,
    Continue,
    Revert {
        error: Option<Node<Expr>>,
    },
    Unsafe(Vec<Node<FuncStmt>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct MatchArm {
    pub pat: Node<Pattern>,
    pub body: Vec<Node<FuncStmt>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum Pattern {
    /// Represents a wildcard pattern `_`.
    WildCard,
    /// Rest pattern. e.g., `..`
    Rest,
    /// Represents a literal pattern. e.g., `true`.
    Literal(Node<LiteralPattern>),
    /// Represents tuple destructuring pattern. e.g., `(x, y, z)`.
    Tuple(Vec<Node<Pattern>>),
    /// Represents unit variant pattern. e.g., `Enum::Unit`.
    Path(Node<Path>),
    /// Represents tuple variant pattern. e.g., `Enum::Tuple(x, y, z)`.
    PathTuple(Node<Path>, Vec<Node<Pattern>>),
    /// Represents struct or struct variant destructuring pattern. e.g.,
    /// `MyStruct {x: pat1, y: pat2}}`.
    PathStruct {
        path: Node<Path>,
        fields: Vec<(Node<SmolStr>, Node<Pattern>)>,
        has_rest: bool,
    },
    /// Represents or pattern. e.g., `EnumUnit | EnumTuple(_, _, _)`
    Or(Vec<Node<Pattern>>),
}

impl Pattern {
    pub fn is_rest(&self) -> bool {
        matches!(self, Pattern::Rest)
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum LiteralPattern {
    Bool(bool),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub enum VarDeclTarget {
    Name(SmolStr),
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
        attr: Node<SmolStr>,
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
    Repeat {
        value: Box<Node<Expr>>,
        len: Box<Node<GenericArg>>,
    },
    Tuple {
        elts: Vec<Node<Expr>>,
    },
    Bool(bool),
    Name(SmolStr),
    Path(Path),
    Num(SmolStr),
    Str(SmolStr),
    Unit,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone)]
pub struct CallArg {
    pub label: Option<Node<SmolStr>>,
    pub value: Node<Expr>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum BoolOperator {
    And,
    Or,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum UnaryOperator {
    Invert,
    Not,
    USub,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
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

impl Node<Enum> {
    pub fn name(&self) -> &str {
        &self.kind.name.kind
    }
}

impl Node<Variant> {
    pub fn name(&self) -> &str {
        &self.kind.name.kind
    }
}

impl Node<Trait> {
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
        &self.kind.sig.kind.name.kind
    }
}

impl Node<FunctionArg> {
    pub fn name(&self) -> &str {
        match &self.kind {
            FunctionArg::Regular { name, .. } => &name.kind,
            FunctionArg::Self_ { .. } => "self",
        }
    }

    pub fn name_span(&self) -> Span {
        match &self.kind {
            FunctionArg::Regular { name, .. } => name.span,
            FunctionArg::Self_ { .. } => self.span,
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
            ModuleStmt::Use(inner) => inner.span,
            ModuleStmt::Trait(inner) => inner.span,
            ModuleStmt::Impl(inner) => inner.span,
            ModuleStmt::TypeAlias(inner) => inner.span,
            ModuleStmt::Contract(inner) => inner.span,
            ModuleStmt::Constant(inner) => inner.span,
            ModuleStmt::Struct(inner) => inner.span,
            ModuleStmt::Enum(inner) => inner.span,
            ModuleStmt::Function(inner) => inner.span,
            ModuleStmt::Attribute(inner) => inner.span,
            ModuleStmt::ParseError(span) => *span,
        }
    }
}

impl Spanned for ContractStmt {
    fn span(&self) -> Span {
        match self {
            ContractStmt::Function(inner) => inner.span,
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let (uses, rest): (Vec<&ModuleStmt>, Vec<&ModuleStmt>) = self
            .body
            .iter()
            .partition(|&stmt| matches!(stmt, ModuleStmt::Use(_) | ModuleStmt::Pragma(_)));
        for stmt in &uses {
            writeln!(f, "{stmt}")?;
        }
        if !uses.is_empty() && !rest.is_empty() {
            writeln!(f)?;
        }
        let mut delim = "";
        for stmt in rest {
            writeln!(f, "{delim}{stmt}")?;
            delim = "\n";
        }
        Ok(())
    }
}

impl fmt::Display for ModuleStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ModuleStmt::Pragma(node) => write!(f, "{}", node.kind),
            ModuleStmt::Use(node) => write!(f, "{}", node.kind),
            ModuleStmt::Trait(node) => write!(f, "{}", node.kind),
            ModuleStmt::Impl(node) => write!(f, "{}", node.kind),
            ModuleStmt::TypeAlias(node) => write!(f, "{}", node.kind),
            ModuleStmt::Contract(node) => write!(f, "{}", node.kind),
            ModuleStmt::Constant(node) => write!(f, "{}", node.kind),
            ModuleStmt::Struct(node) => write!(f, "{}", node.kind),
            ModuleStmt::Enum(node) => write!(f, "{}", node.kind),
            ModuleStmt::Function(node) => write!(f, "{}", node.kind),
            ModuleStmt::Attribute(node) => writeln!(f, "#{}", node.kind),
            ModuleStmt::ParseError(span) => {
                write!(f, "# PARSE ERROR: {}..{}", span.start, span.end)
            }
        }
    }
}

impl fmt::Display for Pragma {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "pragma {}", self.version_requirement.kind)
    }
}

impl fmt::Display for Use {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // TODO pub use
        write!(f, "use {}", self.tree.kind)
    }
}

impl fmt::Display for UseTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UseTree::Glob { prefix } => write!(f, "{prefix}::*"),
            UseTree::Simple { path, rename } => {
                if let Some(rename) = rename {
                    write!(f, "{} as {}", path, rename.kind)
                } else {
                    write!(f, "{path}")
                }
            }
            UseTree::Nested { prefix, children } => {
                write!(f, "{}::{{{}}}", prefix, node_comma_joined(children))
            }
        }
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let joined_names = self
            .segments
            .iter()
            .map(|name| name.kind.as_ref())
            .collect::<Vec<_>>()
            .join("::");
        write!(f, "{joined_names}")?;

        Ok(())
    }
}

impl fmt::Display for ConstantDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let ConstantDecl {
            name,
            typ,
            value,
            pub_qual,
        } = self;
        if pub_qual.is_some() {
            write!(f, "pub ")?;
        }
        write!(f, "const {}: {} = {}", name.kind, typ.kind, value.kind)
    }
}

impl fmt::Display for Trait {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "trait {}:", self.name.kind)?;

        Ok(())
    }
}

impl fmt::Display for Impl {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "impl {} for {}",
            self.impl_trait.kind, self.receiver.kind
        )?;

        Ok(())
    }
}

impl fmt::Display for TypeAlias {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TypeAlias {
            name,
            typ,
            pub_qual,
        } = self;
        if pub_qual.is_some() {
            write!(f, "pub ")?;
        }
        write!(f, "type {} = {}", name.kind, typ.kind)
    }
}

impl fmt::Display for Contract {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Contract {
            name,
            fields,
            body,
            pub_qual,
        } = self;

        if pub_qual.is_some() {
            write!(f, "pub ")?;
        }
        write!(f, "contract {} {{", name.kind)?;

        if !fields.is_empty() {
            for field in fields {
                writeln!(f)?;
                write!(indented(f), "{}", field.kind)?;
            }
            writeln!(f)?;
        }
        if !body.is_empty() {
            writeln!(f)?;
            write!(indented(f), "{}", double_line_joined(body))?;
            writeln!(f)?;
        }
        write!(f, "}}")
    }
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Struct {
            name,
            fields,
            functions,
            pub_qual,
        } = self;

        if pub_qual.is_some() {
            write!(f, "pub ")?;
        }
        write!(f, "struct {} ", name.kind)?;
        write!(f, "{{")?;
        write_nodes_line_wrapped(&mut indented(f), fields)?;

        if !self.fields.is_empty() && !functions.is_empty() {
            writeln!(f)?;
        }
        if !functions.is_empty() {
            writeln!(indented(f), "{}", double_line_joined(functions))?;
        }
        write!(f, "}}")
    }
}

impl fmt::Display for Enum {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Enum {
            name,
            variants,
            functions,
            pub_qual,
        } = self;

        if pub_qual.is_some() {
            write!(f, "pub ")?;
        }

        write!(f, "enum {} ", name.kind)?;
        write!(f, "{{")?;
        write_nodes_line_wrapped(&mut indented(f), variants)?;

        if !functions.is_empty() {
            writeln!(indented(f), "{}", double_line_joined(functions))?;
        }
        write!(f, "}}")
    }
}

impl fmt::Display for TypeDesc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeDesc::Unit => write!(f, "()"),
            TypeDesc::Base { base } => write!(f, "{base}"),
            TypeDesc::Path(path) => write!(f, "{path}"),
            TypeDesc::Tuple { items } => {
                if items.len() == 1 {
                    write!(f, "({},)", items[0].kind)
                } else {
                    write!(f, "({})", node_comma_joined(items))
                }
            }
            TypeDesc::Generic { base, args } => {
                write!(f, "{}<{}>", base.kind, comma_joined(args.kind.iter()))
            }
            TypeDesc::SelfType => write!(f, "Self"),
        }
    }
}

impl fmt::Display for GenericParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GenericParameter::Unbounded(name) => write!(f, "{}", name.kind),
            GenericParameter::Bounded { name, bound } => write!(f, "{}: {}", name.kind, bound.kind),
        }
    }
}

impl fmt::Display for GenericArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GenericArg::TypeDesc(node) => write!(f, "{}", node.kind),
            GenericArg::Int(node) => write!(f, "{}", node.kind),
            GenericArg::ConstExpr(node) => write!(f, "{{ {} }}", node.kind),
        }
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

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name.kind)?;
        match &self.kind {
            VariantKind::Unit => Ok(()),
            VariantKind::Tuple(elts) => {
                write!(f, "({})", node_comma_joined(elts))
            }
        }
    }
}

impl fmt::Display for ContractStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ContractStmt::Function(node) => write!(f, "{}", node.kind),
        }
    }
}

impl fmt::Display for Node<Function> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let FunctionSignature {
            pub_,
            unsafe_,
            name,
            generic_params,
            args,
            return_type,
        } = &self.sig.kind;

        if pub_.is_some() {
            write!(f, "pub ")?;
        }
        if unsafe_.is_some() {
            write!(f, "unsafe ")?;
        }
        write!(f, "fn {}", name.kind)?;
        if !generic_params.kind.is_empty() {
            write!(f, "<{}>", comma_joined(generic_params.kind.iter()))?;
        }
        write!(f, "({})", node_comma_joined(args))?;

        if let Some(return_type) = return_type.as_ref() {
            write!(f, " -> {}", return_type.kind)?;
        }
        write!(f, " {{")?;
        write_nodes_line_wrapped(&mut indented(f), &self.body)?;
        write!(f, "}}")
    }
}

impl fmt::Display for FunctionArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FunctionArg::Regular {
                mut_,
                label,
                name,
                typ,
            } => {
                if mut_.is_some() {
                    write!(f, "mut ")?
                }
                if let Some(label) = label {
                    write!(f, "{} ", label.kind)?
                }

                write!(f, "{}: {}", name.kind, typ.kind)
            }
            FunctionArg::Self_ { mut_ } => {
                if mut_.is_some() {
                    write!(f, "mut ")?;
                }
                write!(f, "self")
            }
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
            FuncStmt::VarDecl {
                mut_,
                target,
                typ,
                value,
            } => {
                let mut_ = if mut_.is_some() { "mut " } else { "" };
                if let Some(value) = value {
                    write!(
                        f,
                        "let {}{}: {} = {}",
                        mut_, target.kind, typ.kind, value.kind
                    )
                } else {
                    write!(f, "let {}{}: {}", mut_, target.kind, typ.kind)
                }
            }
            FuncStmt::ConstantDecl { name, typ, value } => {
                write!(f, "const {}: {} = {}", name.kind, typ.kind, value.kind)
            }
            FuncStmt::Assign { target, value } => write!(f, "{} = {}", target.kind, value.kind),
            FuncStmt::AugAssign { target, op, value } => {
                write!(f, "{} {}= {}", target.kind, op.kind, value.kind)
            }
            FuncStmt::For { target, iter, body } => {
                write!(f, "for {} in {} {{", target.kind, iter.kind)?;
                write_nodes_line_wrapped(&mut indented(f), body)?;
                write!(f, "}}")
            }
            FuncStmt::While { test, body } => {
                write!(f, "while {} {{", test.kind)?;
                write_nodes_line_wrapped(&mut indented(f), body)?;
                write!(f, "}}")
            }
            FuncStmt::If {
                test,
                body,
                or_else,
            } => {
                write!(f, "if {} {{", test.kind)?;
                write_nodes_line_wrapped(&mut indented(f), body)?;

                if or_else.is_empty() {
                    write!(f, "}}")
                } else {
                    if body.is_empty() {
                        writeln!(f)?;
                    }

                    if matches!(
                        &or_else[..],
                        &[Node {
                            kind: FuncStmt::If { .. },
                            ..
                        }]
                    ) {
                        write!(f, "}} else ")?;
                        write!(f, "{}", or_else[0].kind)
                    } else {
                        write!(f, "}} else {{")?;
                        write_nodes_line_wrapped(&mut indented(f), or_else)?;
                        write!(f, "}}")
                    }
                }
            }
            FuncStmt::Match { expr, arms } => {
                write!(f, "match {} {{", expr.kind)?;
                write_nodes_line_wrapped(&mut indented(f), arms)?;
                write!(f, "}}")
            }
            FuncStmt::Assert { test, msg } => {
                if let Some(msg) = msg {
                    write!(f, "assert {}, {}", test.kind, msg.kind)
                } else {
                    write!(f, "assert {}", test.kind)
                }
            }
            FuncStmt::Expr { value } => write!(f, "{}", value.kind),
            FuncStmt::Break => write!(f, "break"),
            FuncStmt::Continue => write!(f, "continue"),
            FuncStmt::Revert { error } => {
                if let Some(error) = error {
                    write!(f, "revert {}", error.kind)
                } else {
                    write!(f, "revert")
                }
            }
            FuncStmt::Unsafe(body) => {
                write!(f, "unsafe {{")?;
                write_nodes_line_wrapped(&mut indented(f), body)?;
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Display for VarDeclTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            VarDeclTarget::Name(name) => write!(f, "{name}"),
            VarDeclTarget::Tuple(elts) => {
                if elts.len() == 1 {
                    write!(f, "({},)", elts[0].kind)
                } else {
                    write!(f, "({})", node_comma_joined(elts))
                }
            }
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
                    write!(f, "<{}>", comma_joined(generic_args.kind.iter()))?;
                }
                write!(f, "({})", node_comma_joined(&args.kind))
            }
            Expr::List { elts } => write!(f, "[{}]", node_comma_joined(elts)),
            Expr::Repeat { value: elt, len } => write!(f, "[{}; {}]", elt.kind, len.kind),
            Expr::Tuple { elts } => {
                if elts.len() == 1 {
                    write!(f, "({},)", elts[0].kind)
                } else {
                    write!(f, "({})", node_comma_joined(elts))
                }
            }
            Expr::Bool(bool) => write!(f, "{bool}"),
            Expr::Name(name) => write!(f, "{name}"),
            Expr::Path(path) => write!(f, "{path}"),
            Expr::Num(num) => write!(f, "{num}"),
            Expr::Str(str) => write!(f, "\"{str}\""),
            Expr::Unit => write!(f, "()"),
        }
    }
}

impl fmt::Display for CallArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            if let Expr::Name(var_name) = &self.value.kind {
                if var_name == &label.kind {
                    return write!(f, "{var_name}");
                }
            }
            write!(f, "{}: {}", label.kind, self.value.kind)
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

impl fmt::Display for MatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} => {{", self.pat.kind)?;
        write_nodes_line_wrapped(&mut indented(f), &self.body)?;
        write!(f, "}}")
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WildCard => write!(f, "_"),
            Self::Rest => write!(f, ".."),
            Self::Literal(pat) => write!(f, "{}", pat.kind),
            Self::Path(path) => write!(f, "{}", path.kind),
            Self::PathTuple(path, elts) => {
                write!(f, "{}", path.kind)?;
                write!(f, "({})", node_comma_joined(elts))
            }
            Self::Tuple(elts) => {
                write!(f, "({})", node_comma_joined(elts))
            }
            Self::PathStruct {
                path,
                fields,
                has_rest: is_rest,
            } => {
                write!(f, "{}", path.kind)?;
                let fields = fields
                    .iter()
                    .map(|(name, pat)| format!("{}: {}", name.kind, pat.kind));
                let fields = comma_joined(fields);
                if *is_rest {
                    write!(f, "{{{fields}, ..}}")
                } else {
                    write!(f, "{{{fields}}}")
                }
            }
            Self::Or(pats) => {
                write!(f, "{}", node_delim_joined(pats, "| "))
            }
        }
    }
}

impl fmt::Display for LiteralPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
        }
    }
}

fn node_comma_joined(nodes: &[Node<impl fmt::Display>]) -> String {
    node_delim_joined(nodes, ", ")
}

fn node_delim_joined(nodes: &[Node<impl fmt::Display>], delim: &str) -> String {
    delim_joined(nodes.iter().map(|node| &node.kind), delim)
}

fn comma_joined<T>(items: impl Iterator<Item = T>) -> String
where
    T: fmt::Display,
{
    delim_joined(items, ", ")
}

fn delim_joined<T>(items: impl Iterator<Item = T>, delim: &str) -> String
where
    T: fmt::Display,
{
    items
        .map(|item| format!("{item}"))
        .collect::<Vec<_>>()
        .join(delim)
}

fn write_nodes_line_wrapped(f: &mut impl Write, nodes: &[Node<impl fmt::Display>]) -> fmt::Result {
    if !nodes.is_empty() {
        writeln!(f)?;
    }
    for n in nodes {
        writeln!(f, "{}", n.kind)?;
    }
    Ok(())
}

fn double_line_joined(items: &[impl fmt::Display]) -> String {
    items
        .iter()
        .map(|item| format!("{item}"))
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
        format!("({expr})")
    } else {
        format!("{expr}")
    }
}

fn maybe_fmt_right_with_parens(op: &impl InfixBindingPower, expr: &Expr) -> String {
    if op.infix_binding_power().1 > expr_left_binding_power(expr) {
        format!("({expr})")
    } else {
        format!("{expr}")
    }
}

fn maybe_fmt_operand_with_parens(op: &impl PrefixBindingPower, expr: &Expr) -> String {
    if op.prefix_binding_power() > expr_left_binding_power(expr) {
        format!("({expr})")
    } else {
        format!("{expr}")
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
        Expr::Repeat { .. } => max_power,
        Expr::Tuple { .. } => max_power,
        Expr::Bool(_) => max_power,
        Expr::Name(_) => max_power,
        Expr::Path(_) => max_power,
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
        Expr::Repeat { .. } => max_power,
        Expr::Tuple { .. } => max_power,
        Expr::Bool(_) => max_power,
        Expr::Name(_) => max_power,
        Expr::Path(_) => max_power,
        Expr::Num(_) => max_power,
        Expr::Str(_) => max_power,
        Expr::Unit => max_power,
    }
}
