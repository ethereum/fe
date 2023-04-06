use parser::{
    ast::{self, prelude::*, AstPtr, SyntaxNodePtr},
    TextRange,
};

use common::InputFile;

use self::db::SpannedHirDb;

pub mod attr;
pub mod db;
pub mod expr;
pub mod item;
pub mod params;
pub mod pat;
pub mod path;
pub mod types;
pub mod use_tree;

mod transition;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HirOrigin<T>
where
    T: AstNode,
{
    pub file: InputFile,
    pub kind: LocalOrigin<T>,
}

impl<T> HirOrigin<T>
where
    T: AstNode<Language = parser::FeLang>,
{
    fn syntax_ptr(&self) -> Option<SyntaxNodePtr> {
        self.kind.syntax_ptr()
    }
}

impl<T> HirOrigin<T>
where
    T: AstNode<Language = parser::FeLang>,
{
    pub(crate) fn new(file: InputFile, origin: LocalOrigin<T>) -> Self {
        HirOrigin { file, kind: origin }
    }

    pub(crate) fn raw(file: InputFile, ast: &T) -> Self {
        Self::new(file, LocalOrigin::raw(ast))
    }
}

/// This enum represents the origin of the HIR node is a file.
/// The origin has three possible kinds.
/// 1. `Raw` is used for nodes that are created by the parser and not
/// 2. `Expanded` is used for nodes that are created by the compiler and not
/// 3. `Desugared` is used for nodes that are created by the compiler and not
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LocalOrigin<T>
where
    T: AstNode,
{
    /// The HIR node is created by direct lowering from the corresponding AST.
    Raw(AstPtr<T>),
    /// The HIR node is created by expanding attributes.
    /// The `SyntaxNode` points to the callsite of the attribute.
    Expanded(SyntaxNodePtr),
    /// The HIR node is the result of desugaring in the lower phase from AST to
    /// HIR. e.g., `a += b` is desugared into `a = a + b`.
    Desugared(DesugaredOrigin),

    /// The HIR node is created by the compiler and not directly from the AST.
    /// This is only used with `Invalid` nodes that don't have a corresponding
    /// AST node.
    /// e.g., the RHS of `a + ` is represented as `Invalid` node but there is no
    /// corresponding origin.
    None,
}

impl<T> LocalOrigin<T>
where
    T: AstNode<Language = parser::FeLang>,
{
    pub(crate) fn raw(ast: &T) -> Self {
        Self::Raw(AstPtr::new(ast))
    }

    fn syntax_ptr(&self) -> Option<SyntaxNodePtr> {
        match self {
            LocalOrigin::Raw(ptr) => Some(ptr.syntax_node_ptr()),
            LocalOrigin::Expanded(ptr) => Some(ptr.clone()),
            _ => None,
        }
    }

    pub(crate) fn desugared(origin: impl Into<DesugaredOrigin>) -> Self {
        Self::Desugared(origin.into())
    }
}

impl<T> Default for LocalOrigin<T>
where
    T: AstNode,
{
    fn default() -> Self {
        Self::None
    }
}

/// This enum represents the origin of the HIR node which is desugared into
/// other HIR node kinds.
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum DesugaredOrigin {
    /// The HIR node is the result of desugaring an augmented assignment
    /// statement.
    AugAssign(AugAssignDesugared),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum AugAssignDesugared {
    /// The HIR node is the result of desugaring an augmented assignment
    /// statement.
    Stmt(AstPtr<ast::AugAssignStmt>),
    /// The `TextRange` points to the LHS of the augmented assignment statement.
    Lhs(TextRange),
    /// The HIR node points to the RHS of the RHS of augmented assignment.
    Rhs(AstPtr<ast::Expr>),
}

impl AugAssignDesugared {
    pub(crate) fn stmt(ast: &ast::AugAssignStmt) -> Self {
        Self::Stmt(AstPtr::new(ast))
    }
}

/// The trait provides a way to extract [`EvaluatedSpan`] from types which don't
/// have a span information directly, but can be evaluated from the database
/// lazily.
pub trait LazySpan {
    fn eval(&self, db: &dyn SpannedHirDb) -> EvaluatedSpan;
}

/// This struct represents a result of [`LazySpan::span`] method.
/// It contains the file and the text range.
///
/// `range` is an optional field because some HIR nodes doesn't have a span when
/// they are syntactically invalid.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EvaluatedSpan {
    pub file: InputFile,
    pub range: Option<TextRange>,
}
impl EvaluatedSpan {
    pub fn new(file: InputFile, range: Option<TextRange>) -> Self {
        Self { file, range }
    }
}

use transition::define_lazy_span_node;
define_lazy_span_node!(LazySpanAtom);
