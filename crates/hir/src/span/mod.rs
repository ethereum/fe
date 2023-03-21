use fe_parser2::{
    ast::{self, prelude::*, AstPtr, SyntaxNodePtr},
    TextRange,
};

use crate::input::File;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HirOrigin<T>
where
    T: AstNode,
{
    pub file: Option<File>,
    pub kind: HirOriginKind<T>,
}

impl<T> HirOrigin<T>
where
    T: AstNode,
{
    pub(crate) fn new(file: File, origin: HirOriginKind<T>) -> Self {
        HirOrigin {
            file: Some(file),
            kind: origin,
        }
    }

    pub(crate) fn raw(file: File, ast: &T) -> Self {
        HirOrigin {
            file: Some(file),
            kind: HirOriginKind::raw(ast),
        }
    }

    pub(crate) fn none(file: File) -> Self {
        HirOrigin {
            file: Some(file),
            kind: HirOriginKind::None,
        }
    }
}

impl<T> Default for HirOrigin<T>
where
    T: AstNode,
{
    /// The `Default` implemntation is necessary for
    fn default() -> Self {
        Self {
            file: None,
            kind: HirOriginKind::None,
        }
    }
}

/// This enum represents the origin of the HIR node.
/// The origin has three possible kinds.
/// 1. `Raw` is used for nodes that are created by the parser and not
/// 2. `Expanded` is used for nodes that are created by the compiler and not
/// 3. `Desugared` is used for nodes that are created by the compiler and not
// TODO: Change the visibility to `pub(crate)` when https://github.com/salsa-rs/salsa/issues/437 is resolved.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HirOriginKind<T>
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

impl<T> HirOriginKind<T>
where
    T: AstNode,
{
    pub(crate) fn raw(ast: &T) -> Self {
        Self::Raw(AstPtr::new(ast))
    }

    pub(crate) fn desugared(origin: impl Into<DesugaredOrigin>) -> Self {
        Self::Desugared(origin.into())
    }
}

/// This enum represents the origin of the HIR node which is desugared into
/// other HIR node kinds.
// TODO: Change the visibility to `pub(crate)` when https://github.com/salsa-rs/salsa/issues/437 is resolved.
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

    pub(crate) fn rhs(ast: &ast::Expr) -> Self {
        Self::Rhs(AstPtr::new(ast))
    }
}
