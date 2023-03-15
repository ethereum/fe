use fe_parser2::{ast::AstPtr, SyntaxNode};

/// This enum represents the origin of the HIR node.
/// The origin has three possible kinds.
/// 1. `Raw` is used for nodes that are created by the parser and not
/// 2. `Expanded` is used for nodes that are created by the compiler and not
/// 3. `Desugared` is used for nodes that are created by the compiler and not
// TODO: Change the visibility to `pub(crate)` when https://github.com/salsa-rs/salsa/issues/437 is resolved.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HirOrigin<T>
where
    T: Send + Clone + PartialEq + Eq + std::fmt::Debug + std::hash::Hash,
{
    /// The HIR node is created by direct lowering from the corresponding AST.
    Raw(T),
    /// The HIR node is created by expanding attributes.
    /// The `SyntaxNode` points to the callsite of the attribute.
    Expanded(SyntaxNode),
    /// The HIR node is the result of desugaring in the lower phase from AST to
    /// HIR. e.g., `a += b` is desugared into `a = a + b`.
    Desugared(DesugaredOrigin),
}

/// This enum represents the origin of the HIR node which is desugared into
/// other HIR node kinds.
// TODO: Change the visibility to `pub(crate)` when https://github.com/salsa-rs/salsa/issues/437 is resolved.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DesugaredOrigin {
    /// The HIR node is the result of desugaring an augmented assignment
    /// statement.
    AugAssign(AstPtr<fe_parser2::ast::AugAssignStmt>),
}
