use std::path::PathBuf;

use fe_parser2::{
    ast::{prelude::*, AstPtr},
    SyntaxNode,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HirOrigin<T>
where
    T: AstNode,
{
    pub fid: FileId,
    pub kind: HirOriginKind<T>,
}

impl<T> HirOrigin<T>
where
    T: AstNode,
{
    pub fn raw(fid: FileId, ast: &T) -> Self {
        HirOrigin {
            fid,
            kind: HirOriginKind::Raw(AstPtr::new(ast)),
        }
    }

    pub fn none(file: FileId) -> Self {
        HirOrigin {
            fid: file,
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
    Expanded(SyntaxNode),
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

/// This enum represents the origin of the HIR node which is desugared into
/// other HIR node kinds.
// TODO: Change the visibility to `pub(crate)` when https://github.com/salsa-rs/salsa/issues/437 is resolved.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DesugaredOrigin {
    /// The HIR node is the result of desugaring an augmented assignment
    /// statement.
    AugAssign(AstPtr<fe_parser2::ast::AugAssignStmt>),
}

/// This enum represents the file
#[salsa::interned]
pub struct FileId {
    /// A ingot id which the file belongs to.
    ingot: IngotId,
    /// A relative path from the ingot root.
    path: PathBuf,
}

#[salsa::interned]
pub struct IngotId {
    /// A full path to the ingot root.
    path: PathBuf,
    kind: IngotKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IngotKind {
    /// A standalone ingot is a dummy ingot when the compiler is invoked
    /// directly on a file.
    StandAlone,

    /// A local ingot which is the current ingot being compiled.
    Local,

    /// An external ingot which is depended on by the current ingot.
    External,

    /// A std ingot.
    Std,
}
