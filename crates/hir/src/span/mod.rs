use parser::{
    ast::{self, prelude::*, AstPtr, SyntaxNodePtr},
    TextRange,
};

use common::diagnostics::Span;

use crate::{
    hir_def::{
        Body, Const, Contract, Enum, ExternFunc, Func, Impl, ImplTrait, Mod, Struct, TopLevelMod,
        Trait, TypeAlias, Use,
    },
    lower::top_mod_ast,
    SpannedHirDb,
};

pub mod attr;
pub mod expr;
pub mod item;
pub mod params;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod types;
pub mod use_tree;

mod transition;

/// The trait provides a way to extract [`Span`](common::diagnostics::Span) from
/// types which don't have a span information directly, but can be resolved into
/// a span lazily.
pub trait LazySpan {
    fn resolve(&self, db: &dyn crate::SpannedHirDb) -> Span;
}

pub fn toplevel_ast(db: &dyn SpannedHirDb, item: TopLevelMod) -> HirOrigin<ast::Root> {
    HirOrigin::raw(&top_mod_ast(db.upcast(), item))
}

pub fn mod_ast(db: &dyn SpannedHirDb, item: Mod) -> &HirOrigin<ast::Mod> {
    item.origin(db.upcast())
}

pub fn func_ast(db: &dyn SpannedHirDb, item: Func) -> &HirOrigin<ast::Fn> {
    item.origin(db.upcast())
}

pub fn extern_func_ast(db: &dyn SpannedHirDb, item: ExternFunc) -> &HirOrigin<ast::Fn> {
    item.origin(db.upcast())
}

pub fn struct_ast(db: &dyn SpannedHirDb, item: Struct) -> &HirOrigin<ast::Struct> {
    item.origin(db.upcast())
}

pub fn contract_ast(db: &dyn SpannedHirDb, item: Contract) -> &HirOrigin<ast::Contract> {
    item.origin(db.upcast())
}

pub fn enum_ast(db: &dyn SpannedHirDb, item: Enum) -> &HirOrigin<ast::Enum> {
    item.origin(db.upcast())
}

pub fn type_alias_ast(db: &dyn SpannedHirDb, item: TypeAlias) -> &HirOrigin<ast::TypeAlias> {
    item.origin(db.upcast())
}

pub fn impl_ast(db: &dyn SpannedHirDb, item: Impl) -> &HirOrigin<ast::Impl> {
    item.origin(db.upcast())
}

pub fn trait_ast(db: &dyn SpannedHirDb, item: Trait) -> &HirOrigin<ast::Trait> {
    item.origin(db.upcast())
}

pub fn impl_trait_ast(db: &dyn SpannedHirDb, item: ImplTrait) -> &HirOrigin<ast::ImplTrait> {
    item.origin(db.upcast())
}

pub fn const_ast(db: &dyn SpannedHirDb, item: Const) -> &HirOrigin<ast::Const> {
    item.origin(db.upcast())
}

pub fn use_ast(db: &dyn SpannedHirDb, item: Use) -> &HirOrigin<ast::Use> {
    item.origin(db.upcast())
}

pub fn body_ast(db: &dyn SpannedHirDb, item: Body) -> &HirOrigin<ast::Expr> {
    item.origin(db.upcast())
}

pub fn body_source_map(db: &dyn SpannedHirDb, item: Body) -> &crate::hir_def::BodySourceMap {
    item.source_map(db.upcast())
}

/// This enum represents the origin of the HIR node in a file.
/// The origin has three possible kinds.
/// 1. `Raw` is used for nodes that are created by the parser and not
/// 2. `Expanded` is used for nodes that are created by the compiler and not
/// 3. `Desugared` is used for nodes that are created by the compiler and not
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HirOrigin<T>
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

impl<T> HirOrigin<T>
where
    T: AstNode<Language = parser::FeLang>,
{
    pub(crate) fn raw(ast: &T) -> Self {
        Self::Raw(AstPtr::new(ast))
    }

    pub(crate) fn desugared(origin: impl Into<DesugaredOrigin>) -> Self {
        Self::Desugared(origin.into())
    }
}

impl<T> Default for HirOrigin<T>
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

use transition::define_lazy_span_node;

define_lazy_span_node!(LazySpanAtom);
