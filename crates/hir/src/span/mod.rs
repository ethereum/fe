use parser::{
    ast::{self, prelude::*, AstPtr, SyntaxNodePtr},
    TextRange,
};

use common::diagnostics::Span;

use crate::{
    hir_def::{
        Body, Const, Contract, Enum, Func, Impl, ImplTrait, Mod, Struct, TopLevelMod, Trait,
        TypeAlias, Use,
    },
    lower::top_mod_ast,
    HirDb, SpannedHirDb,
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

pub(crate) mod transition;

pub mod lazy_spans {
    pub use super::attr::{
        LazyAttrArgListSpan, LazyAttrArgSpan, LazyAttrListSpan, LazyAttrSpan,
        LazyDocCommentAttrSpan, LazyNormalAttrSpan,
    };

    pub use super::expr::{
        LazyBinExprSpan, LazyCallArgListSpan, LazyCallArgSpan, LazyCallExprSpan, LazyExprSpan,
        LazyFieldExprSpan, LazyFieldListSpan, LazyFieldSpan, LazyLitExprSpan, LazyMatchArmListSpan,
        LazyMatchArmSpan, LazyMatchExprSpan, LazyMethodCallExprSpan, LazyPathExprSpan,
        LazyRecordInitExprSpan, LazyUnExprSpan,
    };

    pub use super::item::{
        LazyBodySpan, LazyConstSpan, LazyContractSpan, LazyEnumSpan, LazyFieldDefListSpan,
        LazyFieldDefSpan, LazyFuncSpan, LazyImplSpan, LazyImplTraitSpan, LazyItemModifierSpan,
        LazyItemSpan, LazyModSpan, LazyStructSpan, LazyTopModSpan, LazyTraitSpan,
        LazyTypeAliasSpan, LazyUseSpan, LazyVariantDefListSpan, LazyVariantDefSpan,
    };

    pub use super::params::{
        LazyConstGenericParamSpan, LazyFuncParamListSpan, LazyFuncParamSpan,
        LazyGenericArgListSpan, LazyGenericArgSpan, LazyGenericParamListSpan, LazyGenericParamSpan,
        LazyKindBoundSpan, LazyTraitBoundSpan, LazyTypeBoundListSpan, LazyTypeBoundSpan,
        LazyTypeGenericArgSpan, LazyWhereClauseSpan, LazyWherePredicateSpan,
    };

    pub use super::pat::{
        LazyLitPatSpan, LazyPatSpan, LazyPathPatSpan, LazyPathTuplePatSpan,
        LazyRecordPatFieldListSpan, LazyRecordPatFieldSpan, LazyRecordPatSpan,
    };

    pub use super::path::{LazyPathSegmentSpan, LazyPathSpan};

    pub use super::stmt::{LazyLetStmtSpan, LazyStmtSpan};

    pub use super::types::{
        LazyArrayTypeSpan, LazyPathTypeSpan, LazyPtrTypeSpan, LazyTupleTypeSpan, LazyTySpan,
    };

    pub use super::use_tree::{LazyUseAliasSpan, LazyUsePathSegmentSpan, LazyUsePathSpan};

    pub use super::{DynLazySpan, LazyLitSpan, LazySpan, LazySpanAtom};
}

/// This struct represents a dynamic lazy span, which can be converted from all
/// types that implement [`LazySpan`] in this module. We want to avoid `dyn
/// LazySpan` usage because it doesn't implement `Clone` and `Eq` which leads to
/// a lot of difficulties in salsa integration
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DynLazySpan(pub(super) Option<SpanTransitionChain>);
impl DynLazySpan {
    pub fn invalid() -> Self {
        Self(None)
    }

    pub fn top_mod(&self, db: &dyn HirDb) -> Option<TopLevelMod> {
        self.0.as_ref().map(|chain| chain.top_mod(db))
    }
}
impl LazySpan for DynLazySpan {
    fn resolve(&self, db: &dyn crate::SpannedHirDb) -> Option<Span> {
        if let Some(chain) = &self.0 {
            chain.resolve(db)
        } else {
            None
        }
    }
}

pub trait SpanDowncast {
    fn downcast(dyn_span: DynLazySpan) -> Option<Self>
    where
        Self: Sized;
}

/// The trait provides a way to extract [`Span`](common::diagnostics::Span) from
/// types which don't have a span information directly, but can be resolved into
/// a span lazily.
pub trait LazySpan {
    fn resolve(&self, db: &dyn crate::SpannedHirDb) -> Option<Span>;
}

pub fn toplevel_ast(db: &dyn SpannedHirDb, item: TopLevelMod) -> HirOrigin<ast::Root> {
    HirOrigin::raw(&top_mod_ast(db.as_hir_db(), item))
}

pub fn mod_ast(db: &dyn SpannedHirDb, item: Mod) -> &HirOrigin<ast::Mod> {
    item.origin(db.as_hir_db())
}

pub fn func_ast(db: &dyn SpannedHirDb, item: Func) -> &HirOrigin<ast::Func> {
    item.origin(db.as_hir_db())
}

pub fn struct_ast(db: &dyn SpannedHirDb, item: Struct) -> &HirOrigin<ast::Struct> {
    item.origin(db.as_hir_db())
}

pub fn contract_ast(db: &dyn SpannedHirDb, item: Contract) -> &HirOrigin<ast::Contract> {
    item.origin(db.as_hir_db())
}

pub fn enum_ast(db: &dyn SpannedHirDb, item: Enum) -> &HirOrigin<ast::Enum> {
    item.origin(db.as_hir_db())
}

pub fn type_alias_ast(db: &dyn SpannedHirDb, item: TypeAlias) -> &HirOrigin<ast::TypeAlias> {
    item.origin(db.as_hir_db())
}

pub fn impl_ast(db: &dyn SpannedHirDb, item: Impl) -> &HirOrigin<ast::Impl> {
    item.origin(db.as_hir_db())
}

pub fn trait_ast(db: &dyn SpannedHirDb, item: Trait) -> &HirOrigin<ast::Trait> {
    item.origin(db.as_hir_db())
}

pub fn impl_trait_ast(db: &dyn SpannedHirDb, item: ImplTrait) -> &HirOrigin<ast::ImplTrait> {
    item.origin(db.as_hir_db())
}

pub fn const_ast(db: &dyn SpannedHirDb, item: Const) -> &HirOrigin<ast::Const> {
    item.origin(db.as_hir_db())
}

pub fn use_ast(db: &dyn SpannedHirDb, item: Use) -> &HirOrigin<ast::Use> {
    item.origin(db.as_hir_db())
}

pub fn body_ast(db: &dyn SpannedHirDb, item: Body) -> &HirOrigin<ast::Expr> {
    item.origin(db.as_hir_db())
}

pub fn body_source_map(db: &dyn SpannedHirDb, item: Body) -> &crate::hir_def::BodySourceMap {
    item.source_map(db.as_hir_db())
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

    /// The HIR node is the result of desugaring a AST use.
    /// In HIR lowering, nested use tree is flattened into a single use path.
    Use(UseDesugared),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseDesugared {
    pub root: AstPtr<ast::Use>,
    pub path: Vec<AstPtr<ast::UsePathSegment>>,
    pub alias: Option<AstPtr<ast::UseAlias>>,
    focus: DesugaredUseFocus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum DesugaredUseFocus {
    Root,
    Path,
    Alias,
}

impl UseDesugared {
    pub(super) fn new(ast: &ast::Use) -> Self {
        Self {
            root: AstPtr::new(ast),
            path: vec![],
            alias: None,
            focus: DesugaredUseFocus::Root,
        }
    }

    pub(super) fn add_alias(&mut self, alias: &ast::UseAlias) {
        self.alias = Some(AstPtr::new(alias))
    }

    pub(super) fn push_seg(&mut self, seg: &ast::UsePathSegment) {
        self.path.push(AstPtr::new(seg));
    }
}

use transition::define_lazy_span_node;

use self::transition::SpanTransitionChain;

define_lazy_span_node!(LazySpanAtom);
impl LazySpanAtom {
    pub(super) fn into_lit_span(self) -> LazyLitSpan {
        LazyLitSpan(self.0)
    }
}
define_lazy_span_node!(LazyLitSpan);
