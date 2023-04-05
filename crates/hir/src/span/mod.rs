use std::sync::Arc;

use parser::{
    ast::{self, prelude::*, AstPtr, SyntaxNodePtr},
    syntax_node::NodeOrToken,
    SyntaxNode, TextRange,
};

use common::{diagnostics::Span, InputFile};
use smallvec::SmallVec;

use crate::{hir_def::ItemKind, parse_file};

use self::db::SpannedHirDb;

pub mod attr;
pub mod db;
pub mod item;
pub mod params;
pub mod path;
pub mod types;
pub mod use_tree;

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
        match &self.kind {
            LocalOrigin::Raw(ptr) => Some(ptr.syntax_node_ptr()),
            LocalOrigin::Expanded(ptr) => Some(ptr.clone()),
            _ => None,
        }
    }
}

impl<T> HirOrigin<T>
where
    T: AstNode,
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
    T: AstNode,
{
    pub(crate) fn raw(ast: &T) -> Self {
        Self::Raw(AstPtr::new(ast))
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

/// The trait provides a way to extract [`Span`] from types which don't have a
/// span information directly.
pub trait LazySpan {
    fn span(self, db: &dyn SpannedHirDb) -> Span;
}

type TransitionFn = Arc<dyn Fn(SyntaxNode) -> Option<NodeOrToken>>;

#[derive(Clone)]
pub(super) struct SpanTransitionChain {
    root: ItemKind,
    chain: SmallVec<[TransitionFn; 4]>,
}

impl SpanTransitionChain {
    fn new(item: ItemKind) -> Self {
        Self {
            root: item,
            chain: SmallVec::new(),
        }
    }

    fn push_state(&self, transition: TransitionFn) -> Self {
        let mut new_state = self.clone();
        new_state.chain.push(transition);
        new_state
    }
}

impl LazySpan for SpanTransitionChain {
    fn span(self, db: &dyn SpannedHirDb) -> Span {
        let (file, ptr) = match self.root {
            ItemKind::TopMod(top_level_mod) => {
                let ast = db.toplevel_ast(top_level_mod);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::Mod(mod_) => {
                let ast = db.mod_ast(mod_);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::Fn(fn_) => {
                let ast = db.fn_ast(fn_);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::ExternFn(extern_fn) => {
                let ast = db.extern_fn_ast(extern_fn);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::Struct(struct_) => {
                let ast = db.struct_ast(struct_);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::Contract(contract) => {
                let ast = db.contract_ast(contract);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::Enum(enum_) => {
                let ast = db.enum_ast(enum_);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::TypeAlias(alias) => {
                let ast = db.type_alias_ast(alias);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::Impl(impl_) => {
                let ast = db.impl_ast(impl_);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::Trait(trait_) => {
                let ast = db.trait_ast(trait_);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::ImplTrait(impl_trait) => {
                let ast = db.impl_trait_ast(impl_trait);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::Const(const_) => {
                let ast = db.const_ast(const_);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::Use(use_) => {
                let ast = db.use_ast(use_);
                (ast.file, ast.syntax_ptr().unwrap())
            }

            ItemKind::Body(body) => {
                let ast = db.body_ast(body);
                (ast.file, ast.syntax_ptr().unwrap())
            }
        };

        let root_node = SyntaxNode::new_root(parse_file(db.upcast(), file));
        let mut node = ptr.to_node(&root_node);

        for transition in self.chain {
            node = match transition(node.clone()) {
                Some(NodeOrToken::Node(node)) => node,
                Some(NodeOrToken::Token(token)) => {
                    return Span::new(file, token.text_range());
                }
                None => {
                    return Span::new(file, node.text_range());
                }
            };
        }

        Span::new(file, node.text_range())
    }
}

define_lazy_span_item!(LazyTokenSpan);

macro_rules! define_lazy_span_item {
    (
        $name:ident
        $(,
            $sk_node: ty
            $(,
                $(new($hir_ty:ty),)?
                $(@token {$(($name_token:ident, $getter_token:ident),)*})?
                $(@node {$(($name_node:ident, $getter_node:ident, $result:tt),)*})?
                $(@idx { $(($name_iter:ident, $result_iter:tt),)*})?
                $(,)?
            )?
        )?
    ) => {
        #[derive(Clone)]
        pub struct $name(pub(super) crate::span::SpanTransitionChain);
        $(
            $(
            impl $name {

            $(pub fn new(hir: $hir_ty) -> Self {
                Self(crate::span::SpanTransitionChain::new(hir.into()))
            })?

            $($(
                pub fn $name_token(&self) -> crate::span::LazyTokenSpan {
                    use parser::ast::prelude::*;
                    let transition = |node: parser::SyntaxNode| {
                        <$sk_node as AstNode>::cast(node)
                            .and_then(|n| n.$getter_token())
                            .map(|n| n.into())
                    };
                    crate::span::LazyTokenSpan(
                        self.0.push_state(std::sync::Arc::new(transition))
                    )
                }
            )*)?

            $($(
                pub fn $name_node(&self) -> $result{
                    use parser::ast::prelude::*;
                    let transition = |node: parser::SyntaxNode| {
                        <$sk_node as AstNode>::cast(node)
                            .and_then(|f| f.$getter_node())
                            .map(|n| n.syntax().clone().into())
                    };
                    $result(self.0.push_state(std::sync::Arc::new(transition)))
                }
            )*)?

            $($(

                pub fn $name_iter(&self, idx: usize) -> $result_iter {
                    use parser::ast::prelude::*;
                    let transition = move |node: parser::SyntaxNode| {
                        <$sk_node as AstNode>::cast(node)
                            .and_then(|f| f.into_iter().nth(idx))
                            .map(|n| n.syntax().clone().into())
                    };
                    $result_iter(self.0.push_state(std::sync::Arc::new(transition)))
                }
            )*)?
        })?)?


        impl crate::span::LazySpan for $name {
            fn span(self, db: &dyn crate::span::SpannedHirDb) -> common::diagnostics::Span {
                self.0.span(db)
            }
        }
    };
}

use define_lazy_span_item;