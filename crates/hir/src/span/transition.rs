use common::{
    diagnostics::{Span, SpanKind},
    InputFile,
};
use parser::{
    ast::prelude::*, syntax_node::NodeOrToken, FeLang, SyntaxNode, SyntaxToken, TextRange,
};

use crate::{
    hir_def::{
        Body, Const, Contract, Enum, Func, Impl, ImplTrait, ItemKind, Mod, Struct, TopLevelMod,
        Trait, TypeAlias, Use,
    },
    lower::top_mod_ast,
    SpannedHirDb,
};

use super::{
    body_ast, const_ast, contract_ast, enum_ast, expr::ExprRoot, func_ast, impl_ast,
    impl_trait_ast, mod_ast, pat::PatRoot, stmt::StmtRoot, struct_ast, trait_ast, type_alias_ast,
    use_ast, AugAssignDesugared, DesugaredOrigin, DesugaredUseFocus, HirOrigin, LazySpan,
    UseDesugared,
};

/// This type represents function from the hir origin to another hir origin to
/// identify the span of HIR node. `LazyTransitionFn` is regarded as a closure
/// that takes a `HirOrigin` and [`LazyArg`], `LazyArg` is considered as
/// captured variables.
/// The reason why we use `LazyTransitionFn` instead of `dyn
/// Fn` is that we want to make all types that use `LazyTransitionFn` to be
/// `Clone` and `Eq`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) struct LazyTransitionFn {
    pub(super) f: fn(ResolvedOrigin, LazyArg) -> ResolvedOrigin,
    pub(super) arg: LazyArg,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) enum LazyArg {
    Idx(usize),
    None,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) struct SpanTransitionChain {
    pub(crate) root: ChainRoot,
    pub(super) chain: Vec<LazyTransitionFn>,
}

impl SpanTransitionChain {
    pub(super) fn new(root: impl Into<ChainRoot>) -> Self {
        Self {
            root: root.into(),
            chain: Vec::new(),
        }
    }

    pub(super) fn push(&mut self, transition: LazyTransitionFn) {
        self.chain.push(transition);
    }

    pub(crate) fn pop_transition(&mut self) {
        self.chain.pop();
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, derive_more::From)]
pub(crate) enum ChainRoot {
    ItemKind(ItemKind),
    TopMod(TopLevelMod),
    Mod(Mod),
    Func(Func),
    Struct(Struct),
    Contract(Contract),
    Enum(Enum),
    TypeAlias(TypeAlias),
    Impl(Impl),
    Trait(Trait),
    ImplTrait(ImplTrait),
    Const(Const),
    Use(Use),
    Body(Body),
    Stmt(StmtRoot),
    Expr(ExprRoot),
    Pat(PatRoot),
}

pub(crate) struct ResolvedOrigin {
    pub(crate) file: InputFile,
    pub(crate) kind: ResolvedOriginKind,
}

impl ResolvedOrigin {
    pub(crate) fn new(file: InputFile, kind: ResolvedOriginKind) -> Self {
        Self { file, kind }
    }

    pub(crate) fn resolve<T>(
        db: &dyn SpannedHirDb,
        top_mod: TopLevelMod,
        origin: &HirOrigin<T>,
    ) -> ResolvedOrigin
    where
        T: AstNode<Language = FeLang>,
    {
        let root = top_mod_ast(db.as_hir_db(), top_mod).syntax().clone();
        let kind = match origin {
            HirOrigin::Raw(ptr) => ResolvedOriginKind::Node(ptr.syntax_node_ptr().to_node(&root)),
            HirOrigin::Expanded(ptr) => ResolvedOriginKind::Expanded(ptr.to_node(&root)),
            HirOrigin::Desugared(desugared) => {
                ResolvedOriginKind::Desugared(root, desugared.clone())
            }
            HirOrigin::None => ResolvedOriginKind::None,
        };

        ResolvedOrigin::new(top_mod.file(db.as_hir_db()), kind)
    }

    pub(crate) fn map<F>(self, f: F) -> Self
    where
        F: FnOnce(SyntaxNode) -> Option<NodeOrToken>,
    {
        let kind = match self.kind {
            ResolvedOriginKind::Node(node) => match f(node) {
                Some(NodeOrToken::Node(node)) => ResolvedOriginKind::Node(node),
                Some(NodeOrToken::Token(token)) => ResolvedOriginKind::Token(token),
                None => ResolvedOriginKind::None,
            },
            kind => kind,
        };

        ResolvedOrigin {
            file: self.file,
            kind,
        }
    }

    pub(crate) fn map_desugared<F>(self, f: F) -> Self
    where
        F: FnOnce(SyntaxNode, DesugaredOrigin) -> ResolvedOriginKind,
    {
        let kind = match self.kind {
            ResolvedOriginKind::Desugared(root, desugared) => f(root, desugared),
            kind => kind,
        };

        ResolvedOrigin {
            file: self.file,
            kind,
        }
    }
}

pub(crate) enum ResolvedOriginKind {
    Node(SyntaxNode),
    Token(SyntaxToken),
    Expanded(SyntaxNode),
    Desugared(SyntaxNode, DesugaredOrigin),
    None,
}

impl ChainInitiator for ChainRoot {
    fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin {
        match self {
            Self::ItemKind(kind) => match kind {
                ItemKind::TopMod(top_mod) => top_mod.init(db),
                ItemKind::Mod(mod_) => mod_.init(db),
                ItemKind::Func(func) => func.init(db),
                ItemKind::Struct(struct_) => struct_.init(db),
                ItemKind::Contract(contract) => contract.init(db),
                ItemKind::Enum(enum_) => enum_.init(db),
                ItemKind::TypeAlias(type_alias) => type_alias.init(db),
                ItemKind::Impl(impl_) => impl_.init(db),
                ItemKind::Trait(trait_) => trait_.init(db),
                ItemKind::ImplTrait(impl_trait) => impl_trait.init(db),
                ItemKind::Const(const_) => const_.init(db),
                ItemKind::Use(use_) => use_.init(db),
                ItemKind::Body(body) => body.init(db),
            },
            Self::TopMod(top_mod) => top_mod.init(db),
            Self::Mod(mod_) => mod_.init(db),
            Self::Func(func) => func.init(db),
            Self::Struct(struct_) => struct_.init(db),
            Self::Contract(contract) => contract.init(db),
            Self::Enum(enum_) => enum_.init(db),
            Self::TypeAlias(type_alias) => type_alias.init(db),
            Self::Impl(impl_) => impl_.init(db),
            Self::Trait(trait_) => trait_.init(db),
            Self::ImplTrait(impl_trait) => impl_trait.init(db),
            Self::Const(const_) => const_.init(db),
            Self::Use(use_) => use_.init(db),
            Self::Body(body) => body.init(db),
            Self::Stmt(stmt) => stmt.init(db),
            Self::Expr(expr) => expr.init(db),
            Self::Pat(pat) => pat.init(db),
        }
    }
}

impl LazySpan for SpanTransitionChain {
    fn resolve(&self, db: &dyn crate::SpannedHirDb) -> Option<Span> {
        let mut resolved = self.root.init(db);

        for LazyTransitionFn { f, arg } in &self.chain {
            resolved = f(resolved, *arg);
        }

        Some(match resolved.kind {
            ResolvedOriginKind::Node(node) => {
                Span::new(resolved.file, node.text_range(), SpanKind::Original)
            }
            ResolvedOriginKind::Token(token) => {
                Span::new(resolved.file, token.text_range(), SpanKind::Original)
            }
            ResolvedOriginKind::Expanded(node) => {
                Span::new(resolved.file, node.text_range(), SpanKind::Expanded)
            }
            ResolvedOriginKind::Desugared(root, desugared) => {
                desugared.resolve(db, root, resolved.file)
            }
            ResolvedOriginKind::None => return None,
        })
    }
}

/// A trait for types that can be used as the root of a `SpanTransitionChain`.
pub(crate) trait ChainInitiator {
    /// Returns the `ResolvedOrigin` for the root of the chain.
    fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin;
}

impl ChainInitiator for TopLevelMod {
    fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin {
        let file = self.file(db.as_hir_db());
        let ast = top_mod_ast(db.as_hir_db(), *self);
        ResolvedOrigin::new(file, ResolvedOriginKind::Node(ast.syntax().clone()))
    }
}

macro_rules! impl_chain_root {
    ($(($ty:ty, $fn:ident),)*) => {
        $(
        impl ChainInitiator for $ty {
            fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin {
                let top_mod = self.top_mod(db.as_hir_db());
                let origin = $fn(db, *self);
                ResolvedOrigin::resolve(db, top_mod, origin)
            }
        })*
    };
}

impl_chain_root! {
    (Mod, mod_ast),
    (Func, func_ast),
    (Struct, struct_ast),
    (Contract, contract_ast),
    (Enum, enum_ast),
    (TypeAlias, type_alias_ast),
    (Impl, impl_ast),
    (Trait, trait_ast),
    (ImplTrait, impl_trait_ast),
    (Const, const_ast),
    (Use, use_ast),
    (Body, body_ast),
}

macro_rules! define_lazy_span_node {
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
        #[derive(Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $name(pub(crate) crate::span::transition::SpanTransitionChain);
        $(
            $(
            impl $name {

            $(pub fn new(hir: $hir_ty) -> Self {
                Self(crate::span::transition::SpanTransitionChain::new(hir))
            })?

            $($(
                pub fn $name_token(&self) -> crate::span::LazySpanAtom {
                    let cloned = self.clone();
                    paste::paste! {
                        cloned.[<$name_token _moved>]()
                    }
                }

                paste::paste! {
                    pub fn [<$name_token _moved>](mut self) -> crate::span::LazySpanAtom {
                        use parser::ast::prelude::*;
                        fn f(origin: crate::span::transition::ResolvedOrigin, _: crate::span::transition::LazyArg) -> crate::span::transition::ResolvedOrigin {
                            origin.map(|node| <$sk_node as AstNode>::cast(node)
                                .and_then(|n| n.$getter_token())
                                .map(|n| n.into()))
                        }

                        let lazy_transition = crate::span::transition::LazyTransitionFn {
                            f,
                            arg: crate::span::transition::LazyArg::None,
                        };

                        self.0.push(lazy_transition);
                        crate::span::LazySpanAtom(self.0)
                    }
                }
            )*)?

            $($(
                pub fn $name_node(&self) -> $result {
                    let cloned = self.clone();
                    paste::paste! {
                        cloned.[<$name_node _moved>]()
                    }
                }

                paste::paste! {
                        pub fn [<$name_node _moved>](mut self) -> $result {
                        use parser::ast::prelude::*;

                        fn f(origin: crate::span::transition::ResolvedOrigin, _: crate::span::transition::LazyArg) -> crate::span::transition::ResolvedOrigin {
                            origin.map(|node| <$sk_node as AstNode>::cast(node)
                                .and_then(|n| n.$getter_node())
                                .map(|n| n.syntax().clone().into()))
                        }

                        let lazy_transition = crate::span::transition::LazyTransitionFn {
                            f,
                            arg: crate::span::transition::LazyArg::None,
                        };
                        self.0.push(lazy_transition);
                        $result(self.0)
                    }
                }
            )*)?

            $($(

                pub fn $name_iter(&self, idx: usize) -> $result_iter {
                    let cloned = self.clone();
                    paste::paste! {
                        cloned.[<$name_iter _moved>](idx)
                    }
                }

                paste::paste! {
                    pub fn [<$name_iter _moved>](mut self, idx: usize) -> $result_iter {
                        use parser::ast::prelude::*;
                        fn f(origin: crate::span::transition::ResolvedOrigin, arg: crate::span::transition::LazyArg) -> crate::span::transition::ResolvedOrigin {
                            let idx = match arg {
                                crate::span::transition::LazyArg::Idx(idx) => idx,
                                _ => unreachable!(),
                            };

                            origin.map(|node| <$sk_node as AstNode>::cast(node)
                                .and_then(|f| f.into_iter().nth(idx))
                                .map(|n| n.syntax().clone().into()))
                        }

                        let lazy_transition = crate::span::transition::LazyTransitionFn {
                            f,
                            arg: crate::span::transition::LazyArg::Idx(idx),
                        };

                        self.0.push(lazy_transition);
                        $result_iter(self.0)
                    }
                }
            )*)?
        })?)?


        impl crate::span::LazySpan for $name {
            fn resolve(&self, db: &dyn crate::SpannedHirDb) -> Option<common::diagnostics::Span> {
                self.0.resolve(db)
            }
        }

        impl From<$name> for crate::span::DynLazySpan {
            fn from(val: $name) -> Self {
                Self(val.0.into())
            }
        }

        impl crate::span::SpanDowncast for $name  {
            fn downcast(val: crate::span::DynLazySpan) -> Option<Self> {
                val.0.map(|inner| Self(inner))
            }
        }
    };
}

impl DesugaredOrigin {
    fn resolve(self, _db: &dyn SpannedHirDb, root: SyntaxNode, file: InputFile) -> Span {
        let range = match self {
            Self::AugAssign(AugAssignDesugared::Stmt(ptr)) => {
                ptr.syntax_node_ptr().to_node(&root).text_range()
            }
            Self::AugAssign(AugAssignDesugared::Lhs(range)) => range,
            Self::AugAssign(AugAssignDesugared::Rhs(ptr)) => {
                ptr.syntax_node_ptr().to_node(&root).text_range()
            }

            Self::Use(UseDesugared {
                root: use_root,
                path,
                alias,
                focus,
            }) => match focus {
                DesugaredUseFocus::Root => use_root.syntax_node_ptr().to_node(&root).text_range(),
                DesugaredUseFocus::Path => {
                    if let Some(first_seg) = path.first() {
                        let last_seg = path.last().unwrap();
                        TextRange::new(
                            first_seg
                                .syntax_node_ptr()
                                .to_node(&root)
                                .text_range()
                                .start(),
                            last_seg.syntax_node_ptr().to_node(&root).text_range().end(),
                        )
                    } else {
                        return Span::new(
                            file,
                            TextRange::new(0.into(), 0.into()),
                            SpanKind::NotFound,
                        );
                    }
                }
                DesugaredUseFocus::Alias => {
                    if let Some(alias) = alias {
                        alias.syntax_node_ptr().to_node(&root).text_range()
                    } else {
                        return Span::new(
                            file,
                            TextRange::new(0.into(), 0.into()),
                            SpanKind::NotFound,
                        );
                    }
                }
            },
        };

        Span::new(file, range, SpanKind::Original)
    }
}

pub(super) use define_lazy_span_node;
