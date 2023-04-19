use common::{
    diagnostics::{Span, SpanKind},
    InputFile,
};
use parser::{
    ast::prelude::*, syntax_node::NodeOrToken, FeLang, SyntaxNode, SyntaxToken, TextRange,
};

use crate::{
    hir_def::{
        Body, Const, Contract, Enum, ExternFunc, Func, Impl, ImplTrait, Mod, Struct, TopLevelMod,
        Trait, TypeAlias, Use,
    },
    lower::{map_file_to_mod_impl, top_mod_ast},
    SpannedHirDb,
};

use super::{
    body_ast, const_ast, contract_ast, enum_ast, expr::ExprRoot, extern_func_ast, func_ast,
    impl_ast, impl_trait_ast, mod_ast, pat::PatRoot, stmt::StmtRoot, struct_ast, trait_ast,
    type_alias_ast, use_ast, AugAssignDesugared, DesugaredOrigin, HirOrigin, LazySpan,
};

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
    root: ChainRoot,
    chain: Vec<LazyTransitionFn>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, derive_more::From)]
pub(crate) enum ChainRoot {
    TopMod(TopLevelMod),
    Mod(Mod),
    Func(Func),
    ExternFunc(ExternFunc),
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
        let root = top_mod_ast(db.upcast(), top_mod).syntax().clone();
        let kind = match origin {
            HirOrigin::Raw(ptr) => ResolvedOriginKind::Node(ptr.syntax_node_ptr().to_node(&root)),
            HirOrigin::Expanded(ptr) => ResolvedOriginKind::Expanded(ptr.to_node(&root)),
            HirOrigin::Desugared(desugared) => ResolvedOriginKind::Desugared(desugared.clone()),
            HirOrigin::None => ResolvedOriginKind::None,
        };

        ResolvedOrigin::new(top_mod.file(db.upcast()), kind)
    }
}

pub(crate) enum ResolvedOriginKind {
    Node(SyntaxNode),
    Token(SyntaxToken),
    Expanded(SyntaxNode),
    Desugared(DesugaredOrigin),
    None,
}

impl ResolvedOrigin {
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
}

impl ChainInitiator for ChainRoot {
    fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin {
        match self {
            Self::TopMod(top_mod) => top_mod.init(db),
            Self::Mod(mod_) => mod_.init(db),
            Self::Func(func) => func.init(db),
            Self::ExternFunc(extern_func) => extern_func.init(db),
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

impl SpanTransitionChain {
    pub(super) fn new(root: impl Into<ChainRoot>) -> Self {
        Self {
            root: root.into(),
            chain: Vec::new(),
        }
    }

    pub(super) fn push_transition(&self, transition: LazyTransitionFn) -> Self {
        let mut new_state = self.clone();
        new_state.chain.push(transition);
        new_state
    }
}

impl LazySpan for SpanTransitionChain {
    fn resolve(&self, db: &dyn crate::SpannedHirDb) -> Span {
        let mut resolved = self.root.init(db);

        for LazyTransitionFn { f, arg } in &self.chain {
            resolved = f(resolved, *arg);
        }

        match resolved.kind {
            ResolvedOriginKind::Node(node) => {
                Span::new(resolved.file, node.text_range(), SpanKind::Original)
            }
            ResolvedOriginKind::Token(token) => {
                Span::new(resolved.file, token.text_range(), SpanKind::Original)
            }
            ResolvedOriginKind::Expanded(node) => {
                Span::new(resolved.file, node.text_range(), SpanKind::Expanded)
            }
            ResolvedOriginKind::Desugared(desugared) => desugared.resolve(db, resolved.file),
            ResolvedOriginKind::None => Span::new(
                resolved.file,
                TextRange::new(0.into(), 0.into()),
                SpanKind::NotFound,
            ),
        }
    }
}

/// A trait for types that can be used as the root of a `SpanTransitionChain`.
pub(crate) trait ChainInitiator {
    /// Returns the `ResolvedOrigin` for the root of the chain.
    fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin;
}

impl ChainInitiator for TopLevelMod {
    fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin {
        let file = self.file(db.upcast());
        let ast = top_mod_ast(db.upcast(), *self);
        ResolvedOrigin::new(file, ResolvedOriginKind::Node(ast.syntax().clone()))
    }
}

macro_rules! impl_chain_root {
    ($(($ty:ty, $fn:ident),)*) => {
        $(
        impl ChainInitiator for $ty {
            fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin {
                let top_mod = self.top_mod(db.upcast());
                let origin = $fn(db, *self);
                ResolvedOrigin::resolve(db, top_mod, origin)
            }
        })*
    };
}

impl_chain_root! {
    (Mod, mod_ast),
    (Func, func_ast),
    (ExternFunc, extern_func_ast),
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
                    crate::span::LazySpanAtom(
                        self.0.push_transition(lazy_transition)
                    )
                }
            )*)?

            $($(
                pub fn $name_node(&self) -> $result {
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
                    $result(self.0.push_transition(lazy_transition))
                }
            )*)?

            $($(

                pub fn $name_iter(&self, idx: usize) -> $result_iter {
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

                    $result_iter(self.0.push_transition(lazy_transition))
                }
            )*)?
        })?)?


        impl crate::span::LazySpan for $name {
            fn resolve(&self, db: &dyn crate::SpannedHirDb) -> common::diagnostics::Span {
                self.0.resolve(db)
            }
        }
    };
}

impl DesugaredOrigin {
    fn resolve(self, db: &dyn SpannedHirDb, file: InputFile) -> Span {
        let range = match self {
            Self::AugAssign(AugAssignDesugared::Stmt(ptr)) => {
                let top_mod = map_file_to_mod_impl(db.upcast(), file);
                let top_mod_ast = top_mod_ast(db.upcast(), top_mod);
                ptr.syntax_node_ptr()
                    .to_node(top_mod_ast.syntax())
                    .text_range()
            }

            Self::AugAssign(AugAssignDesugared::Lhs(range)) => range,

            Self::AugAssign(AugAssignDesugared::Rhs(ptr)) => {
                let top_mod = map_file_to_mod_impl(db.upcast(), file);
                let top_mod_ast = top_mod_ast(db.upcast(), top_mod);
                ptr.syntax_node_ptr()
                    .to_node(top_mod_ast.syntax())
                    .text_range()
            }
        };

        Span::new(file, range, SpanKind::Original)
    }
}

pub(super) use define_lazy_span_node;
