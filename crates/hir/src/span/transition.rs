use common::{diagnostics::Span, InputFile};
use parser::{ast::prelude::*, syntax_node::NodeOrToken, SyntaxNode};

use crate::{
    hir_def::{
        Body, Const, Contract, Enum, ExternFunc, Func, Impl, ImplTrait, Mod, Struct, TopLevelMod,
        Trait, TypeAlias, Use,
    },
    lower::top_mod_ast,
};

use super::{
    body_ast, const_ast, contract_ast, enum_ast, expr::ExprRoot, extern_func_ast, func_ast,
    impl_ast, impl_trait_ast, mod_ast, pat::PatRoot, stmt::StmtRoot, struct_ast, trait_ast,
    type_alias_ast, use_ast, LazySpan,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) struct LazyTransitionFn {
    pub(super) f: fn(SyntaxNode, LazyArg) -> Option<NodeOrToken>,
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

impl ChainInitiator for ChainRoot {
    fn init(&self, db: &dyn crate::SpannedHirDb) -> (InputFile, SyntaxNode) {
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
        let (file, mut node) = self.root.init(db);

        for LazyTransitionFn { f, arg } in &self.chain {
            node = match f(node.clone(), *arg) {
                Some(NodeOrToken::Node(node)) => node,
                Some(NodeOrToken::Token(token)) => {
                    return Span::new(file, token.text_range());
                }
                None => {
                    break;
                }
            };
        }

        Span::new(file, node.text_range())
    }
}

pub trait ChainInitiator {
    fn init(&self, db: &dyn crate::SpannedHirDb) -> (InputFile, SyntaxNode);
}

impl ChainInitiator for TopLevelMod {
    fn init(&self, db: &dyn crate::SpannedHirDb) -> (InputFile, SyntaxNode) {
        let file = self.file(db.upcast());
        let ast = top_mod_ast(db.upcast(), *self);
        (file, ast.syntax().clone())
    }
}

macro_rules! impl_chain_root {
    ($(($ty:ty, $fn:ident),)*) => {
        $(
        impl ChainInitiator for $ty {
            fn init(&self, db: &dyn crate::SpannedHirDb) -> (InputFile, SyntaxNode) {
                let ast = $fn(db, *self);
                let (file, root) = self.top_mod(db.upcast()).init(db);
                let ptr = ast.syntax_ptr().unwrap();
                let node = ptr.to_node(&root);
                (file, node)
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
                    fn f(node: parser::SyntaxNode, _: crate::span::transition::LazyArg) -> Option<parser::NodeOrToken> {
                        <$sk_node as AstNode>::cast(node)
                            .and_then(|n| n.$getter_token())
                            .map(|n| n.into())
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

                    fn f(node: parser::SyntaxNode, _: crate::span::transition::LazyArg) -> Option<parser::NodeOrToken> {
                        <$sk_node as AstNode>::cast(node)
                            .and_then(|n| n.$getter_node())
                            .map(|n| n.syntax().clone().into())
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
                    fn f(node: parser::SyntaxNode, arg: crate::span::transition::LazyArg) -> Option<parser::NodeOrToken> {
                        let idx = match arg {
                            crate::span::transition::LazyArg::Idx(idx) => idx,
                            _ => unreachable!(),
                        };

                        <$sk_node as AstNode>::cast(node)
                            .and_then(|f| f.into_iter().nth(idx))
                            .map(|n| n.syntax().clone().into())
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

pub(super) use define_lazy_span_node;
