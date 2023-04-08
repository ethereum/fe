use std::sync::Arc;

use common::{diagnostics::Span, InputFile};
use parser::{ast::prelude::*, syntax_node::NodeOrToken, SyntaxNode};
use smallvec::SmallVec;

use crate::{
    hir_def::{
        Body, Const, Contract, Enum, ExternFunc, Func, Impl, ImplTrait, Mod, Struct, TopLevelMod,
        Trait, TypeAlias, Use,
    },
    lower::top_mod_ast,
};

use super::{
    body_ast, const_ast, contract_ast, enum_ast, extern_func_ast, func_ast, impl_ast,
    impl_trait_ast, mod_ast, struct_ast, trait_ast, type_alias_ast, use_ast, LazySpan,
};

type TransitionFn = Arc<dyn Fn(SyntaxNode) -> Option<NodeOrToken>>;

#[derive(Clone)]
pub(crate) struct SpanTransitionChain {
    root: Arc<dyn ChainRoot>,
    chain: SmallVec<[TransitionFn; 4]>,
}

impl SpanTransitionChain {
    pub(super) fn new<T: ChainRoot + 'static>(root: T) -> Self {
        let root = Arc::new(root);
        Self {
            root,
            chain: SmallVec::new(),
        }
    }

    pub(super) fn push_transition(&self, transition: TransitionFn) -> Self {
        let mut new_state = self.clone();
        new_state.chain.push(transition);
        new_state
    }
}

impl LazySpan for SpanTransitionChain {
    fn resolve(&self, db: &dyn crate::SpannedHirDb) -> Span {
        let (file, mut node) = self.root.root(db);

        for transition in &self.chain {
            node = match transition(node.clone()) {
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

pub(super) trait ChainRoot {
    fn root(&self, db: &dyn crate::SpannedHirDb) -> (InputFile, SyntaxNode);
}

impl ChainRoot for TopLevelMod {
    fn root(&self, db: &dyn crate::SpannedHirDb) -> (InputFile, SyntaxNode) {
        let file = self.file(db.upcast());
        let ast = top_mod_ast(db.upcast(), *self);
        (file, ast.syntax().clone())
    }
}

macro_rules! impl_chain_root {
    ($(($ty:ty, $fn:ident),)*) => {
        $(
        impl ChainRoot for $ty {
            fn root(&self, db: &dyn crate::SpannedHirDb) -> (InputFile, SyntaxNode) {
                let ast = $fn(db, *self);
                let (file, root) = self.top_mod(db.upcast()).root(db);
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
        #[derive(Clone)]
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
                    let transition = |node: parser::SyntaxNode| {
                        <$sk_node as AstNode>::cast(node)
                            .and_then(|n| n.$getter_token())
                            .map(|n| n.into())
                    };
                    crate::span::LazySpanAtom(
                        self.0.push_transition(std::sync::Arc::new(transition))
                    )
                }
            )*)?

            $($(
                pub fn $name_node(&self) -> $result {
                    use parser::ast::prelude::*;
                    let transition = |node: parser::SyntaxNode| {
                        <$sk_node as AstNode>::cast(node)
                            .and_then(|f| f.$getter_node())
                            .map(|n| n.syntax().clone().into())
                    };
                    $result(self.0.push_transition(std::sync::Arc::new(transition)))
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
                    $result_iter(self.0.push_transition(std::sync::Arc::new(transition)))
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
