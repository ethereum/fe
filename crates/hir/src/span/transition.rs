use std::sync::Arc;

use common::{diagnostics::Span, InputFile};
use parser::{syntax_node::NodeOrToken, SyntaxNode};
use smallvec::SmallVec;

use crate::{
    hir_def::{
        Body, Const, Contract, Enum, ExternFunc, Func, Impl, ImplTrait, Mod, Struct, TopLevelMod,
        Trait, TypeAlias, Use,
    },
    parse_file,
};

use super::{db::SpannedHirDb, LazySpan};

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
    fn span(&self, db: &dyn SpannedHirDb) -> Span {
        let (file, mut node) = self.root.root(db);

        for transition in &self.chain {
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

pub(super) trait ChainRoot {
    fn root(&self, db: &dyn SpannedHirDb) -> (InputFile, SyntaxNode);
}

macro_rules! impl_chain_root {
    ($(($ty:ty, $fn:ident),)*) => {
        $(
        impl ChainRoot for $ty {
            fn root(&self, db: &dyn SpannedHirDb) -> (InputFile, SyntaxNode) {
                let ast = db.$fn(*self);
                let file = ast.file;
                let ptr = ast.syntax_ptr().unwrap();
                let root_node = SyntaxNode::new_root(parse_file(db.upcast(), file));
                let node = ptr.to_node(&root_node);
                (file, node)
            }
        })*
    };
}
impl_chain_root! {
    (TopLevelMod, toplevel_ast),
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
        pub struct $name(pub(crate) crate::span::transition::SpanTransitionChain);
        $(
            $(
            impl $name {

            $(pub fn new(hir: $hir_ty) -> Self {
                Self(crate::span::transition::SpanTransitionChain::new(hir))
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
                        self.0.push_transition(std::sync::Arc::new(transition))
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
            fn span(&self, db: &dyn crate::span::SpannedHirDb) -> common::diagnostics::Span {
                self.0.span(db)
            }
        }
    };
}

pub(super) use define_lazy_span_item;
