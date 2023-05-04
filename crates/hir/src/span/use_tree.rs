use parser::ast::{self, prelude::*};

use crate::span::{
    transition::{LazyArg, LazyTransitionFn, ResolvedOrigin, ResolvedOriginKind},
    DesugaredOrigin,
};

use super::{define_lazy_span_node, LazySpanAtom};

define_lazy_span_node!(LazyUsePathSpan);
impl LazyUsePathSpan {
    pub fn segment(&self, idx: usize) -> LazyUsePathSegmentSpan {
        fn f(origin: ResolvedOrigin, arg: LazyArg) -> ResolvedOrigin {
            let LazyArg::Idx(idx) = arg else {
                unreachable!()
            };

            origin
                .map(|node| {
                    ast::UsePath::cast(node)
                        .and_then(|f| f.into_iter().nth(idx))
                        .map(|n| n.syntax().clone().into())
                })
                .map_desugared(|root, desugared| match desugared {
                    DesugaredOrigin::Use(use_) => use_
                        .path
                        .get(idx)
                        .map(|ptr| ResolvedOriginKind::Node(ptr.syntax_node_ptr().to_node(&root)))
                        .unwrap_or_else(|| ResolvedOriginKind::None),
                    _ => ResolvedOriginKind::None,
                })
        }

        let lazy_transition = LazyTransitionFn {
            f,
            arg: LazyArg::Idx(idx),
        };

        LazyUsePathSegmentSpan(self.0.push_transition(lazy_transition))
    }
}

define_lazy_span_node!(LazyUsePathSegmentSpan);

define_lazy_span_node!(LazyUseAliasSpan, ast::UseAlias,);

impl LazyUseAliasSpan {
    pub fn name(&self) -> LazySpanAtom {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin
                .map(|node| {
                    ast::UseAlias::cast(node)
                        .and_then(|a| a.alias())
                        .map(|n| n.into())
                })
                .map_desugared(|root, desugared| match desugared {
                    DesugaredOrigin::Use(use_) => use_
                        .alias
                        .and_then(|ptr| ptr.to_node(&root).alias().map(ResolvedOriginKind::Token))
                        .unwrap_or_else(|| ResolvedOriginKind::None),
                    _ => ResolvedOriginKind::None,
                })
        }

        let lazy_transition = LazyTransitionFn {
            f,
            arg: LazyArg::None,
        };

        LazySpanAtom(self.0.push_transition(lazy_transition))
    }
}
