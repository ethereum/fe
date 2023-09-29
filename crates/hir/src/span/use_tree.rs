use parser::ast::{self, prelude::*};

use crate::span::{
    transition::{LazyArg, LazyTransitionFn, ResolvedOrigin, ResolvedOriginKind},
    DesugaredOrigin,
};

use super::{define_lazy_span_node, LazySpanAtom};

define_lazy_span_node!(LazyUsePathSpan);
impl LazyUsePathSpan {
    pub fn segment(&self, idx: usize) -> LazyUsePathSegmentSpan {
        self.clone().segment_moved(idx)
    }

    pub fn segment_moved(mut self, idx: usize) -> LazyUsePathSegmentSpan {
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
                })
        }

        let lazy_transition = LazyTransitionFn {
            f,
            arg: LazyArg::Idx(idx),
        };

        self.0.push(lazy_transition);
        LazyUsePathSegmentSpan(self.0)
    }
}

define_lazy_span_node!(LazyUsePathSegmentSpan);
impl LazyUsePathSegmentSpan {
    pub fn into_atom(self) -> LazySpanAtom {
        LazySpanAtom(self.0)
    }
}

define_lazy_span_node!(LazyUseAliasSpan, ast::UseAlias,);

impl LazyUseAliasSpan {
    pub fn name(&self) -> LazySpanAtom {
        self.clone().name_moved()
    }

    pub fn name_moved(mut self) -> LazySpanAtom {
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
                })
        }

        let lazy_transition = LazyTransitionFn {
            f,
            arg: LazyArg::None,
        };
        self.0.push(lazy_transition);

        LazySpanAtom(self.0)
    }
}
