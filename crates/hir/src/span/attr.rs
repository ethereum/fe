use parser::ast::{self, prelude::*};

use super::define_lazy_span_node;

define_lazy_span_node!(
    LazyAttrListSpan,
    ast::AttrList,
    @idx {
        (attr, LazyAttrSpan),
    }
);
impl LazyAttrListSpan {
    pub fn normal_attr(&self, idx: usize) -> LazyNormalAttrSpan {
        fn f(
            node: parser::SyntaxNode,
            arg: crate::span::transition::LazyArg,
        ) -> Option<parser::NodeOrToken> {
            let idx = match arg {
                crate::span::transition::LazyArg::Idx(idx) => idx,
                _ => unreachable!(),
            };
            ast::AttrList::cast(node)
                .and_then(|f| f.normal_attrs().nth(idx))
                .map(|n| n.syntax().clone().into())
        }

        let lazy_transition = crate::span::transition::LazyTransitionFn {
            f,
            arg: crate::span::transition::LazyArg::Idx(idx),
        };

        LazyNormalAttrSpan(self.0.push_transition(lazy_transition))
    }
}

define_lazy_span_node!(LazyAttrSpan);
define_lazy_span_node!(
    LazyNormalAttrSpan,
    ast::NormalAttr,
    @token {
        (name, name),
    }
    @node {
        (args, args, LazyAttrArgListSpan),
    }
);

define_lazy_span_node!(
    LazyAttrArgListSpan,
    ast::AttrArgList,
    @idx {
        (arg, LazyAttrArgSpan),
    }
);

define_lazy_span_node!(
    LazyAttrArgSpan,
    ast::AttrArg,
    @token {
        (key, key),
        (value, value),
    }
);
