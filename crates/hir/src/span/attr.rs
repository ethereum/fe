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
        let transition = move |node: parser::SyntaxNode| {
            ast::AttrList::cast(node)
                .and_then(|f| f.normal_attrs().nth(idx))
                .map(|n| n.syntax().clone().into())
        };
        LazyNormalAttrSpan(self.0.push_transition(std::sync::Arc::new(transition)))
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
