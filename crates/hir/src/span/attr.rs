use parser::ast;

use super::define_lazy_span_node;

define_lazy_span_node!(
    LazyAttrListSpan,
    ast::AttrList,
    @idx {
        (attr, LazyAttrSpan),
    }
);
impl LazyAttrListSpan {}

define_lazy_span_node!(LazyAttrSpan);
impl LazyAttrSpan {
    pub fn into_normal_attr(&self) -> LazyNormalAttrSpan {
        self.clone().into_normal_attr_moved()
    }

    pub fn into_normal_attr_moved(self) -> LazyNormalAttrSpan {
        LazyNormalAttrSpan(self.0)
    }

    pub fn into_doc_comment_attr(&self) -> LazyDocCommentAttrSpan {
        self.clone().into_doc_comment_attr_moved()
    }

    pub fn into_doc_comment_attr_moved(self) -> LazyDocCommentAttrSpan {
        LazyDocCommentAttrSpan(self.0)
    }
}

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
    LazyDocCommentAttrSpan,
    ast::DocCommentAttr,
    @token {
        (doc, doc),
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
