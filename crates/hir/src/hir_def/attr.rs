use super::IdentId;

#[salsa::interned]
pub struct AttrListId {
    #[return_ref]
    attrs: Vec<Attr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Attr {
    Normal(NormalAttr),
    DocComment(DocCommentAttr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NormalAttr {
    pub name: IdentId,
    pub args: Vec<AttrArg>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DocCommentAttr {
    /// This is the text of the doc comment, excluding the `///` prefix.
    pub text: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttrArg {
    key: IdentId,
    value: IdentId,
}
