use super::{IdentId, Partial, StringId};

#[salsa::interned]
pub struct AttrListId {
    #[return_ref]
    attrs: Vec<Attr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum Attr {
    Normal(NormalAttr),
    DocComment(DocCommentAttr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NormalAttr {
    pub name: Partial<IdentId>,
    pub args: Vec<AttrArg>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DocCommentAttr {
    /// This is the text of the doc comment, excluding the `///` prefix.
    pub text: StringId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttrArg {
    pub key: Partial<IdentId>,
    pub value: Partial<IdentId>,
}
