use cranelift_entity::entity_impl;

use crate::span::pat::LazyPatSpan;

use super::{Body, IdentId, LitKind, Partial, PathId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    WildCard,
    Rest,
    Lit(Partial<LitKind>),
    Tuple(Vec<PatId>),
    Path(Partial<PathId>),
    PathTuple(Partial<PathId>, Vec<PatId>),
    Record(Partial<PathId>, Vec<RecordPatField>),
    Or(PatId, PatId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PatId(u32);
entity_impl!(PatId);

impl PatId {
    pub fn lazy_span(self, body: Body) -> LazyPatSpan {
        LazyPatSpan::new(self, body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordPatField {
    pub label: Partial<IdentId>,
    pub pat: PatId,
}
