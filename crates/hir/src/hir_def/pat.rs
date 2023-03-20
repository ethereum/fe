use cranelift_entity::entity_impl;

use super::{IdentId, LitKind, MaybeInvalid, PathId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    WildCard,
    Rest,
    Lit(MaybeInvalid<LitKind>),
    Tuple(Vec<PatId>),
    Path(MaybeInvalid<PathId>),
    PathTuple(MaybeInvalid<PathId>, Vec<PatId>),
    Record(MaybeInvalid<PathId>, Vec<RecordPatField>),
    Or(PatId, PatId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PatId(u32);
entity_impl!(PatId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordPatField {
    pub label: MaybeInvalid<IdentId>,
    pub pat: PatId,
}
