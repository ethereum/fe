use cranelift_entity::entity_impl;

use super::{IdentId, LitKind, Partial, PathId};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordPatField {
    pub label: Partial<IdentId>,
    pub pat: PatId,
}
