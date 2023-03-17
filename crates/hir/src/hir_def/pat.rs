use cranelift_entity::entity_impl;

use super::{IdentId, LitKind, PathId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    WildCard,
    Rest,
    Lit(LitKind),
    Tuple(Vec<PatId>),
    Path(PathId),
    PathTuple(PathId, Vec<PatId>),
    Record(PathId, Vec<RecordPatField>),
    Or(PatId, PatId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PatId(u32);
entity_impl!(PatId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordPatField {
    pub label: Option<IdentId>,
    pub pat: PatId,
}
