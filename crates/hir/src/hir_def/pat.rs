use cranelift_entity::entity_impl;

use super::{Body, IdentId, LitKind, Partial, PathId};
use crate::{span::pat::LazyPatSpan, HirDb};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    WildCard,
    Rest,
    Lit(Partial<LitKind>),
    Tuple(Vec<PatId>),
    /// The second bool is `true` if the pat has `mut` in front of it.
    Path(Partial<PathId>, bool),
    PathTuple(Partial<PathId>, Vec<PatId>),
    Record(Partial<PathId>, Vec<RecordPatField>),
    Or(PatId, PatId),
}

impl Pat {
    /// Return `true` if this pattern is a binding.
    pub fn is_bind(&self, db: &dyn HirDb) -> bool {
        match self {
            Self::Path(Partial::Present(p), _) => p.len(db) == 1,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PatId(u32);
entity_impl!(PatId);

impl PatId {
    pub fn lazy_span(self, body: Body) -> LazyPatSpan {
        LazyPatSpan::new(body, self)
    }

    pub fn data(self, db: &dyn HirDb, body: Body) -> &Partial<Pat> {
        &body.pats(db)[self]
    }

    /// Return `true` if this pattern is a binding.
    pub fn is_bind(self, db: &dyn HirDb, body: Body) -> bool {
        match self.data(db, body) {
            Partial::Present(p) => p.is_bind(db),
            Partial::Absent => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordPatField {
    pub label: Partial<IdentId>,
    pub pat: PatId,
}

impl RecordPatField {
    pub fn label(&self, db: &dyn HirDb, body: Body) -> Option<IdentId> {
        if let Partial::Present(label) = self.label {
            return Some(label);
        }

        match self.pat.data(db, body) {
            Partial::Present(Pat::Path(Partial::Present(path), _)) if path.is_ident(db) => {
                path.last_segment(db).to_opt()
            }
            _ => None,
        }
    }
}
