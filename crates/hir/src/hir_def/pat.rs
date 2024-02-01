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

    pub fn is_rest(self, db: &dyn HirDb, body: Body) -> bool {
        matches!(self.data(db, body), Partial::Present(Pat::Rest))
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
