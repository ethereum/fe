use crate::{hir_def::Partial, HirDb};

use super::IdentId;

#[salsa::interned]
pub struct PathId {
    #[return_ref]
    pub segments: Vec<Partial<IdentId>>,
}

impl PathId {
    pub fn last_segment(self, db: &dyn HirDb) -> Partial<IdentId> {
        self.segments(db).last().copied().unwrap_or_default()
    }

    pub fn len(self, db: &dyn HirDb) -> usize {
        self.segments(db).len()
    }
}
