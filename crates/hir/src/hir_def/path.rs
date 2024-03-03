use super::{kw, IdentId};
use crate::{hir_def::Partial, HirDb};

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

    pub fn is_ident(self, db: &dyn HirDb) -> bool {
        self.len(db) == 1
    }

    pub fn self_ty(db: &dyn HirDb) -> Self {
        let self_ty = Partial::Present(kw::SELF_TY);
        Self::new(db, vec![self_ty])
    }

    pub fn from_ident(db: &dyn HirDb, ident: IdentId) -> Self {
        Self::new(db, vec![Partial::Present(ident)])
    }

    pub fn push(self, db: &dyn HirDb, segment: IdentId) -> Self {
        let mut segments = self.segments(db).clone();
        segments.push(Partial::Present(segment));
        Self::new(db, segments)
    }

    pub fn pretty_print(self, db: &dyn HirDb) -> String {
        self.segments(db)
            .iter()
            .map(|seg| {
                seg.to_opt()
                    .map_or("_".to_string(), |id| id.data(db).clone())
            })
            .collect::<Vec<_>>()
            .join("::")
    }
}
