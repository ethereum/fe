use super::{GenericArgListId, IdentId};
use crate::{hir_def::Partial, HirDb};

#[salsa::interned]
pub struct PathId<'db> {
    #[return_ref]
    pub segments: Vec<PathSegmentId<'db>>,
}

impl<'db> PathId<'db> {
    pub fn last_segment(self, db: &'db dyn HirDb) -> Option<PathSegmentId> {
        self.segments(db).last().copied()
    }

    pub fn len(self, db: &dyn HirDb) -> usize {
        self.segments(db).len()
    }

    pub fn as_ident(self, db: &'db dyn HirDb) -> Option<IdentId> {
        if self.len(db) == 1 {
            self.last_segment(db).unwrap().ident(db).to_opt()
        } else {
            None
        }
    }

    pub fn self_ty(db: &'db dyn HirDb) -> Self {
        Self::from_ident(db, IdentId::make_self_ty(db))
    }

    pub fn from_ident(db: &'db dyn HirDb, ident: IdentId<'db>) -> Self {
        Self::new(db, vec![PathSegmentId::from_ident(db, ident)])
    }

    pub fn push(self, db: &'db dyn HirDb, segment: PathSegmentId<'db>) -> Self {
        let mut segments = self.segments(db).clone();
        segments.push(segment);
        Self::new(db, segments)
    }

    pub fn pretty_print(self, db: &dyn HirDb) -> String {
        self.segments(db)
            .iter()
            .map(|seg| {
                seg.ident(db)
                    .to_opt()
                    .map_or("_".to_string(), |id| id.data(db).clone())
                // xxx print generic args
            })
            .collect::<Vec<_>>()
            .join("::")
    }
}

#[salsa::interned]
pub struct PathSegmentId<'db> {
    pub ident: Partial<IdentId<'db>>,
    pub generic_args: GenericArgListId<'db>,
}

impl<'db> PathSegmentId<'db> {
    pub fn from_ident(db: &'db dyn HirDb, ident: IdentId<'db>) -> Self {
        Self::new(db, Partial::Present(ident), GenericArgListId::none(db))
    }
}
