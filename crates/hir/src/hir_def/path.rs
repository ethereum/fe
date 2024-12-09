use super::{GenericArgListId, IdentId};
use crate::{hir_def::Partial, HirDb};

#[salsa::interned]
pub struct PathId<'db> {
    pub ident: Partial<IdentId<'db>>,
    pub generic_args: GenericArgListId<'db>,
    pub parent: Option<PathId<'db>>,
}

impl<'db> PathId<'db> {
    pub fn from_ident(db: &'db dyn HirDb, ident: IdentId<'db>) -> Self {
        Self::new(
            db,
            Partial::Present(ident),
            GenericArgListId::none(db),
            None,
        )
    }

    pub fn self_ty(db: &'db dyn HirDb) -> Self {
        Self::from_ident(db, IdentId::make_self_ty(db))
    }

    pub fn len(self, db: &dyn HirDb) -> usize {
        if let Some(parent) = self.parent(db) {
            parent.len(db) + 1
        } else {
            1
        }
    }

    pub fn segment(self, db: &'db dyn HirDb, idx: usize) -> Option<PathId<'db>> {
        if idx == self.len(db) - 1 {
            Some(self)
        } else {
            self.parent(db).and_then(|p| p.segment(db, idx))
        }
    }

    pub fn root_ident(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        if let Some(parent) = self.parent(db) {
            parent.root_ident(db)
        } else {
            self.ident(db).to_opt()
        }
    }

    // xxx check uses, remove?
    pub fn as_ident(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        if self.parent(db).is_none() && self.generic_args(db).is_empty(db) {
            self.ident(db).to_opt()
        } else {
            None
        }
    }

    pub fn push(
        self,
        db: &'db dyn HirDb,
        ident: Partial<IdentId<'db>>,
        generic_args: GenericArgListId<'db>,
    ) -> Self {
        Self::new(db, ident, generic_args, Some(self))
    }

    pub fn push_ident(self, db: &'db dyn HirDb, ident: IdentId<'db>) -> Self {
        Self::new(
            db,
            Partial::Present(ident),
            GenericArgListId::none(db),
            Some(self),
        )
    }

    pub fn pretty_print(self, db: &dyn HirDb) -> String {
        let ident = self.ident(db).to_opt().map_or("_", |id| id.data(db));
        if let Some(parent) = self.parent(db) {
            parent.pretty_print(db) + "::" + ident
        } else {
            ident.to_string()
        }
    }
}
