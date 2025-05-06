use super::{GenericArgListId, IdentId};
use crate::{hir_def::Partial, HirDb};

#[salsa::interned]
#[derive(Debug)]
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

    pub fn self_ty(db: &'db dyn HirDb, args: GenericArgListId<'db>) -> Self {
        Self::new(db, Partial::Present(IdentId::make_self_ty(db)), args, None)
    }

    pub fn len(self, db: &dyn HirDb) -> usize {
        if let Some(parent) = self.parent(db) {
            parent.len(db) + 1
        } else {
            1
        }
    }

    pub fn segment_index(self, db: &dyn HirDb) -> usize {
        self.len(db) - 1
    }

    pub fn segment(self, db: &'db dyn HirDb, idx: usize) -> Option<PathId<'db>> {
        if idx == self.segment_index(db) {
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

    pub fn as_ident(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        if self.parent(db).is_none() && self.generic_args(db).is_empty(db) {
            self.ident(db).to_opt()
        } else {
            None
        }
    }

    pub fn is_bare_ident(self, db: &dyn HirDb) -> bool {
        self.parent(db).is_none()
            && self.ident(db).is_present()
            && self.generic_args(db).is_empty(db)
    }

    pub fn is_self_ty(self, db: &dyn HirDb) -> bool {
        if self.parent(db).is_none() && self.ident(db).is_present() {
            self.ident(db).unwrap().is_self_ty(db)
        } else {
            false
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
        let this = format!(
            "{}{}",
            self.ident(db).to_opt().map_or("_", |id| id.data(db)),
            self.generic_args(db).pretty_print(db)
        );

        if let Some(parent) = self.parent(db) {
            parent.pretty_print(db) + "::" + &this
        } else {
            this
        }
    }
}
