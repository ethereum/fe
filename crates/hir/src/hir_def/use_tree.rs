use crate::{hir_def::Partial, HirDb};

use super::IdentId;

#[salsa::interned]
pub struct UsePathId {
    #[return_ref]
    pub data: Vec<Partial<UsePathSegment>>,
}

impl UsePathId {
    pub fn is_glob(&self, db: &dyn HirDb) -> bool {
        self.data(db)
            .last()
            .and_then(|seg| seg.to_opt())
            .map_or(false, |seg| seg.is_glob())
    }

    pub fn last_ident(&self, db: &dyn HirDb) -> Option<IdentId> {
        self.data(db)
            .last()
            .and_then(|seg| seg.to_opt())
            .and_then(|seg| seg.ident())
    }

    pub fn segment_len(&self, db: &dyn HirDb) -> usize {
        self.data(db).len()
    }

    pub fn pretty_path(&self, db: &dyn HirDb) -> String {
        let mut path = String::new();

        for (i, seg) in self.data(db).iter().enumerate() {
            if i != 0 {
                path.push_str("::");
            }
            match seg {
                Partial::Absent => path.push_str("{invalid}"),
                Partial::Present(seg) => match seg {
                    UsePathSegment::Ident(ident) => path.push_str(ident.data(db)),
                    UsePathSegment::Glob => path.push('*'),
                },
            }
        }
        path
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UsePathSegment {
    Ident(IdentId),
    /// `*`.
    Glob,
}

impl UsePathSegment {
    /// Returns the ident of the last path segment.
    /// If the last segment is a glob, returns `None`.
    pub fn ident(self) -> Option<IdentId> {
        match self {
            UsePathSegment::Ident(ident) => Some(ident),
            UsePathSegment::Glob => None,
        }
    }

    pub fn is_glob(self) -> bool {
        match self {
            UsePathSegment::Ident(_) => false,
            UsePathSegment::Glob => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UseAlias {
    Ident(IdentId),
    Underscore,
}
